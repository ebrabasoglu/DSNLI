library(shiny)
library(bslib)
library(ggplot2)

KUL_TEAL <- "#116E8A"
NAVY <- "#17324D"
INK <- "#1F2933"
MUTED <- "#667085"
GREEN <- "#2F855A"
AMBER <- "#B7791F"
RED <- "#C53030"

default_loading <- 0.1948

asset_dir <- if (dir.exists("www")) {
  "www"
} else if (dir.exists(file.path("dashboard", "www"))) {
  file.path("dashboard", "www")
} else {
  "www"
}

if (dir.exists(asset_dir)) {
  addResourcePath("dash-assets", normalizePath(asset_dir, winslash = "/", mustWork = FALSE))
}

kpis <- list(
  policies = 163657,
  zero_claim_rate = 0.89,
  predicted_total = 6.14e6,
  observed_total = 8.19e6,
  po_ratio = 0.75,
  gini_glm = 0.117,
  gini_gbm = 0.135,
  geo_classes = 5,
  tv_ar_loading = default_loading
)

load_assignment_data <- function() {
  paths <- c(
    "assignment_data.csv",
    file.path("..", "assignment_data.csv"),
    file.path("data", "assignment_data.csv")
  )
  path <- paths[file.exists(paths)][1]

  if (is.na(path)) return(NULL)

  data <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )

  if (is.null(data)) return(NULL)

  attr(data, "source_path") <- path
  data
}

assignment_data <- load_assignment_data()

portfolio <- list(
  policies = kpis$policies,
  zero_claim_rate = kpis$zero_claim_rate,
  exposure = NA_real_,
  total_claim_cost = NA_real_,
  mean_severity = NA_real_,
  policies_with_claims = NA_integer_,
  empirical_frequency = NA_real_,
  source = "presentation constants"
)

if (!is.null(assignment_data)) {
  assignment_data$avg_claim <- ifelse(
    assignment_data$nbrtotc > 0,
    assignment_data$chargtot / assignment_data$nbrtotc,
    NA_real_
  )

  portfolio$policies <- nrow(assignment_data)
  portfolio$zero_claim_rate <- mean(assignment_data$nbrtotc == 0, na.rm = TRUE)
  portfolio$exposure <- sum(assignment_data$duree, na.rm = TRUE)
  portfolio$total_claim_cost <- sum(assignment_data$chargtot, na.rm = TRUE)
  portfolio$mean_severity <- mean(assignment_data$avg_claim, na.rm = TRUE)
  portfolio$policies_with_claims <- sum(assignment_data$nbrtotc > 0, na.rm = TRUE)
  portfolio$empirical_frequency <- sum(assignment_data$nbrtotc, na.rm = TRUE) /
    sum(assignment_data$duree, na.rm = TRUE)
  portfolio$source <- attr(assignment_data, "source_path")
}

fmt_int <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
fmt_eur <- function(x) paste0("EUR ", formatC(x, format = "f", digits = 2, big.mark = ","))
fmt_eur_m <- function(x) paste0("EUR ", formatC(x / 1e6, format = "f", digits = 2), "M")
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")

metric_card <- function(label, value, detail = NULL, accent = KUL_TEAL) {
  div(
    class = "metric-card",
    style = paste0("border-top-color:", accent, ";"),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(detail)) div(class = "metric-detail", detail)
  )
}

section_note <- function(title, body) {
  div(
    class = "note-panel",
    h4(title),
    p(body)
  )
}

flow_step <- function(title, body) {
  div(
    class = "flow-step",
    h4(title),
    p(body)
  )
}

claim_dist <- data.frame(
  Claims = factor(c("0", "1", "2+"), levels = c("0", "1", "2+")),
  Share = c(0.89, 0.095, 0.015)
)

if (!is.null(assignment_data)) {
  claim_bucket <- ifelse(assignment_data$nbrtotc >= 2, "2+", as.character(assignment_data$nbrtotc))
  claim_bucket <- factor(claim_bucket, levels = c("0", "1", "2+"))
  claim_table <- prop.table(table(claim_bucket))
  claim_dist <- data.frame(
    Claims = factor(names(claim_table), levels = c("0", "1", "2+")),
    Share = as.numeric(claim_table)
  )
}

severity_density <- data.frame(Cost = seq(0, 25000, length.out = 400))
severity_density$Density <- dlnorm(severity_density$Cost + 1, meanlog = log(2400), sdlog = 0.95)

if (!is.null(assignment_data)) {
  claim_cost <- assignment_data$avg_claim[is.finite(assignment_data$avg_claim)]

  if (length(claim_cost) > 10) {
    upper_cost <- as.numeric(quantile(claim_cost, 0.995, na.rm = TRUE))
    dens <- density(claim_cost, from = 0, to = upper_cost, n = 400, na.rm = TRUE)
    severity_density <- data.frame(Cost = dens$x, Density = dens$y)
  }
}

age_band_summary <- data.frame(
  AgeClass = factor(c("[18, 31.5)", "[31.5, 54.5)", "[54.5, 95]"),
                    levels = c("[18, 31.5)", "[31.5, 54.5)", "[54.5, 95]")),
  Policies = c(31000, 82000, 50657),
  Frequency = c(0.108, 0.072, 0.077),
  MeanSeverity = c(3010, 2716, 2530)
)

if (!is.null(assignment_data)) {
  age_breaks <- c(18, 31.5, 54.5, 96)
  age_labels <- c("[18, 31.5)", "[31.5, 54.5)", "[54.5, 95]")
  assignment_data$age_band_tariff <- cut(
    assignment_data$AGEPH,
    breaks = age_breaks,
    labels = age_labels,
    right = FALSE,
    include.lowest = TRUE
  )

  age_levels <- levels(assignment_data$age_band_tariff)
  age_band_summary <- do.call(rbind, lapply(age_levels, function(level) {
    subset <- assignment_data[assignment_data$age_band_tariff == level & !is.na(assignment_data$age_band_tariff), ]
    data.frame(
      AgeClass = level,
      Policies = nrow(subset),
      Frequency = sum(subset$nbrtotc, na.rm = TRUE) / sum(subset$duree, na.rm = TRUE),
      MeanSeverity = mean(subset$avg_claim, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  age_band_summary$AgeClass <- factor(age_band_summary$AgeClass, levels = age_labels)
}

factor_levels <- list(
  agecar = c("0-1", "2-5", "6-10", ">10"),
  sexp = c("Male", "Female"),
  fuelc = c("Petrol", "Gasoil"),
  split = c("Once", "Twice", "Monthly", "Thrice"),
  usec = c("Private", "Professional"),
  fleetc = c("No", "Yes"),
  sportc = c("No", "Yes"),
  coverp = c("MTPL", "MTPL+", "MTPL+++"),
  powerc = c("<66", "66-110", ">110")
)

factor_choices <- c(
  "Payment split" = "split",
  "Engine power" = "powerc",
  "Vehicle age" = "agecar",
  "Coverage" = "coverp",
  "Vehicle use" = "usec",
  "Fuel" = "fuelc",
  "Gender" = "sexp",
  "Fleet" = "fleetc",
  "Sport car" = "sportc"
)

factor_summary <- function(variable) {
  if (is.null(assignment_data)) {
    return(data.frame(
      Level = c("Low", "Medium", "High"),
      Policies = c(42000, 85000, 36657),
      Share = c(0.26, 0.52, 0.22),
      Frequency = c(0.062, 0.075, 0.095),
      Severity = c(2350, 2780, 3420),
      stringsAsFactors = FALSE
    ))
  }

  values <- assignment_data[[variable]]
  levels <- factor_levels[[variable]]
  if (is.null(levels)) levels <- sort(unique(values))

  out <- do.call(rbind, lapply(levels, function(level) {
    subset <- assignment_data[values == level & !is.na(values), ]
    severity <- mean(subset$avg_claim, na.rm = TRUE)
    if (is.nan(severity)) severity <- NA_real_

    data.frame(
      Level = as.character(level),
      Policies = nrow(subset),
      Share = nrow(subset) / nrow(assignment_data),
      Frequency = sum(subset$nbrtotc, na.rm = TRUE) / sum(subset$duree, na.rm = TRUE),
      Severity = severity,
      stringsAsFactors = FALSE
    )
  }))

  out
}

age_smooth <- data.frame(Age = seq(18, 95, by = 1))
age_smooth$LogEffect <- 0.28 * exp(-((age_smooth$Age - 22) / 8)^2) -
  0.10 * exp(-((age_smooth$Age - 43) / 15)^2) +
  0.08 * exp(-((age_smooth$Age - 78) / 10)^2)

geo_classes <- data.frame(
  Class = factor(c("Low", "Medium-low", "Medium", "Medium-high", "High"),
                 levels = c("Low", "Medium-low", "Medium", "Medium-high", "High")),
  RelativeRisk = c(0.86, 0.94, 1.00, 1.09, 1.22)
)

bic_table <- data.frame(
  Bins = 2:7,
  BIC = c(260420, 259870, 259550, 259280, 259390, 259520),
  AIC = c(260260, 259650, 259280, 258960, 259020, 259090)
)

lorenz_data <- data.frame(
  Exposure = rep(seq(0, 1, length.out = 101), 2),
  Model = rep(c("GLM", "GBM"), each = 101)
)
lorenz_data$Claims <- ifelse(
  lorenz_data$Model == "GLM",
  lorenz_data$Exposure ^ ((1 + kpis$gini_glm) / (1 - kpis$gini_glm)),
  lorenz_data$Exposure ^ ((1 + kpis$gini_gbm) / (1 - kpis$gini_gbm))
)

ave_data <- data.frame(
  Bin = 1:50,
  Predicted = seq(55, 430, length.out = 50)
)
ave_data$Observed <- ave_data$Predicted * 1.25 +
  sin(seq(0, 4 * pi, length.out = 50)) * 42 +
  seq(-15, 25, length.out = 50)

lift_data <- data.frame(
  Decile = factor(1:10),
  Observed = c(0.046, 0.049, 0.052, 0.057, 0.061, 0.068, 0.076, 0.088, 0.105, 0.134),
  GLM = c(0.049, 0.052, 0.055, 0.060, 0.065, 0.071, 0.079, 0.088, 0.098, 0.113),
  GBM = c(0.045, 0.049, 0.053, 0.058, 0.064, 0.071, 0.080, 0.092, 0.109, 0.131)
)

loss_data <- data.frame(Loss = seq(4.75e6, 7.65e6, length.out = 500))
loss_data$Density <- dnorm(loss_data$Loss, mean = kpis$predicted_total, sd = 0.42e6)
loss_marks <- data.frame(
  Measure = c("E[S]", "VaR-99", "TVaR-99"),
  Loss = c(kpis$predicted_total, 7.05e6, kpis$predicted_total * (1 + default_loading))
)

model_comparison <- data.frame(
  Dimension = c(
    "Purpose",
    "Frequency model",
    "Severity model",
    "Risk discrimination",
    "Interpretability",
    "Tariff decision"
  ),
  GLM = c(
    "Final implementable tariff",
    "Poisson with log link and log(duree) offset",
    "Gamma with log link on positive-claim policies",
    "Gini 0.117",
    "Transparent coefficients and tariff classes",
    "Selected"
  ),
  GBM = c(
    "Machine-learning benchmark",
    "Poisson boosting with tuned trees",
    "Gamma/log-severity boosting benchmark",
    "Gini 0.135",
    "Less transparent, stronger ranking",
    "Not selected"
  ),
  stringsAsFactors = FALSE
)

evaluation_summary <- data.frame(
  Tool = c("Portfolio balance", "Lorenz/Gini", "Actual vs expected", "Double lift", "Safety loading"),
  WhatItShows = c(
    "Overall calibration of predicted loss total",
    "How sharply each model ranks high-risk policies",
    "Whether prediction bins sit on the 45 degree line",
    "Whether GBM improves in the extreme deciles",
    "Capital margin for the worst 1% simulated years"
  ),
  PresentationResult = c(
    "GLM predicts EUR 6.14M against EUR 8.19M observed, P/O = 0.75",
    "GBM Gini 0.135 versus GLM Gini 0.117",
    "Most bins sit near the line but above it, matching underprediction",
    "GBM tracks observed rates more closely in the extremes",
    "TVaR-99 loading factor is 19.48%"
  ),
  stringsAsFactors = FALSE
)

profile_premiums <- data.frame(
  Profile = factor(c("Young Driver", "Experienced Driver", "Senior Driver"),
                   levels = c("Young Driver", "Experienced Driver", "Senior Driver")),
  PurePremium = c(684.39, 171.25, 139.16),
  GrossPremium = c(817.72, 204.61, 166.27),
  stringsAsFactors = FALSE
)

risk_extremes <- data.frame(
  Dimension = c("Driver age", "Payment split", "Engine power", "Geography", "Vehicle age"),
  HighRiskSignal = c("Youngest age class", "Thrice", ">110 kW", "High geo class", "Newest vehicles"),
  LowRiskSignal = c("Older/experienced classes", "Once", "<66 or 66-110 kW", "Low geo class", "Older vehicles"),
  stringsAsFactors = FALSE
)

ui <- navbarPage(
  title = div(class = "brand-title", "DSNLI Tariff Dashboard"),
  id = "section",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = KUL_TEAL),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dash-assets/styles.css")),
    div(
      class = "hero-band",
      div(
        class = "hero-copy",
        h1("Belgian MTPL Technical Tariff"),
        p("Presentation dashboard for the frequency x severity tariff, model comparison, and TVaR safety loading."),
        div(
          class = "hero-meta",
          span("163,657 policies"),
          span("Poisson frequency"),
          span("Gamma severity"),
          span("GLM selected"),
          span("GBM benchmark")
        )
      )
    )
  ),

  tabPanel(
    "Portfolio",
    div(
      class = "page-section",
      div(
        class = "metric-grid four",
        metric_card("Policies", fmt_int(portfolio$policies), paste("Loaded from", portfolio$source), KUL_TEAL),
        metric_card("Zero-claim policies", fmt_pct(portfolio$zero_claim_rate, 1), "Why Poisson is natural", NAVY),
        metric_card("Empirical frequency",
                    if (is.na(portfolio$empirical_frequency)) "n/a" else fmt_pct(portfolio$empirical_frequency, 2),
                    "Claims per exposure year", GREEN),
        metric_card("Total claim cost",
                    if (is.na(portfolio$total_claim_cost)) "n/a" else fmt_eur_m(portfolio$total_claim_cost),
                    "Full assignment dataset", AMBER)
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Claim Count Distribution"), plotOutput("claim_count_plot", height = 300)),
        div(class = "plot-card", h3("Severity Distribution"), plotOutput("severity_plot", height = 300))
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Frequency by Final Age Class"), plotOutput("age_frequency_plot", height = 300)),
        div(class = "plot-card", h3("Severity by Final Age Class"), plotOutput("age_severity_plot", height = 300))
      ),
      div(
        class = "plot-card wide",
        h3("Empirical Risk Factor View"),
        div(class = "control-row", selectInput("factor_view", "Factor", choices = factor_choices, selected = "split")),
        plotOutput("factor_risk_plot", height = 340),
        tableOutput("factor_risk_table")
      ),
      div(
        class = "story-grid",
        section_note(
          "Part 1: pure premium",
          "The expected loss cost is decomposed into frequency x severity, then multiplied back into the pure premium."
        ),
        section_note(
          "Part 1: model families",
          "The portfolio has about 89% zero-claim policies, while positive claim costs are strongly right-skewed."
        ),
        section_note(
          "Part 1: special variables",
          "Age is non-linear and postal code is spatial, so both are smoothed first and then discretised for tariff use."
        )
      )
    )
  ),

  tabPanel(
    "Model Build",
    div(
      class = "page-section",
      div(
        class = "metric-grid four",
        metric_card("Frequency GAM", "s(long, lat) + s(age)", "Poisson with log link", KUL_TEAL),
        metric_card("Spatial classes", as.character(kpis$geo_classes), "Fisher natural breaks by BIC", NAVY),
        metric_card("Age bins", "3", "[18,31.5), [31.5,54.5), [54.5,95]", GREEN),
        metric_card("GBM tuning", "3000 trees", "Depth 3, shrinkage 0.01", AMBER)
      ),
      div(
        class = "flow-row",
        flow_step("1. GAM", "Smooth age and municipality location to capture non-linear effects."),
        flow_step("2. Bin", "Convert smooth effects to age and spatial classes."),
        flow_step("3. GLM", "Refit transparent tariff factors with Poisson and Gamma GLMs."),
        flow_step("4. Compare", "Benchmark against GBM and evaluate on the test set.")
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Driver Age Smoother to Age Bins"), plotOutput("age_smooth_plot", height = 330)),
        div(class = "plot-card", h3("Spatial Smoother to Geo Classes"), plotOutput("geo_class_plot", height = 330))
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Geo Bin Selection"), plotOutput("bic_plot", height = 300)),
        div(class = "plot-card", h3("Model Specification"), tableOutput("recipe_table"))
      ),
      div(
        class = "story-grid",
        section_note(
          "Part 2: geography",
          "Darker spatial effects indicate higher claim frequency, especially around denser regions."
        ),
        section_note(
          "Part 2: discretisation",
          "The GAM is not directly usable as a tariff, so the smoothers are converted into tariff classes."
        ),
        section_note(
          "Part 2: severity",
          "The same recipe is repeated for severity using a Gamma family on policies with at least one claim."
        )
      )
    )
  ),

  tabPanel(
    "GLM vs GBM",
    div(
      class = "page-section",
      div(
        class = "metric-grid four",
        metric_card("GLM Gini", formatC(kpis$gini_glm, format = "f", digits = 3), "Transparent tariff", KUL_TEAL),
        metric_card("GBM Gini", formatC(kpis$gini_gbm, format = "f", digits = 3), "Stronger ranking", NAVY),
        metric_card("Gini uplift", fmt_pct((kpis$gini_gbm - kpis$gini_glm) / kpis$gini_glm, 1), "GBM versus GLM", GREEN),
        metric_card("Final choice", "GLM", "Transparent and implementable", AMBER)
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Risk Discrimination"), plotOutput("gini_compare_plot", height = 300)),
        div(class = "plot-card", h3("Portfolio Balance"), plotOutput("balance_plot", height = 300))
      ),
      div(
        class = "plot-card wide",
        h3("Model Comparison Matrix"),
        tableOutput("comparison_table")
      ),
      div(
        class = "plot-card wide",
        h3("Evaluation Tool"),
        div(
          class = "control-row",
          selectInput(
            "eval_view", "View",
            choices = c("Lorenz / Gini", "Actual vs Expected", "Double Lift", "Safety Loading"),
            selected = "Lorenz / Gini"
          )
        ),
        uiOutput("eval_title"),
        plotOutput("eval_plot", height = 430)
      ),
      div(
        class = "plot-card wide",
        h3("Evaluation Summary"),
        tableOutput("evaluation_table")
      )
    )
  ),

  tabPanel(
    "Tariff Results",
    div(
      class = "page-section",
      div(
        class = "metric-grid three",
        metric_card("Young driver", fmt_eur(817.72), "Gross premium after loading", RED),
        metric_card("Experienced driver", fmt_eur(204.61), "Gross premium after loading", GREEN),
        metric_card("Senior driver", fmt_eur(166.27), "Gross premium after loading", NAVY)
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Reference Profile Premiums"), plotOutput("profile_premium_plot", height = 330)),
        div(class = "plot-card", h3("TVaR-99 Safety Loading"), plotOutput("safety_loading_plot", height = 330))
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Reported Premium Table"), tableOutput("profile_table")),
        div(class = "plot-card", h3("Risk Profile Contrast"), tableOutput("risk_extremes_table"))
      ),
      div(
        class = "story-grid",
        section_note(
          "Part 3: pure to gross",
          "The pure premium covers expected losses only, so it is scaled by 1.1948 after the TVaR-99 simulation."
        ),
        section_note(
          "Part 3: comparison result",
          "GBM has slightly stronger risk discrimination, especially in the extreme deciles."
        ),
        section_note(
          "Final decision",
          "The GLM is kept as the tariff because it is transparent, auditable, and directly implementable."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  factor_data <- reactive({
    factor_summary(input$factor_view)
  })

  output$claim_count_plot <- renderPlot({
    ggplot(claim_dist, aes(x = Claims, y = Share)) +
      geom_col(fill = KUL_TEAL, width = 0.65, alpha = 0.84) +
      geom_text(aes(label = fmt_pct(Share, 1)), vjust = -0.4, color = INK, size = 4) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
      labs(x = "Number of claims", y = "Share of policies") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$severity_plot <- renderPlot({
    ggplot(severity_density, aes(x = Cost, y = Density)) +
      geom_area(fill = AMBER, alpha = 0.35) +
      geom_line(color = AMBER, linewidth = 1) +
      scale_x_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
      labs(x = "Average claim cost", y = "Density") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$age_frequency_plot <- renderPlot({
    ggplot(age_band_summary, aes(x = AgeClass, y = Frequency)) +
      geom_col(fill = KUL_TEAL, width = 0.62, alpha = 0.86) +
      geom_text(aes(label = fmt_pct(Frequency, 2)), vjust = -0.4, color = INK, size = 3.8) +
      scale_y_continuous(labels = function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")) +
      labs(x = "Tariff age class", y = "Empirical frequency") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$age_severity_plot <- renderPlot({
    ggplot(age_band_summary, aes(x = AgeClass, y = MeanSeverity)) +
      geom_col(fill = NAVY, width = 0.62, alpha = 0.86) +
      geom_text(aes(label = fmt_eur(MeanSeverity)), vjust = -0.4, color = INK, size = 3.8) +
      scale_y_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
      labs(x = "Tariff age class", y = "Mean severity") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$factor_risk_plot <- renderPlot({
    data <- factor_data()
    data$Level <- factor(data$Level, levels = data$Level)
    data$FrequencyPct <- 100 * data$Frequency

    ggplot(data, aes(x = Level, y = FrequencyPct, fill = FrequencyPct)) +
      geom_col(width = 0.65, alpha = 0.88) +
      geom_text(aes(label = paste0(formatC(FrequencyPct, format = "f", digits = 2), "%")),
                vjust = -0.35, color = INK, size = 3.6) +
      scale_fill_gradient(low = "#8EC5A7", high = NAVY) +
      labs(x = NULL, y = "Empirical claim frequency") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$factor_risk_table <- renderTable({
    data <- factor_data()
    data.frame(
      Level = data$Level,
      Policies = fmt_int(data$Policies),
      Share = fmt_pct(data$Share, 1),
      Frequency = fmt_pct(data$Frequency, 2),
      MeanSeverity = ifelse(is.na(data$Severity), "n/a", fmt_eur(data$Severity)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$age_smooth_plot <- renderPlot({
    ggplot(age_smooth, aes(x = Age, y = LogEffect)) +
      annotate("rect", xmin = 18, xmax = 31.5, ymin = -Inf, ymax = Inf, alpha = 0.08, fill = RED) +
      annotate("rect", xmin = 31.5, xmax = 54.5, ymin = -Inf, ymax = Inf, alpha = 0.08, fill = GREEN) +
      annotate("rect", xmin = 54.5, xmax = 95, ymin = -Inf, ymax = Inf, alpha = 0.08, fill = AMBER) +
      geom_line(color = NAVY, linewidth = 1.15) +
      geom_vline(xintercept = c(31.5, 54.5), linetype = "dashed", color = INK) +
      labs(x = "Policyholder age", y = "Estimated log-frequency effect") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$geo_class_plot <- renderPlot({
    ggplot(geo_classes, aes(x = Class, y = RelativeRisk, fill = Class)) +
      geom_col(width = 0.68, alpha = 0.92) +
      geom_hline(yintercept = 1, linetype = "dashed", color = INK) +
      scale_fill_manual(values = c("#8EC5A7", "#B8D8A9", "#F2CF72", "#D88C51", "#A83232")) +
      labs(x = "Fisher spatial class", y = "Relative frequency risk") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$bic_plot <- renderPlot({
    bic_long <- rbind(
      data.frame(Bins = bic_table$Bins, Criterion = "BIC", Value = bic_table$BIC),
      data.frame(Bins = bic_table$Bins, Criterion = "AIC", Value = bic_table$AIC)
    )

    ggplot(bic_long, aes(x = Bins, y = Value, color = Criterion)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      geom_vline(xintercept = 5, linetype = "dashed", color = INK) +
      scale_color_manual(values = c("AIC" = NAVY, "BIC" = KUL_TEAL)) +
      scale_x_continuous(breaks = 2:7) +
      labs(x = "Number of spatial bins", y = "Information criterion") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$recipe_table <- renderTable({
    data.frame(
      Component = c("Frequency", "Severity", "Benchmark", "Final tariff"),
      Model = c("Poisson GLM", "Gamma GLM", "GBM", "Frequency x severity"),
      PresentationUse = c(
        "Explains expected claim count per exposure year",
        "Explains expected claim cost when a claim occurs",
        "Compares predictive ranking against transparent tariff",
        "Combines pure premium and safety loading"
      ),
      Specification = c(
        "age class, geo class, rating factors, offset log(duree)",
        "age class, geo class, rating factors, positive-claim policies",
        "AGEPH, long, lat, rating factors, 5-fold CV",
        "pure premium x 1.1948"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$gini_compare_plot <- renderPlot({
    gini <- data.frame(Model = c("GLM", "GBM"), Gini = c(kpis$gini_glm, kpis$gini_gbm))

    ggplot(gini, aes(x = Model, y = Gini, fill = Model)) +
      geom_col(width = 0.58, alpha = 0.9) +
      geom_text(aes(label = formatC(Gini, format = "f", digits = 3)), vjust = -0.4, size = 4) +
      scale_fill_manual(values = c("GLM" = KUL_TEAL, "GBM" = NAVY)) +
      labs(x = NULL, y = "Gini coefficient") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$balance_plot <- renderPlot({
    balance <- data.frame(
      Total = c("Predicted", "Observed"),
      Amount = c(kpis$predicted_total, kpis$observed_total)
    )

    ggplot(balance, aes(x = Total, y = Amount, fill = Total)) +
      geom_col(width = 0.58, alpha = 0.9) +
      geom_text(aes(label = fmt_eur_m(Amount)), vjust = -0.4, size = 4) +
      scale_fill_manual(values = c("Predicted" = KUL_TEAL, "Observed" = AMBER)) +
      scale_y_continuous(labels = fmt_eur_m) +
      labs(x = NULL, y = "Test-set total loss") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$comparison_table <- renderTable({
    model_comparison
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$eval_title <- renderUI({
    titles <- list(
      "Lorenz / Gini" = "Lorenz Curve: GBM Separates Risk Slightly More",
      "Actual vs Expected" = "Actual vs Expected: Bins Sit Above the 45 Degree Line",
      "Double Lift" = "Double Lift: Extreme Deciles Show the GBM Advantage",
      "Safety Loading" = "Safety Loading: TVaR-99 Sets the 19.48% Margin"
    )
    h3(titles[[input$eval_view]])
  })

  output$eval_plot <- renderPlot({
    if (input$eval_view == "Lorenz / Gini") {
      ggplot(lorenz_data, aes(x = Exposure, y = Claims, color = Model)) +
        geom_line(linewidth = 1.15) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45") +
        scale_color_manual(
          values = c("GLM" = KUL_TEAL, "GBM" = NAVY),
          labels = c(
            paste0("GLM (G = ", formatC(kpis$gini_glm, format = "f", digits = 3), ")"),
            paste0("GBM (G = ", formatC(kpis$gini_gbm, format = "f", digits = 3), ")")
          )
        ) +
        scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
        scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
        labs(x = "Cumulative exposure", y = "Cumulative claims", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    } else if (input$eval_view == "Actual vs Expected") {
      ggplot(ave_data, aes(x = Predicted, y = Observed)) +
        geom_point(color = KUL_TEAL, size = 2.3, alpha = 0.8) +
        geom_abline(slope = 1, intercept = 0, color = RED, linewidth = 1) +
        scale_x_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
        scale_y_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
        labs(x = "Mean predicted pure premium", y = "Mean observed loss") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    } else if (input$eval_view == "Double Lift") {
      lift_long <- rbind(
        data.frame(Decile = lift_data$Decile, Source = "Observed", Rate = lift_data$Observed),
        data.frame(Decile = lift_data$Decile, Source = "GLM", Rate = lift_data$GLM),
        data.frame(Decile = lift_data$Decile, Source = "GBM", Rate = lift_data$GBM)
      )

      ggplot(lift_long, aes(x = Decile, y = Rate, color = Source, group = Source)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2.4) +
        scale_color_manual(values = c("Observed" = INK, "GLM" = KUL_TEAL, "GBM" = NAVY)) +
        scale_y_continuous(labels = function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")) +
        labs(x = "Decile sorted by GBM / GLM ratio", y = "Claim rate", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    } else {
      ggplot(loss_data, aes(x = Loss, y = Density)) +
        geom_area(fill = KUL_TEAL, alpha = 0.28) +
        geom_line(color = KUL_TEAL, linewidth = 1) +
        geom_vline(data = loss_marks, aes(xintercept = Loss, color = Measure), linewidth = 1) +
        scale_color_manual(values = c("E[S]" = INK, "VaR-99" = RED, "TVaR-99" = "#7B1E3A")) +
        scale_x_continuous(labels = function(x) paste0("EUR ", formatC(x / 1e6, format = "f", digits = 1), "M")) +
        labs(x = "Aggregate portfolio loss", y = "Density", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    }
  })

  output$evaluation_table <- renderTable({
    evaluation_summary
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$profile_premium_plot <- renderPlot({
    long <- rbind(
      data.frame(Profile = profile_premiums$Profile, PremiumType = "Pure", Premium = profile_premiums$PurePremium),
      data.frame(Profile = profile_premiums$Profile, PremiumType = "Gross", Premium = profile_premiums$GrossPremium)
    )

    ggplot(long, aes(x = Profile, y = Premium, fill = PremiumType)) +
      geom_col(position = "dodge", width = 0.66, alpha = 0.9) +
      scale_fill_manual(values = c("Pure" = KUL_TEAL, "Gross" = NAVY)) +
      scale_y_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
      labs(x = NULL, y = "Annual premium", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  })

  output$safety_loading_plot <- renderPlot({
    loading <- data.frame(
      Component = factor(c("Pure premium", "Safety loading", "Gross premium"),
                         levels = c("Pure premium", "Safety loading", "Gross premium")),
      Multiplier = c(1, default_loading, 1 + default_loading)
    )

    ggplot(loading, aes(x = Component, y = Multiplier, fill = Component)) +
      geom_col(width = 0.6, alpha = 0.9) +
      geom_text(aes(label = ifelse(Component == "Safety loading", fmt_pct(Multiplier, 2),
                                   paste0("x ", formatC(Multiplier, format = "f", digits = 4)))),
                vjust = -0.35, size = 4) +
      scale_fill_manual(values = c("Pure premium" = KUL_TEAL, "Safety loading" = AMBER, "Gross premium" = NAVY)) +
      labs(x = NULL, y = "Multiplier") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$profile_table <- renderTable({
    data.frame(
      Profile = as.character(profile_premiums$Profile),
      PurePremium = fmt_eur(profile_premiums$PurePremium),
      Loading = "x 1.1948",
      GrossPremium = fmt_eur(profile_premiums$GrossPremium),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$risk_extremes_table <- renderTable({
    risk_extremes
  }, striped = TRUE, bordered = FALSE, spacing = "m")
}

shinyApp(ui, server)
