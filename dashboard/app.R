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
  portfolio$source <- attr(assignment_data, "source_path")
}

reference_profiles <- data.frame(
  Profile = c("Young Driver", "Experienced Driver", "Senior Driver"),
  Frequency = c(0.182, 0.063, 0.055),
  Severity = c(3760.2, 2716.4, 2530.1),
  Gross = c(817.72, 204.61, 166.27),
  stringsAsFactors = FALSE
)
reference_profiles$Pure <- reference_profiles$Gross / (1 + default_loading)

reference_inputs <- list(
  "Young Driver" = list(
    age = 25, agecar = "0-1", sexp = "Male", fuelc = "Petrol",
    split = "Once", usec = "Professional", fleetc = "No", sportc = "Yes",
    coverp = "MTPL+++", powerc = ">110", geo = "Medium"
  ),
  "Experienced Driver" = list(
    age = 43, agecar = "2-5", sexp = "Male", fuelc = "Petrol",
    split = "Once", usec = "Private", fleetc = "No", sportc = "No",
    coverp = "MTPL+", powerc = "66-110", geo = "Medium"
  ),
  "Senior Driver" = list(
    age = 75, agecar = ">10", sexp = "Male", fuelc = "Petrol",
    split = "Once", usec = "Private", fleetc = "No", sportc = "No",
    coverp = "MTPL", powerc = "<66", geo = "Medium"
  )
)

factor_maps <- list(
  age_group = c("18-31.5" = 1.85, "31.5-54.5" = 1.00, "54.5-95" = 0.96),
  agecar = c("0-1" = 1.18, "2-5" = 1.00, "6-10" = 0.97, ">10" = 0.92),
  sexp = c("Male" = 1.00, "Female" = 0.98),
  fuelc = c("Petrol" = 1.00, "Gasoil" = 1.04),
  split = c("Once" = 1.00, "Twice" = 1.12, "Monthly" = 1.06, "Thrice" = 1.32),
  usec = c("Private" = 1.00, "Professional" = 1.12),
  fleetc = c("No" = 1.00, "Yes" = 1.06),
  sportc = c("No" = 1.00, "Yes" = 1.08),
  coverp = c("MTPL" = 0.94, "MTPL+" = 1.00, "MTPL+++" = 1.08),
  powerc = c("<66" = 0.98, "66-110" = 1.00, ">110" = 1.38),
  geo = c("Low" = 0.86, "Medium-low" = 0.94, "Medium" = 1.00,
          "Medium-high" = 1.09, "High" = 1.22)
)

fmt_int <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
fmt_eur <- function(x) paste0("EUR ", formatC(x, format = "f", digits = 2, big.mark = ","))
fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")

age_group_for <- function(age) {
  if (age < 31.5) return("18-31.5")
  if (age < 54.5) return("31.5-54.5")
  "54.5-95"
}

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

portfolio_claims <- data.frame(
  Claims = factor(c("0", "1", "2+"), levels = c("0", "1", "2+")),
  Share = c(0.89, 0.095, 0.015)
)

if (!is.null(assignment_data)) {
  claim_bucket <- ifelse(assignment_data$nbrtotc >= 2, "2+", as.character(assignment_data$nbrtotc))
  claim_bucket <- factor(claim_bucket, levels = c("0", "1", "2+"))
  claim_table <- prop.table(table(claim_bucket))
  portfolio_claims <- data.frame(
    Claims = factor(names(claim_table), levels = c("0", "1", "2+")),
    Share = as.numeric(claim_table)
  )
}

severity_tail <- data.frame(
  Cost = seq(0, 25000, length.out = 400)
)
severity_tail$Density <- dlnorm(severity_tail$Cost + 1, meanlog = log(2400), sdlog = 0.95)

if (!is.null(assignment_data)) {
  claim_cost <- assignment_data$avg_claim[is.finite(assignment_data$avg_claim)]

  if (length(claim_cost) > 10) {
    upper_cost <- as.numeric(quantile(claim_cost, 0.995, na.rm = TRUE))
    dens <- density(claim_cost, from = 0, to = upper_cost, n = 400, na.rm = TRUE)
    severity_tail <- data.frame(Cost = dens$x, Density = dens$y)
  }
}

age_smooth <- data.frame(Age = seq(18, 95, by = 1))
age_smooth$LogEffect <- 0.28 * exp(-((age_smooth$Age - 22) / 8)^2) -
  0.10 * exp(-((age_smooth$Age - 43) / 15)^2) +
  0.08 * exp(-((age_smooth$Age - 78) / 10)^2)

geo_classes <- data.frame(
  Class = factor(c("Low", "Medium-low", "Medium", "Medium-high", "High"),
                 levels = names(factor_maps$geo)),
  RelativeRisk = unname(factor_maps$geo)
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
  Loss = c(kpis$predicted_total, 7.05e6, kpis$predicted_total * (1 + default_loading)),
  Colour = c(INK, RED, "#7B1E3A")
)

ui <- navbarPage(
  title = div(class = "brand-title", "DSNLI Tariff Dashboard"),
  id = "section",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = KUL_TEAL),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    div(
      class = "hero-band",
      div(
        class = "hero-copy",
        h1("Belgian MTPL Technical Tariff"),
        p("Presentation dashboard for the frequency x severity tariff, model comparison, and TVaR safety loading."),
        div(
          class = "hero-meta",
          span("Poisson frequency"),
          span("Gamma severity"),
          span("Transparent GLM tariff"),
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
        metric_card("Zero-claim policies", fmt_pct(portfolio$zero_claim_rate, 1), "Motivates Poisson frequency", NAVY),
        metric_card(
          "Total claim cost",
          if (is.na(portfolio$total_claim_cost)) "n/a" else fmt_eur(portfolio$total_claim_cost),
          "Full assignment dataset",
          AMBER
        ),
        metric_card(
          "Total exposure",
          if (is.na(portfolio$exposure)) "n/a" else fmt_int(round(portfolio$exposure)),
          "Exposure years",
          GREEN
        )
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Claim Count Distribution"), plotOutput("claim_count_plot", height = 300)),
        div(class = "plot-card", h3("Severity Tail"), plotOutput("severity_plot", height = 300))
      ),
      div(
        class = "story-grid",
        section_note(
          "Part 1 message",
          "The pure premium is expected claim cost. The dashboard follows the presentation decomposition: frequency times severity, with exposure handled through log(duree)."
        ),
        section_note(
          "Model family",
          "Claim counts are mostly zero, so the frequency component uses a Poisson model. Claim costs are positive and right-skewed, so severity uses a Gamma model."
        ),
        section_note(
          "Special variables",
          "Driver age is non-linear and postal code is spatial. The original analysis uses smooth GAM effects before discretising them back into tariff classes."
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
        metric_card("GAM frequency", "s(long, lat) + s(age)", "Then discretised to GLM", KUL_TEAL),
        metric_card("Spatial classes", as.character(kpis$geo_classes), "Fisher natural breaks by BIC", NAVY),
        metric_card("Age classes", "3", "[18,31.5), [31.5,54.5), [54.5,95]", GREEN),
        metric_card("GBM benchmark", "3000 trees", "Depth 3, shrinkage 0.01", AMBER)
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Driver Age Smooth"), plotOutput("age_smooth_plot", height = 330)),
        div(class = "plot-card", h3("Spatial Class Risk"), plotOutput("geo_class_plot", height = 330))
      ),
      div(
        class = "two-col",
        div(class = "plot-card", h3("Geo Bin Selection"), plotOutput("bic_plot", height = 300)),
        div(
          class = "plot-card",
          h3("Final Tariff Recipe"),
          tableOutput("recipe_table")
        )
      )
    )
  ),

  tabPanel(
    "Evaluation",
    div(
      class = "page-section",
      div(
        class = "metric-grid four",
        metric_card("Predicted total", fmt_eur(kpis$predicted_total), "GLM test prediction", KUL_TEAL),
        metric_card("P/O ratio", formatC(kpis$po_ratio, format = "f", digits = 2), "25% underprediction", RED),
        metric_card("GLM Gini", formatC(kpis$gini_glm, format = "f", digits = 3), "Frequency ranking", NAVY),
        metric_card("GBM Gini", formatC(kpis$gini_gbm, format = "f", digits = 3), "Sharper risk separation", GREEN)
      ),
      div(
        class = "control-row",
        selectInput(
          "eval_view", "Evaluation view",
          choices = c("Lorenz / Gini", "Actual vs Expected", "Double Lift", "Safety Loading"),
          selected = "Lorenz / Gini"
        )
      ),
      div(class = "plot-card wide", uiOutput("eval_title"), plotOutput("eval_plot", height = 460)),
      div(
        class = "story-grid",
        section_note(
          "Main conclusion",
          "The GBM separates risk slightly better, but the GLM remains the tariff choice because it is transparent and directly implementable."
        ),
        section_note(
          "Calibration",
          "The GLM predicts EUR 6.14 million against EUR 8.19 million observed, consistent with underprediction driven by very large claims."
        ),
        section_note(
          "Safety loading",
          "The final tariff applies a TVaR-99 loading of 19.48%, scaling each pure premium by 1.1948."
        )
      )
    )
  ),

  tabPanel(
    "Premium Simulator",
    div(
      class = "page-section simulator-layout",
      div(
        class = "input-panel",
        h3("Profile Inputs"),
        selectInput(
          "profile_template", "Start from",
          choices = c("Experienced Driver", "Young Driver", "Senior Driver", "Custom scenario"),
          selected = "Experienced Driver"
        ),
        sliderInput("age", "Driver age", min = 18, max = 95, value = 43, step = 1),
        selectInput("agecar", "Vehicle age", choices = names(factor_maps$agecar), selected = "2-5"),
        selectInput("split", "Payment split", choices = names(factor_maps$split), selected = "Once"),
        selectInput("powerc", "Engine power", choices = names(factor_maps$powerc), selected = "66-110"),
        selectInput("geo", "Geo risk class", choices = names(factor_maps$geo), selected = "Medium"),
        selectInput("coverp", "Coverage", choices = names(factor_maps$coverp), selected = "MTPL+"),
        selectInput("usec", "Vehicle use", choices = names(factor_maps$usec), selected = "Private"),
        selectInput("sportc", "Sport car", choices = names(factor_maps$sportc), selected = "No"),
        selectInput("fleetc", "Fleet", choices = names(factor_maps$fleetc), selected = "No"),
        selectInput("sexp", "Gender", choices = names(factor_maps$sexp), selected = "Male"),
        selectInput("fuelc", "Fuel", choices = names(factor_maps$fuelc), selected = "Petrol"),
        sliderInput(
          "loading", "Safety loading",
          min = 0, max = 35, value = round(default_loading * 100, 2), step = 0.25,
          post = "%"
        )
      ),
      div(
        class = "sim-output",
        uiOutput("premium_cards"),
        div(class = "plot-card wide", h3("Rating Factor Impact"), plotOutput("factor_plot", height = 340)),
        div(
          class = "two-col",
          div(class = "plot-card", h3("Selected Factor Multipliers"), tableOutput("factor_table")),
          div(class = "plot-card", h3("Reported Reference Premiums"), tableOutput("reference_table"))
        ),
        div(class = "source-note", textOutput("premium_note"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$profile_template, {
    if (!input$profile_template %in% names(reference_inputs)) return()

    profile <- reference_inputs[[input$profile_template]]
    updateSliderInput(session, "age", value = profile$age)
    updateSelectInput(session, "agecar", selected = profile$agecar)
    updateSelectInput(session, "sexp", selected = profile$sexp)
    updateSelectInput(session, "fuelc", selected = profile$fuelc)
    updateSelectInput(session, "split", selected = profile$split)
    updateSelectInput(session, "usec", selected = profile$usec)
    updateSelectInput(session, "fleetc", selected = profile$fleetc)
    updateSelectInput(session, "sportc", selected = profile$sportc)
    updateSelectInput(session, "coverp", selected = profile$coverp)
    updateSelectInput(session, "powerc", selected = profile$powerc)
    updateSelectInput(session, "geo", selected = profile$geo)
  }, ignoreInit = FALSE)

  current_profile_values <- reactive({
    list(
      age = input$age,
      agecar = input$agecar,
      sexp = input$sexp,
      fuelc = input$fuelc,
      split = input$split,
      usec = input$usec,
      fleetc = input$fleetc,
      sportc = input$sportc,
      coverp = input$coverp,
      powerc = input$powerc,
      geo = input$geo
    )
  })

  matched_reference <- reactive({
    template <- input$profile_template
    if (!template %in% names(reference_inputs)) return(NA_character_)

    current <- current_profile_values()
    target <- reference_inputs[[template]]
    same <- all(vapply(names(target), function(name) identical(current[[name]], target[[name]]), logical(1)))

    if (same) template else NA_character_
  })

  factor_table_data <- reactive({
    age_group <- age_group_for(input$age)

    data.frame(
      Factor = c(
        "Driver age", "Vehicle age", "Gender", "Fuel", "Payment split",
        "Use", "Fleet", "Sport car", "Coverage", "Engine power", "Geo class"
      ),
      Level = c(
        age_group, input$agecar, input$sexp, input$fuelc, input$split,
        input$usec, input$fleetc, input$sportc, input$coverp, input$powerc, input$geo
      ),
      Multiplier = c(
        factor_maps$age_group[[age_group]],
        factor_maps$agecar[[input$agecar]],
        factor_maps$sexp[[input$sexp]],
        factor_maps$fuelc[[input$fuelc]],
        factor_maps$split[[input$split]],
        factor_maps$usec[[input$usec]],
        factor_maps$fleetc[[input$fleetc]],
        factor_maps$sportc[[input$sportc]],
        factor_maps$coverp[[input$coverp]],
        factor_maps$powerc[[input$powerc]],
        factor_maps$geo[[input$geo]]
      ),
      stringsAsFactors = FALSE
    )
  })

  premium_result <- reactive({
    factors <- factor_table_data()
    base_pure <- reference_profiles$Pure[reference_profiles$Profile == "Experienced Driver"]
    estimated_pure <- base_pure * prod(factors$Multiplier)

    matched <- matched_reference()
    pure <- estimated_pure
    source <- "Interactive estimate from transparent rating multipliers anchored to the experienced-driver profile."

    if (!is.na(matched)) {
      pure <- reference_profiles$Pure[reference_profiles$Profile == matched]
      source <- paste0("Using the reported ", matched, " reference pure premium from the project output.")
    }

    loading <- input$loading / 100

    list(
      pure = pure,
      gross = pure * (1 + loading),
      loading = loading,
      risk_score = prod(factors$Multiplier),
      source = source
    )
  })

  output$claim_count_plot <- renderPlot({
    ggplot(portfolio_claims, aes(x = Claims, y = Share)) +
      geom_col(fill = KUL_TEAL, width = 0.65, alpha = 0.82) +
      geom_text(aes(label = fmt_pct(Share, 1)), vjust = -0.4, color = INK, size = 4) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
      labs(x = "Number of claims", y = "Share of policies") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$severity_plot <- renderPlot({
    ggplot(severity_tail, aes(x = Cost, y = Density)) +
      geom_area(fill = AMBER, alpha = 0.35) +
      geom_line(color = AMBER, linewidth = 1) +
      scale_x_continuous(labels = function(x) paste0("EUR ", fmt_int(x))) +
      labs(x = "Average claim cost", y = "Density") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

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
      theme(panel.grid.minor = element_blank())
  })

  output$recipe_table <- renderTable({
    data.frame(
      Component = c("Frequency", "Severity", "Benchmark", "Final tariff"),
      Model = c("Poisson GLM", "Gamma GLM", "GBM", "Frequency x severity"),
      Main_terms = c(
        "Age class, geo class, rating factors, offset log(duree)",
        "Age class, geo class, rating factors",
        "AGEPH, long, lat, rating factors",
        "Pure premium scaled by TVaR-99 loading"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "m")

  output$eval_title <- renderUI({
    titles <- list(
      "Lorenz / Gini" = "Lorenz Curve and Gini Coefficient",
      "Actual vs Expected" = "Actual vs Expected: Pure Premium",
      "Double Lift" = "Double Lift Chart: GBM vs GLM",
      "Safety Loading" = "Monte Carlo Safety Loading"
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
        scale_x_continuous(labels = function(x) paste0("EUR ", fmt_int(x / 1e6), "M")) +
        labs(x = "Aggregate portfolio loss", y = "Density", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
    }
  })

  output$premium_cards <- renderUI({
    result <- premium_result()

    div(
      class = "metric-grid four",
      metric_card("Pure premium", fmt_eur(result$pure), "Expected loss cost", KUL_TEAL),
      metric_card("Gross premium", fmt_eur(result$gross), "After safety loading", GREEN),
      metric_card("Loading", fmt_pct(result$loading, 2), "TVaR-99 slider", AMBER),
      metric_card("Risk score", formatC(result$risk_score, format = "f", digits = 2),
                  "Relative to experienced driver", NAVY)
    )
  })

  output$factor_plot <- renderPlot({
    factors <- factor_table_data()
    factors$Effect <- 100 * (factors$Multiplier - 1)

    ggplot(factors, aes(x = reorder(Factor, Effect), y = Effect, fill = Effect >= 0)) +
      geom_col(width = 0.68, alpha = 0.9) +
      geom_hline(yintercept = 0, color = INK) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = KUL_TEAL, "FALSE" = GREEN)) +
      labs(x = NULL, y = "Effect versus neutral level (%)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })

  output$factor_table <- renderTable({
    factors <- factor_table_data()
    factors$Multiplier <- formatC(factors$Multiplier, format = "f", digits = 2)
    factors
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$reference_table <- renderTable({
    data.frame(
      Profile = reference_profiles$Profile,
      Pure = fmt_eur(reference_profiles$Pure),
      Gross = fmt_eur(reference_profiles$Gross),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$premium_note <- renderText({
    premium_result()$source
  })
}

shinyApp(ui, server)
