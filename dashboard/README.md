# DSNLI Shiny Dashboard

Interactive presentation dashboard for the Belgian MTPL technical tariff project.
The dashboard is structured around the presentation: portfolio evidence, model construction, GLM-vs-GBM comparison, and final tariff results.

## Run locally

From the repository root:

```r
shiny::runApp("dashboard")
```

or from a terminal:

```bash
Rscript -e "shiny::runApp('dashboard', launch.browser = TRUE)"
```

The app reads `assignment_data.csv` from the repository root when available. The portfolio and empirical factor views are data-driven. The evaluation KPIs are fixed to the presentation/test-set results:

- GLM predicted total: EUR 6.14 million
- Observed total: EUR 8.19 million
- P/O ratio: 0.75
- Gini: 0.117 for GLM and 0.135 for GBM
- TVaR-99 loading: 19.48%

## Deploy

Install the deployment package once:

```r
install.packages("rsconnect")
```

Then deploy from the repository root:

```r
dir.create("dashboard/data", showWarnings = FALSE)
file.copy("assignment_data.csv", "dashboard/data/assignment_data.csv", overwrite = TRUE)
rsconnect::deployApp("dashboard")
```

The copied CSV is ignored by Git so the repository does not store the 11 MB dataset twice.
