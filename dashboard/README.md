# DSNLI Shiny Dashboard

Interactive presentation dashboard for the Belgian MTPL technical tariff project.

## Run locally

From the repository root:

```r
shiny::runApp("dashboard")
```

or from a terminal:

```bash
Rscript -e "shiny::runApp('dashboard', launch.browser = TRUE)"
```

The app reads `assignment_data.csv` from the repository root when available. The evaluation KPIs are fixed to the presentation/test-set results:

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
rsconnect::deployApp("dashboard")
```

If deploying to a service where the parent repository files are not bundled, either deploy from the repository root with `assignment_data.csv` included or copy the CSV into `dashboard/data/assignment_data.csv`.
