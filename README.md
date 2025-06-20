dbhydroInsights R Package
=========================

Install from GitHub (package is not on CRAN):

```r
remotes::install_github("walkerjeffd/dbhydroInsights")
```

Quick start:

```r
library(dbhydroInsights)

locations <- get_locations()

ts_metadata <- get_timeseries_metadata(dbkey = "91510")

ts <- get_timeseries_data(dbkeys = "91510", startDate = "20200101", endDate = "20240101")

wq_filters <- get_wq_filters()
wq_filters$parameter

get_wq_metadata(parameters = "25", startDate = "20190101", endDate = "20191231")

get_wq_data(locations = c("G722", "BB52"), parameters = c("18", "25"), startDate = "20190101", endDate = "20241231")
```

Reference Tables: https://apps.sfwmd.gov/dbhydro-insights-data/#/reference-tables
