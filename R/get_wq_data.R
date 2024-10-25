#' Get Water Quality Data
#'
#' Retrieves water quality data based on specified filters and date range.
#'
#' @param ... Query filters (locations, parameters, methods, projects, matrices, paramGroups) see `create_wq_query()`
#' @param timeseriesIds Character vector of timeseries IDs (optional)
#' @param startDate Start date as string in "YYYY-MM-DD" format
#' @param endDate End date as string in "YYYY-MM-DD" format
#'
#' @return A tibble of water quality values (NULL if no data found)
#' @export
#'
#' @examples
#' get_wq_data(locations = "G722", parameters = "25", 
#'             startDate = "2020-01-01", endDate = "2024-01-01")
get_wq_data <- function(..., timeseriesIds = NULL, startDate = NULL, endDate = NULL) {
  query <- create_wq_query(...)

  startDate <- format(parse_date(startDate), "%Y%m%d")
  endDate <- format(parse_date(endDate), "%Y%m%d")

  if (is.null(startDate) || is.null(endDate)) {
    stop("startDate and endDate must be provided in YMD format")
  }

  url <- "https://api.sfwmd.gov/v1/insights-data/chem/report/data"
  req <- httr2::request(url)
  req <- httr2::req_url_query(
    req,
    reportType = "timeseries",
    format = "csv",
    startDate = startDate,
    endDate = endDate
  )

  req <- httr2::req_method(req, "POST")
  req <- httr2::req_body_json(req, list(query = query))
  resp <- httr2::req_perform(req)

  if (httr2::resp_content_type(resp) != "text/csv") {
    stop("Response does not contain CSV data")
  }

  csv_text <- httr2::resp_body_string(resp)

  if (nrow(readr::read_csv(csv_text, comment = "#")) == 0) {
    warning("No data returned")
    return(NULL)
  }

  readr::read_csv(csv_text, comment = "#", col_types = readr::cols(
    .default = readr::col_character(),
    value = readr::col_double(),
    collectDate = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
    mdl = readr::col_double(),
    depth = readr::col_double(),
    dcsMeters = readr::col_double(),
    discharge = readr::col_double(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    totalDepth = readr::col_double()
  ))
}
