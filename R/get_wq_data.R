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

  if (nrow(readr::read_csv(csv_text, comment = "#", show_col_types = FALSE)) == 0) {
    return(NULL)
  }

  x <- readr::read_csv(csv_text, comment = "#", col_types = readr::cols(
    .default = readr::col_character(),
    collectDate = readr::col_datetime(format = "%Y-%m-%d %H:%M"),
    firstTriggerDate = readr::col_datetime(format = "%Y-%m-%d %H:%M"),
    depth = readr::col_double(),
    testNumber = readr::col_double(),
    value = readr::col_double(),
    dilution = readr::col_double(),
    mdl = readr::col_double(),
    pql = readr::col_double(),
    rdl = readr::col_double(),
    nDec = readr::col_double(),
    discharge = readr::col_double(),
    weatherCode = readr::col_double(),
    collectionSpan = readr::col_double(),
    dcsMeters = readr::col_double(),
    totalDepth = readr::col_double(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    validationLevel = readr::col_double(),
    receiveDate = readr::col_datetime(format = "%Y-%m-%d %H:%M"),
    measureDate = readr::col_datetime(format = "%Y-%m-%d %H:%M"),
    filtrationDate = readr::col_datetime(format = "%Y-%m-%d %H:%M")
  ))

  if (nrow(readr::problems(x)) > 0) {
    warning("There were problems parsing the CSV data")
    print(readr::problems(x))
    stop("Failed to parse WQ data from CSV")
  }

  # manually parse numeric columns outside read_csv() to handle commas in numbers (e.g., "1,000")
  numeric_cols <- c("sigFigValue")
  x <- dplyr::mutate(x,
    dplyr::across(dplyr::all_of(numeric_cols), readr::parse_number)
  )

  x
}
