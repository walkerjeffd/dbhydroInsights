#' Get Timeseries Data
#'
#' Retrieves timeseries data for specified database keys and date range.
#'
#' @param dbkeys A character vector with the database keys to get timeseries data for
#' @param startDate Start date as string in "YYYY-MM-DD" format
#' @param endDate End date as string in "YYYY-MM-DD" format
#'
#' @importFrom utils unzip
#' @return A tibble containing timeseries data
#' @export
#'
#' @examples
#' get_timeseries_data(dbkeys = "91510", startDate = "2020-01-01", endDate = "2024-01-01")
get_timeseries_data <- function(dbkeys, startDate = NULL, endDate = NULL) {
  startDate <- format(parse_date(startDate), "%Y%m%d")
  endDate <- format(parse_date(endDate), "%Y%m%d")

  if (length(dbkeys) == 0) {
    stop("No dbkeys provided")
  }

  if (is.null(startDate) || is.null(endDate)) {
    stop("startDate and endDate must be provided in YMD format")
  }

  url <- "https://api.sfwmd.gov/v1/insights-data/cont/data"
  req <- httr2::request(url)
  req <- httr2::req_url_query(
    req,
    timeseriesIds = paste(dbkeys, collapse = ","),
    reportType = "timeseries",
    format = "csv",
    startDate = startDate,
    endDate = endDate
  )
  resp <- httr2::req_perform(req)
  zip_file <- tempfile(fileext = ".zip")
  writeBin(httr2::resp_body_raw(resp), zip_file)
  exdir <- file.path(tempdir(), paste0("dbhydroInsights_", format(Sys.time(), "%Y%m%d%H%M%S")))
  unzip(zip_file, exdir = exdir)

  csv_files <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    stop(paste0("No CSV file found in extracted zip (", exdir, ")"))
  } else if (length(csv_files) > 1) {
    stop(paste0("Multiple CSV files found in extracted zip, expected only one (", exdir, ")"))
  }

  csv_file <- csv_files[1]

  readr::read_csv(csv_file, comment = "#", col_types = readr::cols(
    .default = readr::col_character(),
    TIMESTAMP = readr::col_character(), #since timestamp can be by date or by breakpoint datetime, need dynamic way to parse
    VALUE = readr::col_double(),
    REVISION_DATE = readr::col_datetime(format = "%m/%d/%Y %H:%M")
  ))%>%
    dplyr::mutate(TIMESTAMP = dplyr::na_if(TIMESTAMP, "")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      TIMESTAMP = if (stringr::str_detect(TIMESTAMP, "\\d{2}:\\d{2}(:\\d{2})?")) { #checks for if timestamp includes hms
        readr::parse_datetime(TIMESTAMP) #if includes hms, treats as datatime
      } else {
        readr::parse_date(TIMESTAMP) #otherwise treats just as a date
      }
    ) %>%
    dplyr::ungroup()

}
