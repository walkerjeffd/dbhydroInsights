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
  #csv_file <- file.path(exdir, paste0("sfwmd-data-",format(Sys.time(),"%Y%m%d%H%M"),".csv")) #this works, but if it takes more than a minute to load, the formatting could fail, so using something more robust
  #if (!file.exists(csv_file)) {
    #stop(paste0("Data file not found after extracting zip file (", csv_file, ")"))
  csv_files <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    stop(paste0("No CSV file found in extracted zip (", exdir, ")")) #if zip empty, return message
  }

  csv_file <- csv_files[1] #grab the first csv file...only expecting one anyway, more robust to avoid the timestamps or SFWMD csv file nomenclature changes


  readr::read_csv(csv_file, comment = "#", col_types = readr::cols(
    .default = readr::col_character(),
    TIMESTAMP = readr::col_date(format = "%Y-%m-%d"),
    VALUE = readr::col_double(),
    REVISION_DATE = readr::col_datetime(format = "%m/%d/%Y %H:%M")
  ))
}
