#' Get Timeseries Metadata
#'
#' Retrieves metadata for a specific timeseries identified by its database key.
#'
#' @param dbkey A character string with the database key to get metadata for
#'
#' @return A list containing metadata for the specified timeseries
#' @export
#'
#' @examples
#' metadata <- get_timeseries_metadata("91510")
#' str(metadata)
get_timeseries_metadata <- function(dbkey) {
  url <- paste0("https://api.sfwmd.gov/v1/insights-data/dbkeyInfo/", dbkey)
  req <- httr2::request(url)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  body
}
