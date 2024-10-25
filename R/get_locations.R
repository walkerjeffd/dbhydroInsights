#' Get Locations Table
#'
#' Retrieves a table of locations from the API.
#'
#' @return A data frame containing location information
#' @export
#'
#' @examples
#' locations <- get_locations()
#' head(locations)
get_locations <- function () {
  url <- "https://api.sfwmd.gov/v1/insights-data/lookups/locations"
  req <- httr2::request(url)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  dplyr::bind_rows(body)
}
