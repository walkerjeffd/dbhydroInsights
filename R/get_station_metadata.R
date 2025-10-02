#' Get Station Metadata
#'
#' Retrieves metadata for a specific station.
#'
#' @param station_id A character string for the station ID
#'
#' @return A list containing metadata for the specified timeseries
#' @export
#'
#' @examples
#' get_station_metadata("LOX 10")
#' @importFrom utils URLencode
get_station_metadata <- function(station_id) {
  url <- URLencode(paste0("https://api.sfwmd.gov/v1/insights-data/nearby-stations/", station_id))
  req <- httr2::request(url)
  req <- httr2::req_url_query(req, distance = 1)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  body$nearbyStations <- NULL
  body$monitoringProjects <- NULL
  body
}