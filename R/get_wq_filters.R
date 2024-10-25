#' Get Water Quality Filters
#'
#' Retrieves available filters for water quality data queries.
#'
#' @return A named list of filter options
#' @export
#'
#' @examples
#' filters <- get_wq_filters()
#' str(filters)
get_wq_filters <- function() {
  url <- "https://api.sfwmd.gov/v1/insights-data/ui/chem/filters"
  req <- httr2::request(url)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  body
}
