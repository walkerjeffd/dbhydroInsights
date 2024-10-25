#' Parse Date
#'
#' Converts various date formats to a standardized Date object.
#'
#' @param x A character string, Date object, or NULL
#'
#' @return A Date object or NULL
#' @export
#'
#' @examples
#' parse_date("2021-01-01")
#' parse_date(NULL)
#' parse_date(Sys.Date())
parse_date <- function(x) {
  if (is.null(x)) return(NULL)
  if (lubridate::is.Date(x)) return(x)
  lubridate::as_date(x)
}
