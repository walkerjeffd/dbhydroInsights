#' Create a Water Quality Query
#'
#' @param locations Character vector of location names or "ALL"
#' @param location_type Type of location, either "STATION" or "SITE" (default "STATION")
#' @param parameters Character vector of parameter codes or "ALL"
#' @param methods Character vector of method codes or "ALL"
#' @param projects Character vector of project codes or "ALL"
#' @param matrices Character vector of matrix codes or "ALL"
#' @param paramGroups Character vector of parameter group codes or "ALL"
#'
#' @return A list containing the query structure
#' @keywords internal
create_wq_query <- function (locations = "ALL", location_type = "STATION", parameters = "ALL", methods = "ALL", projects = "ALL", matrices = "ALL", paramGroups = "ALL") {
  x <- list(
    parameters = as.list(parameters),
    methods = as.list(methods),
    projects = as.list(projects),
    matrices = as.list(matrices),
    paramGroups = as.list(paramGroups)
  )

  if (length(locations) == 1 && locations == "ALL") {
    x[["locations"]] <- list(
      list(
        name = locations,
        type = "ALL"
      )
    )
  } else {
    x[["locations"]] <- lapply(locations, function (x) {
      list(
        name = x,
        type = location_type
      )
    })
  }

  x
    }

#' Get Water Quality Metadata
#'
#' Retrieves metadata for water quality measurements based on specified filters.
#'
#' @param ... Query filters (locations, parameters, methods, projects, matrices, paramGroups) see `create_wq_query()`
#' @param startDate Start date as string in "YYYYMMDD" format
#' @param endDate End date as string in "YYYYMMDD" format
#' @param offset Integer specifying the number of records to skip
#' @param limit Integer specifying the maximum number of records to return per request
#' @param sleep Number of seconds to pause between paginated requests
#'
#' @importFrom tibble as_tibble
#'
#' @return A tibble containing water quality metadata
#' @export
#'
#' @examples
#' get_wq_metadata(locations = c("G722", "BB52"), parameters = c("18", "25"),
#'                 startDate = "20190101", endDate = "20191231")
get_wq_metadata <- function (..., startDate = NULL, endDate = NULL, offset = 0, limit = 1000, sleep = 1) {
  query <- create_wq_query(...)

  url <- "https://api.sfwmd.gov/v1/insights-data/chem/ts"
  req <- httr2::req_url_query(
    httr2::request(url),
    offset = offset,
    limit = limit,
    startDate = startDate,
    endDate = endDate
  )
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_body_json(req, list(query = query))
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  results <- as_tibble(body$results)
  results[["startDate"]] <- lubridate::mdy(results[["startDate"]])
  results[["startDateTime"]] <- as.POSIXct(results[["startDateTime"]] / 1000)
  results[["endDate"]] <- lubridate::mdy(results[["endDate"]])
  results[["endDateTime"]] <- as.POSIXct(results[["endDateTime"]] / 1000)

  remaining <- body$totalRecords - offset - nrow(results)
  if (remaining > 0) {
    Sys.sleep(sleep)
    next_results <- get_wq_metadata(
      ...,
      startDate = startDate,
      endDate = endDate,
      offset = offset + limit,
      limit = limit,
      sleep = sleep
    )
    results <- dplyr::bind_rows(results, next_results)
  }

  results
}
