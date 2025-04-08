#' Get google analytics data as a dataframe
#'
#' This function retrieves Google Analytics data and returns it as a dataframe.
#'
#' @param property_id The Google Analytics property ID (e.g., "UA-12345678-1").
#' @param start_date The start date for the data retrieval (e.g., "2023-01-01").
#' @param end_date The end date for the data retrieval (e.g., "2023-01-31").
#' @param metrics The metrics to retrieve (e.g., "ga:sessions,ga:pageviews").
#' @param dimensions The dimensions to retrieve (e.g., "ga:date,ga:source").
#' @param filters Optional filters to apply to the
#'   data (e.g., "ga:country==United States").
#' @param sort Optional sorting for the data (e.g., "-ga:sessions").
#'
#' @return A dataframe containing the Google Analytics data.
#'
#' @export
google_analytics_dataframe <- function(
    property_id,
    start_date,
    end_date,
    metrics,
    dimensions,
    filters = NULL,
    sort = NULL) {
  # Authenticate with Google Analytics
  googleAnalyticsR::ga_auth()

  # Retrieve the data
  res <- googleAnalyticsR::ga_data(
    property_id,
    date_range = c(start_date, end_date),
    metrics = metrics,
    dimensions = dimensions,
    filters = filters,
    sort = sort
  )

  return(res)
}
