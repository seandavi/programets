#' Get google analytics data as a dataframe
#'
#' This function retrieves Google Analytics data and returns it as a dataframe.
#'
#' @param property_id The Google Analytics Property ID (e.g., "2839410283").
#' @param start_date The start date for the data retrieval (e.g., "2023-01-01").
#' @param end_date The end date for the data retrieval (e.g., "2023-01-31").
#' @param metrics The metrics to retrieve (e.g., "ga:sessions,ga:pageviews").
#' @param dimensions The dimensions to retrieve (e.g., "ga:date,ga:source").
#'
#' @return A dataframe containing the Google Analytics data.
#'
#' @export
ga_dataframe <- function(
    property_id,
    start_date,
    end_date,
    metrics,
    dimensions
  ) {

  # Retrieve the data
  tibble::as_tibble(googleAnalyticsR::ga_data(
    propertyId = property_id,
    date_range = c(start_date, end_date),
    metrics = metrics,
    dimensions = dimensions
  ))
}
