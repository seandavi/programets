#' Google analytics metadata dataframe
#'
#' This function retrieves Google Analytics metadata
#' and returns it as a dataframe. The metadata includes
#' information about metrics, dimensions, and other
#' attributes available in Google Analytics.
#'
#' This function is a wrapper around the
#' `googleAnalyticsR::ga_meta()` function. It retrieves
#' metadata for the Google Analytics API version 4.
#'
#' @note This function requires first authenticating to 
#' Google Analytics using the `ga_auth()` function.
#'
#' @family Google Analytics
#'
#' @examples
#' \dontrun{
#' ga_meta_simple()
#' }
#' 
#' @return A tibble containing Google Analytics metadata.
#'
#' @export
ga_meta_simple <- function() {
  tibble::as_tibble(googleAnalyticsR::ga_meta(version = "data"))
}

#' Browse google analytics query explorer
#'
#' This function opens the Google Analytics Query Explorer in a web browser.
#'
#' It allows users to explore and test Google Analytics queries interactively.
#' This is useful for developers and analysts who want to experiment with
#' different queries and see the results in real-time.
#'
#' @return Opens the Google Analytics Query Explorer in a web browser.
#'
#' @note This function is intended for users who have a Google Analytics account
#' and want to explore the API capabilities. It does not return any data or
#' perform any analysis.
#'
#' @family Google Analytics
#'
#' @examples
#' \dontrun{
#' ga_query_explorer()
#' }
#'
#' @export
ga_query_explorer <- function() {
  # Open the Google Analytics API Explorer in a web browser
  utils::browseURL("https://ga-dev-tools.google/ga4/query-explorer/")
}
