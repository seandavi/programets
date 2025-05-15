#' Browse google analytics query explorer
#'
#' This function opens the Google Analytics Query Explorer in a web browser.
#'
#' It allows users to explore and test Google Analytics queries interactively.
#' This is useful for developers and analysts who want to experiment with
#' different queries and see the results in real-time.
#'
#' @note This function is intended for users who have a Google Analytics account
#' and want to explore the API capabilities. It does not return any data or
#' perform any analysis. You need to have a Google account with google analytics
#' access and will be prompted to log in if you are not already.
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
  if (interactive()) {
    # Open the Google Analytics API Explorer in a web browser
    utils::browseURL("https://ga-dev-tools.google/ga4/query-explorer/")
  } else {
    # Print a message if not in interactive mode
    message("This function is intended to be used interactively.")
  }
}
