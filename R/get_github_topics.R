#' Get GitHub Repositories by Topic
#'
#' @param topics A vector of GitHub topics to search for.
#' @param token A GitHub personal access token. If not provided, the function
#'   will run without authentication.
#' @param limit The maximum number of results to return. Defaults to 30.
#' 
#' @importFrom httr2 request req_url_query req_perform resp_status resp_body_json req_auth_bearer_token
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_dbl
#' @importFrom rlang %||%
#'
#' @return A data frame containing the results of the search query.
#' @export
#'
#' @examples
#' \dontrun{
#' topics <- c("u54od036472")
#' get_github_topics(topics)
#' }

get_github_by_topic <- function(topics, token = NULL, limit = 30) {
  if (length(topics) == 0) {
    stop("At least one topic must be provided.")
  }

  # Construct GitHub search query
  q <- paste(sprintf("topic:%s", topics), collapse = " ")

  # Build request
  req <- request("https://api.github.com/search/repositories") |>
    req_url_query(q = q, per_page = limit)

  if (!is.null(token)) {
    req <- req |> 
      req_auth_bearer_token(token)
  }

  resp <- req_perform(req)

  # Check Response
  if (resp_status(resp) != 200) {
    stop("GitHub API request failed: ", resp_status(resp))
  }

  # Parse JSON response
  content <- resp_body_json(resp, simplifyVector = FALSE)

  # Extract relevant fields
  results <- content$items

  if (length(results) == 0) return(data.frame())

  df <- tibble(
    name = map_chr(results, ~ .x$name),
    owner = map_chr(results, ~ .x$owner$login),
    stars = map_dbl(results, ~ .x$stargazers_count),
    forks = map_dbl(results, ~ .x$forks_count),
    open_issues = map_dbl(results, ~ .x$open_issues_count),
    language = map_chr(results, ~ .x$language %||% NA_character_),
    license = map_chr(results, ~ .x$license$name %||% NA_character_),
    created_at = map_chr(results, ~ .x$created_at),
    pushed_at = map_chr(results, ~ .x$pushed_at),
    updated_at = map_chr(results, ~ .x$updated_at),
    html_url = map_chr(results, ~ .x$html_url)
  )

  return(df)
}

