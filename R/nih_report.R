#' Get Publications for Core Projects
#'
#' This function retrieves a list of publications associated with a set of core
#' project numbers from the NIH RePORTER API.
#'
#' @param core_project_numbers A character vector of core project numbers.
#'
#' @importFrom dplyr arrange bind_rows mutate tibble
#' @importFrom purrr map_dfr
#' @importFrom httr2 req_body_json req_headers req_method req_perform resp_body_json request
#' @importFrom rlang .data
#' 
#' @return A tibble with the following columns:
#' \describe{
#'   \item{core_project_number}{The core project number.}
#'   \item{found}{A logical indicating whether any publications were found for this core project number.}
#'   \item{applid}{The application ID associated with the publication.}
#'   \item{pmid}{The PubMed ID associated with the publication.}
#'   \item{pubmed_url}{The URL of the PubMed page for the publication.}
#' }
#' 
#' @export
get_publications_for_core_projects <- function(core_project_numbers) {
  req <- request("https://api.reporter.nih.gov/v2/publications/search") |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      criteria = list(core_project_nums = core_project_numbers),
      include_fields = list( "applid", "coreproject", "pmid")
    )) 
  
  resp <- req |>
    req_perform() |>
    resp_body_json()

  # Parse returned publications to a tibble
  results_tbl <- 
    purrr::map_dfr(resp$results, function(pub) {
      tibble(
        core_project_number = pub$coreproject,
        found = TRUE,
        applid = as.character(pub$applid),
        pmid = as.character(pub$pmid),
        pubmed_url = paste0("https://pubmed.ncbi.nlm.nih.gov/", pub$pmid)
      )
    }
  )

  # Ensure all user input core project numbers are represented
  all_results <- 
    dplyr::bind_rows(
      results_tbl,
      tibble(core_project_number = setdiff(core_project_numbers, results_tbl$core_project_number)) |>
      mutate(
        found = FALSE,
        applid = NA_character_,
        pmid = NA_character_,
        pubmed_url = NA_character_
      )
    ) |>
    arrange(.data$core_project_number)

  return(all_results)
}
