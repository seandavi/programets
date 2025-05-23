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
#' @importFrom rlang .data abort inform format_error_bullets
#' @importFrom stringr regex str_detect
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
#' @examples
#' # Get publications for a set of core project numbers
#' publications <- get_publications_for_core_projects(c("u54od036472", "99999999"))
#' # View the results
#' publications
get_publications_for_core_projects <- function(core_project_numbers) {
  # Validate input
  if (!is.character(core_project_numbers)) {
    stop("Input must be a character vector")
  }

  # Convert input to uppercase
  core_project_numbers <- toupper(core_project_numbers)

  # Construct httr2 req
  req <- request("https://api.reporter.nih.gov/v2/publications/search") |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      criteria = list(core_project_nums = core_project_numbers),
      include_fields = list( "applid", "coreproject", "pmid")
    )) 
  
  # Attempt to perform req, with error handling
  resp <- tryCatch(
    {
      req |> 
        req_perform() |> 
        resp_body_json()
    },
    error = function(e){
      if(str_detect(as.character(e), pattern = regex('HTTP 400', ignore_case = T)) ) {
        inform(format_error_bullets(c('!' = conditionMessage(e))))
        abort('Please verify at least one of the core projects has been entered correctly and exists.', class = 'api_400')
      } else {
        abort(conditionMessage(e), class = 'api_general')
      }
    }
  )

  # Parse returned publications to a tibble
  results_tbl <- 
    purrr::map_dfr(resp$results, function(pub) {
      dplyr::tibble(
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


