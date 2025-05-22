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


  # Construct httr2 req
  req_proj <- request("https://api.reporter.nih.gov/v2/projects/search") |>
    req_method("POST") |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      criteria = list(project_nums = core_project_numbers)
    )
  ) 
  resp_proj <- req_proj %>% 
    req_perform() %>% 
    resp_body_json()

  proj_results_tbl <- 
    purrr::map_dfr(resp_proj$results, function(proj_info) {
      dplyr::tibble(
        applid = as.integer(proj_info$appl_id),
        core_project_num = as.character(proj_info$core_project_num),
        subproject_id = as.character(proj_info$subproject_id),
        fiscal_year = as.integer(proj_info$fiscal_year),
        project_num = as.character(proj_info$project_num),
        project_serial_num = as.character(proj_info$project_serial_num),
        organization = list(proj_info$organization),
        award_type = as.character(proj_info$award_type),
        activity_code = as.character(proj_info$activity_code),
        is_active = as.logical(proj_info$is_active),
        project_num_split = list(proj_info$project_num_split),
        principal_investigators = list(proj_info$principal_investigators),
        contact_pi_name = as.character(proj_info$contact_pi_name),
        program_officers = list(proj_info$program_officers),
        agency_ic_admin = list(proj_info$agency_ic_admin),
        agency_ic_fundings = list(proj_info$agency_ic_fundings),
        cong_dist = as.character(proj_info$cong_dist),
        spending_categories = as.character(proj_info$spending_categories %||% NA_character_),
        project_start_date = as.character(proj_info$project_start_date),
        project_end_date = as.character(proj_info$project_end_date),
        organization_type = list(proj_info$organization_type),
        geo_lat_lon = list(proj_info$geo_lat_lon),
        opportunity_number = as.character(proj_info$opportunity_number),
        full_study_section = list(proj_info$full_study_section),
        award_notice_date = as.character(proj_info$award_notice_date),
        is_new = as.logical(proj_info$is_new),
        mechanism_code_dc = as.character(proj_info$mechanism_code_dc),
        terms = as.character(proj_info$terms %||% NA_character_),
        pref_terms = as.character(proj_info$pref_terms %||% NA_character_),
        abstract_text = as.character(proj_info$abstract_text),
        project_title = as.character(proj_info$project_title),
        phr_text = as.character(proj_info$phr_text %||% NA_character_),
        spending_categories_desc = as.character(proj_info$spending_categories %||% NA_character_),
        agency_code = as.character(proj_info$agency_code),
        covid_response = as.character(proj_info$covid_response %||% NA_character_),
        arra_funded = as.character(proj_info$arra_funded),
        budget_start = as.character(proj_info$budget_start),
        budget_end = as.character(proj_info$budget_end),
        cfda_code = as.character(proj_info$cfda_code %||% NA_character_),
        funding_mechanism = as.character(proj_info$funding_mechanism),
        direct_cost_amt = as.integer(proj_info$direct_cost_amt),
        indirect_cost_amt = as.integer(proj_info$indirect_cost_amt),
        project_detail_url = as.character(proj_info$project_detail_url),
        date_added = as.character(proj_info$date_added)
      )
    }
  )
  
  return(all_results)
}


