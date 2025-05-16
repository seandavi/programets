# filepath: /Users/davsean/Documents/git/programets/R/epmc_fts.R
#' Search Europe PMC for publications
#' 
#' This function queries the Europe PMC REST API using a provided search string.
#' It handles pagination using a cursor and returns a tibble of search results.
#' 
#' @param query A character string representing the search query for Europe PMC.
#'   See \url{https://europepmc.org/Help} for query syntax.
#' @param page_limit An integer specifying the maximum number of pages to retrieve.
#'   Defaults to 10. Set to `Inf` to retrieve all pages (use with caution).
#' 
#' @return A tibble where each row represents a publication. Columns include:
#'   \describe{
#'     \item{`id`}{Publication ID}
#'     \item{`source`}{Source of the publication (e.g., "MED", "PMC")}
#'     \item{`pmid`}{PubMed ID}
#'     \item{`pmcid`}{PubMed Central ID}
#'     \item{`doi`}{Digital Object Identifier}
#'     \item{`title`}{Title of the publication}
#'     \item{`authorString`}{String of authors}
#'     \item{`journalTitle`}{Title of the journal}
#'     \item{`issue`}{Journal issue}
#'     \item{`journalVolume`}{Journal volume}
#'     \item{`pubYear`}{Year of publication}
#'     \item{`journalIssn`}{Journal ISSN}
#'     \item{`pageInfo`}{Page information}
#'     \item{`pubType`}{Type of publication (e.g., "journal article", "review")}
#'     \item{`isOpenAccess`}{Whether the publication is open access ("OA" or "N/A")}
#'     \item{`inEPMC`}{Whether the publication is in EPMC ("Y" or "N/A")}
#'     \item{`inPMC`}{Whether the publication is in PMC ("Y" or "N/A")}
#'     \item{`hasPDF`}{Whether a PDF is available ("Y" or "N/A")}
#'     \item{`hasBook`}{Whether it is a book ("Y" or "N/A")}
#'     \item{`citedByCount`}{Number of citations}
#'     \item{`hasReferences`}{Whether references are available ("Y" or "N/A")}
#'     \item{`hasTextMinedTerms`}{Whether text-mined terms are available ("Y" or "N/A")}
#'     \item{`hasDbCrossReferences`}{Whether database cross-references are available ("Y" or "N/A")}
#'     \item{`hasLabsLinks`}{Whether Labs links are available ("Y" or "N/A")}
#'     \item{`hasTMAccessionNumbers`}{Whether TM accession numbers are available ("Y" or "N/A")}
#'     \item{`firstPublicationDate`}{Date of first publication}
#'   }
#' 
#' @importFrom httr2 request req_url_query req_retry req_perform req_error resp_body_json resp_status
#' @importFrom dplyr as_tibble bind_rows
#' @importFrom purrr map_dfr
#' @importFrom rlang abort inform
#' @importFrom jsonlite fromJSON
#' 
#' @examples
#' \dontrun{
#'   # Search for publications related to "crispr"
#'   crispr_results <- epmc_search(query = "crispr")
#'   dplyr::glimpse(crispr_results)
#' 
#'   # Search for publications by a specific author
#'   author_results <- epmc_search(query = "AUTH:\"Smith J\"")
#'   dplyr::glimpse(author_results)
#' }
#' @export
epmc_search <- function(query, page_limit = 10) {
  if (!is.character(query) || length(query) != 1 || nchar(trimws(query)) == 0) {
    rlang::abort("`query` must be a non-empty character string.")
  }
  if (!is.numeric(page_limit) || page_limit <= 0) {
    rlang::abort("`page_limit` must be a positive number.")
  }

  base_url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
  cursor_mark <- "*" # Initial cursor mark for the first page
  all_results <- list()
  page_count <- 0

  while (page_count < page_limit) {
    page_count <- page_count + 1
    rlang::inform(paste("Fetching page", page_count, "for query:", query))

    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        query = query,
        format = "json",
        cursorMark = cursor_mark,
        pageSize = 1000 # Max page size allowed by API
      ) |>
      httr2::req_retry(max_tries = 3) |> # Retry on transient errors
      httr2::req_error(body = function(resp) httr2::resp_body_json(resp, check_type = FALSE))


    resp <- tryCatch(
      {
        httr2::req_perform(req)
      },
      httr2_http_400 = function(cnd) {
        error_body <- cnd$body
        error_message <- "Europe PMC API request failed (HTTP 400 - Bad Request)."
        if (!is.null(error_body) && !is.null(error_body$error)) {
          error_message <- paste(error_message, "Details:", error_body$error)
        }
        rlang::abort(error_message, parent = cnd)
      },
      httr2_http_error = function(cnd) { # Catch other HTTP errors
        error_message <- paste("Europe PMC API request failed with HTTP status:", httr2::resp_status(cnd$resp))
        if (!is.null(cnd$body) && is.character(cnd$body)) {
           error_message <- paste(error_message, "Body:", cnd$body)
        } else if (!is.null(cnd$body) && !is.null(cnd$body$error)) {
           error_message <- paste(error_message, "Details:", cnd$body$error)
        }
        rlang::abort(error_message, parent = cnd)
      },
      error = function(e) { # Catch non-HTTP errors from req_perform
        rlang::abort(paste("An error occurred during the API request:", conditionMessage(e)), parent = e)
      }
    )

    if (httr2::resp_status(resp) >= 300) {
      # This case should ideally be caught by req_error or specific httr2_http_errors
      # but as a fallback:
      error_content <- httr2::resp_body_string(resp)
      rlang::abort(paste(
        "Europe PMC API request failed with status",
        httr2::resp_status(resp),
        ". Response:",
        error_content
      ))
    }

    resp_json <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE),
      error = function(e) {
        rlang::abort(paste("Failed to parse JSON response:", conditionMessage(e)), parent = e)
      }
    )
    
    if (is.null(resp_json$resultList) || is.null(resp_json$resultList$result) || length(resp_json$resultList$result) == 0) {
      if (page_count == 1) {
        rlang::inform("No results found for the query.")
      } else {
        rlang::inform("No more results found.")
      }
      break
    }

    results_df <- dplyr::as_tibble(resp_json$resultList$result)
    all_results[[page_count]] <- results_df

    if (is.null(resp_json$nextCursorMark) || resp_json$nextCursorMark == cursor_mark) {
      rlang::inform("All results retrieved.")
      break 
    }
    cursor_mark <- resp_json$nextCursorMark

    # Add a small delay to be polite to the API
    Sys.sleep(delay)
  }

  if (length(all_results) == 0) {
    # Return an empty tibble with expected columns if no results at all
    # This helps maintain a consistent return type.
    # Define expected columns based on a typical successful response structure.
    # This list might need adjustment based on the API's variability.
    expected_cols <- c("id", "source", "pmid", "pmcid", "doi", "title", "authorString", 
                       "journalTitle", "issue", "journalVolume", "pubYear", "journalIssn", 
                       "pageInfo", "pubType", "isOpenAccess", "inEPMC", "inPMC", 
                       "hasPDF", "hasBook", "citedByCount", "hasReferences", 
                       "hasTextMinedTerms", "hasDbCrossReferences", "hasLabsLinks", 
                       "hasTMAccessionNumbers", "firstPublicationDate")
    # Create an empty tibble with these columns
    # The types are mostly character, adjust if specific types are critical for an empty df
    empty_df_structure <- stats::setNames(lapply(expected_cols, function(x) character()), expected_cols)
    return(dplyr::as_tibble(empty_df_structure))
  }
  
  # Combine all pages into a single tibble
  # Use purrr::map_dfr for robust row-binding, handling potential missing columns
  # by filling with NA.
  # First, ensure all dataframes have the same columns as the first one, adding missing ones with NA
  
  # Get all unique column names from all pages
  all_names <- unique(unlist(lapply(all_results, names)))
  
  # Standardize columns in each dataframe
  standardized_results <- lapply(all_results, function(df) {
    missing_cols <- setdiff(all_names, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA 
    }
    df[all_names] # Reorder columns to be consistent
  })
  
  final_results <- dplyr::bind_rows(standardized_results)

  return(final_results)
}

