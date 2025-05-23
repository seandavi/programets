#' Retrieve icite data including Relative Citation Ratio (RCR) for PubMed IDs.
#'
#' @param pmids vector of PubMed IDs to retrieve (max of 1000 at a time)
#'
#' @return a tibble including the following variables:
#' \describe{
#' \item{`pmid`}{PubMed ID}
#' \item{`authors`}{publication authors}
#' \item{`citation_count`}{total citations}
#' \item{`citations_per_year`}{mean citations per year}
#' \item{`expected_citations_per_year`}{estimated}
#' \item{`field_citation_rate`}{rate relative to field}
#' \item{`is_research_article`}{boolean}
#' \item{`journal`}{journal name abbr.}
#' \item{`nih_percentile`}{percentile}
#' \item{`relative_citation_ratio`}{RCR}
#' \item{`title`}{article title}
#' \item{`year`}{publication year}
#' }
#'
#' See URL for full details.
#'
#' @import httr2
#' @import jsonlite
#' @import dplyr
#'
#' @seealso \url{https://icite.od.nih.gov/api}
#'
#' @author Sean Davis <seandavi@gmail.com>
#'
#' @examples
#' pmids <- c(26001965, 25015380)
#' icite_records <- icite(pmids)
#' dplyr::glimpse(icite_records)
#'
#'
#' @export
icite <- function(pmids) {
  max_pmids <- 1000
  if (length(pmids) > max_pmids) {
    stop("maximum pubmed ids exceeded", call. = FALSE)
  }

  ## icite url
  req <- httr2::request("https://icite.od.nih.gov")
  resp <- req |>
    httr2::req_url_path_append("api/pubs") |>
    httr2::req_url_query("pmids" = paste0(pmids, collapse = ",")) |>
    httr2::req_perform()

  raw <- resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  dplyr::tibble(raw$data)
}
