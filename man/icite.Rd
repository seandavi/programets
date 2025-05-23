% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icite.R
\name{icite}
\alias{icite}
\title{Retrieve icite data including Relative Citation Ratio (RCR) for PubMed IDs.}
\usage{
icite(pmids)
}
\arguments{
\item{pmids}{vector of PubMed IDs to retrieve (max of 1000 at a time)}
}
\value{
a tibble including the following variables:
\describe{
\item{\code{pmid}}{PubMed ID}
\item{\code{authors}}{publication authors}
\item{\code{citation_count}}{total citations}
\item{\code{citations_per_year}}{mean citations per year}
\item{\code{expected_citations_per_year}}{estimated}
\item{\code{field_citation_rate}}{rate relative to field}
\item{\code{is_research_article}}{boolean}
\item{\code{journal}}{journal name abbr.}
\item{\code{nih_percentile}}{percentile}
\item{\code{relative_citation_ratio}}{RCR}
\item{\code{title}}{article title}
\item{\code{year}}{publication year}
}

See URL for full details.
}
\description{
Retrieve icite data including Relative Citation Ratio (RCR) for PubMed IDs.
}
\examples{
pmids <- c(26001965, 25015380)
icite_records <- icite(pmids)
dplyr::glimpse(icite_records)


}
\seealso{
\url{https://icite.od.nih.gov/api}
}
\author{
Sean Davis \href{mailto:seandavi@gmail.com}{seandavi@gmail.com}
}
