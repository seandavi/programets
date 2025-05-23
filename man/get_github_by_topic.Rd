% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_github_topics.R
\name{get_github_by_topic}
\alias{get_github_by_topic}
\title{Get GitHub Repositories by Topic}
\usage{
get_github_by_topic(topics, token = NULL, limit = 30)
}
\arguments{
\item{topics}{A vector of GitHub topics to search for.}

\item{token}{A GitHub personal access token. If not provided, the function
will run without authentication.}

\item{limit}{The maximum number of results to return. Defaults to 30.}
}
\value{
A data frame containing the results of the search query.
}
\description{
Get GitHub Repositories by Topic
}
\examples{
topics <- c("u24ca289073")
df = get_github_by_topic(topics, limit = 50)
head(df)
dplyr::glimpse(df)

}
