% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_dataframe.R
\name{ga_dataframe}
\alias{ga_dataframe}
\title{Get google analytics data as a dataframe}
\usage{
ga_dataframe(property_id, start_date, end_date, metrics, dimensions)
}
\arguments{
\item{property_id}{The Google Analytics Property ID (e.g., "2839410283").}

\item{start_date}{The start date for the data retrieval (e.g., "2023-01-01").}

\item{end_date}{The end date for the data retrieval (e.g., "2023-01-31").}

\item{metrics}{The metrics to retrieve (e.g., "ga:sessions,ga:pageviews").}

\item{dimensions}{The dimensions to retrieve (e.g., "ga:date,ga:source").}
}
\value{
A dataframe containing the Google Analytics data.
}
\description{
This function retrieves Google Analytics data and returns it as a dataframe.
}
