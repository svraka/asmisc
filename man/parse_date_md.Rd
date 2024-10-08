% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/converters.R
\name{parse_date_md}
\alias{parse_date_md}
\title{Parse a month-day character vector as date}
\usage{
parse_date_md(x, year, format = "\%Y\%m\%d")
}
\arguments{
\item{x}{A character vector containing month-day-like strings}

\item{year}{An integer added to \code{x}}

\item{format}{A format specification passed to
\code{\link[readr:parse_datetime]{readr::parse_date()}} used to parse a concatenated string of
\code{year} and \code{x}.}
}
\value{
A \code{\link[=Date]{Date()}} vector
}
\description{
A helper function to add a specified year to a month-day-like
character and parse as date.
}
