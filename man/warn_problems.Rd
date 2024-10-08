% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrow.R
\name{warn_problems}
\alias{warn_problems}
\title{Report parsing failures}
\usage{
warn_problems(x)
}
\arguments{
\item{x}{A data frame}
}
\description{
Helper function to return parsing failures caught by \pkg{readr} in
\code{\link[=read_delim_chunked_to_dataset]{read_delim_chunked_to_dataset()}}. Idea taken from an unexported
function in \pkg{readr} (\code{warn_problems}) but implementation is
much simplified here.
}
\keyword{internal}
