% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook.R
\name{is_whole}
\alias{is_whole}
\alias{maybe_int}
\title{Check if a vector might be coercible to integer}
\usage{
is_whole(x)

maybe_int(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A single logical.
}
\description{
Check if a vector might be coercible to integer
}
\details{
Take the results from these function with caution, as floating
point precision can affect rounding. They are intended to be used
for deciding what \code{col_type} to choose for a column in a delimited
file when processing it with \pkg{readr} functions. If the original
data is not an integer, \pkg{readr} will issue parsing errors.
}
\section{Functions}{
\itemize{
\item \code{is_whole()}: Check if vector only has whole values. Return
\code{TRUE} for whole values larger than the largest integer which can
be represented.

\item \code{maybe_int()}: Check if a vector can be coerced to integer.

}}
