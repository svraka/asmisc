% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelsummary.R
\name{as.modelsummary_list}
\alias{as.modelsummary_list}
\title{Create a \code{modelsummary_list} object from tidy and glance data}
\usage{
as.modelsummary_list(tidy, glance)
}
\arguments{
\item{tidy}{A \code{\link[broom:reexports]{broom::tidy()}} data frame}

\item{glance}{A \code{\link[broom:reexports]{broom::glance()}} data frame}
}
\value{
A \code{modelsummary_list} object, see \href{https://modelsummary.com/vignettes/modelsummary_extension.html}{\pkg{modelsummary}'s Extension and Customization}
vignette.
}
\description{
Create a \code{modelsummary_list} object from a \code{\link[broom:reexports]{broom::tidy()}} and a
\code{\link[broom:reexports]{broom::glance()}} data.frame.
}
