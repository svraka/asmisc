% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrow.R
\name{prepare_dataset_base}
\alias{prepare_dataset_base}
\title{Prepare directory structure for an Arrow dataset}
\usage{
prepare_dataset_base(dataset_base_name, chunk_paths)
}
\arguments{
\item{dataset_base_name}{Path of the directory to write the Hive
partitioned Parquet files to.}
}
\description{
To be used with \code{\link[=read_delim_chunked_to_dataset]{read_delim_chunked_to_dataset()}}.
}
\keyword{internal}
