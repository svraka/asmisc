#' Query metadata of a Parquet file
#'
#' Query column names and column types of an Apache Parquet data file.
#'
#' @param file Path to a Parquet file
#'
#' @return
#' A data frame with two columns:
#'
#' \describe{
#'   \item{\code{name}}{Column names}
#'   \item{\code{type}}{Column types, as returned by the
#'   \code{\link[arrow:ParquetFileReader]{ParquetFileReader$GetSchema()}}
#'   method. This does not directly correspond to navtive R data types
#'   but should be informative nevertheless.}
#' }
#'
#' If the \pkg{tibble} package is available, the data frame is
#' converted into a tibble.
#'
#' @author András Svraka
#'
#' @seealso \code{\link[arrow:write_parquet]{write_parquet}},
#'   \code{\link[arrow:read_parquet]{read_parquet}}
#'
#' @examples
#' tmp <- tempfile()
#' arrow::write_parquet(iris, tmp)
#' metadata_parquet(tmp)
#'
#' @export
metadata_parquet <- function(file) {
  pq <- arrow::ParquetFileReader$create(file)

  pq_schema <- pq$GetSchema()
  pq_names <- pq_schema$names
  pq_types <- sapply(pq_schema$fields, function(x) x$type$ToString())

  df <- data.frame(name = pq_names, type = pq_types, stringsAsFactors = FALSE)

  if (requireNamespace("tibble")) df <- tibble::as_tibble(df)

  df
}
