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

read_delim_chunked_to_dataset <- function(file,
                                          dataset_base_name,
                                          file_nrow, chunk_size,
                                          processing_function = NULL,
                                          chunk_col_name = "chunk",
                                          chunk_file_name = "data.parquet",
                                          ...) {
  # Prepare directory structure. In order to prevent conflicting
  # chunks, first we clean up everything.
  if (dir.exists(dataset_base_name)) unlink(dataset_base_name, recursive = TRUE)
  dir.create(dataset_base_name)

  chunk_paths <- get_chunk_paths(dataset_base_name, file_nrow,
                                 chunk_size, chunk_col_name,
                                 chunk_file_name)

  # Partitioning directories need be created recursively
  purrr::walk(dirname(chunk_paths), dir.create)

  out <- read_delim_chunked(
    file,
    callback = callback_write_parquet(chunk_paths, chunk_size,
                                      processing_function),
    chunk_size = chunk_size,
    ...
  )

  invisible(out)
}

chunked_hive_partition_names <- function(name = "chunk", n) {
  partitions <- strinr::str_pad(seq_len(length(n)), width = max_nchar,
                                side = "left", pad = "0")
}

get_chunk_paths <- function(dataset_base_name, file_nrow, chunk_size,
                            chunk_col_name = "chunk",
                            chunk_file_name = "data.parquet") {
  chunk_numbers <- seq_len((file_nrow %/% chunk_size) + 1)
  max_nchar <- nchar(as.character(max(chunk_numbers)))
  chunk_numbers <- stringr::str_pad(chunk_numbers, width = max_nchar,
                                    side = "left", pad = "0")

  hive_name <- paste0(chunk_col_name, "=", chunk_numbers)

  file.path(dataset_base_name, hive_name, chunk_file_name)
}

callback_write_parquet <- function(chunk_paths, chunk_size) {
  function(x, pos) {
    chunk_number <- (pos %/% chunk_size) + 1

    if (!is.null(processing_function)) {
      x <- processing_function(x)
    }

    arrow::write_parquet(x, sink = chunk_paths[chunk_number])
  }
}
