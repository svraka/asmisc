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

#' Read a delimited file by chunks and write into Hive-style Parquet files
#'
#' Read a single delimited file in chunks using
#' \code{\link[readr]{read_delim_chunked}} and save chunks in Parquet
#' files under a simple Hive-style partitioned directory (i.e.
#' \code{dataset_base_name/chunk=XX/data.parquet}) to be used as the
#' source of a multi-file Apache Arrow dataset.
#'
#' @inheritParams readr::read_delim_chunked
#' @param dataset_base_name Path of the directory to write the Hive
#'   partitioned Parquet files to.
#' @param file_nrow Number of data rows in \code{file}. As there is no
#'   reliable and cross-platform way to get the exact number of lines
#'   in a compressed file, this has to be set manually to calculate
#'   the number of chunks and the names of partitions. Use \code{wc}
#'   on a Unix-like system to determine row count (\code{zcat file.gz
#'   | wc -l}, or similar). Only count rows considered as data,
#'   otherwise the dataset's partitioning scheme will have empty
#'   directories. This does not result in errors but it is undesirable
#'   for human-readability. Subtract from the row count any header
#'   row(s), or the number of lines skipped with the \code{skip}
#'   (again, \code{zcat file.gz | head}, or similar can be useful).
#' @param processing_function A function that takes each chunk and
#'   does arbitrary data processing on it before writing the resulting
#'   data frame into its Parquet partition.
#' @param chunk_col_name Name of the column indicating partition
#'   numbers in the Hive-style partition structure.
#' @param chunk_file_name Name of the individual Parquet files in the
#'   Hive-style partition structure.
#' @param ... Passed to \code{\link[readr]{read_delim_chunked}}
#'
#' @details The main goal of this function is to read a single, large,
#'   unpartitioned delimited file into a partitioned Arrow dataset on
#'   a RAM limited machine. Therefore these Arrow partitions have no
#'   inherent meaning. Although \code{processing_function} allows
#'   flexible changes during reading in, this function was intended to
#'   be used in workflows where only minimal data processing is done
#'   and the original structure of the delimited files is kept
#'   unchanged. Thus \code{read_delim_chunked_to_dataset} will create
#'   a partitioning that keeps the original row order from the
#'   delimited file. However, within partition ordering can be changed
#'   through \code{processing_function}.
#'
#' @return Invisibly return a tibble with parsing problems caught by
#'   \pkg{readr} (see \code{\link[readr]{problems}}). \code{NULL} if
#'   no parsing problems occurred.
#'
#' @seealso \code{vignette(topic = "dataset", package = "arrow")} on
#' how to use mult-file Apache Arrow datasets.
#'
#' @export
read_delim_chunked_to_dataset <- function(file,
                                          dataset_base_name,
                                          file_nrow, chunk_size,
                                          processing_function = NULL,
                                          chunk_col_name = "chunk",
                                          chunk_file_name = "data.parquet",
                                          ...) {
  chunk_paths <- get_chunk_paths(dataset_base_name, file_nrow,
                                 chunk_size, chunk_col_name,
                                 chunk_file_name)

  prepare_dataset_base(dataset_base_name, chunk_paths)

  out <- readr::read_delim_chunked(
    file,
    callback = readr::DataFrameCallback$new(
      callback_write_parquet(chunk_paths, chunk_size, processing_function)
    ),
    chunk_size = chunk_size,
    ...
  )

  warn_problems(out)
}

#' @describeIn read_delim_chunked_to_dataset
#'
#' @param df A data frame
#'
#' @export
write_single_partition_dataset <- function(df, dataset_base_name,
                                           chunk_col_name = "chunk",
                                           chunk_file_name = "data.parquet") {
  chunk_paths <- get_chunk_paths(dataset_base_name, nrow(df),
                                 nrow(df), chunk_col_name,
                                 chunk_file_name)

  prepare_dataset_base(dataset_base_name, chunk_paths)

  arrow::write_parquet(df, chunk_paths)
}

#' Create Hive-style partition paths
#'
#' To be used with \code{\link{read_delim_chunked_to_dataset}}.
#'
#' @inheritParams read_delim_chunked_to_dataset
#'
#' @return A character vector with the paths to the partitions.
#' @keywords internal
get_chunk_paths <- function(dataset_base_name, file_nrow, chunk_size,
                            chunk_col_name = "chunk",
                            chunk_file_name = "data.parquet") {
  chunk_numbers <- seq_len(ceiling(file_nrow / chunk_size))

  # Pad partition numbers with zeros to keep the dataset's ordering
  # after reading with back `open_dataset()`
  max_nchar <- nchar(as.character(max(chunk_numbers)))
  fmt <- sprintf("%%0.%si", max_nchar)
  chunk_numbers <- sprintf(fmt, chunk_numbers)

  hive_name <- paste0(chunk_col_name, "=", chunk_numbers)

  file.path(dataset_base_name, hive_name, chunk_file_name)
}

#' Prepare directory structure for an Arrow dataset
#'
#' To be used with \code{\link{read_delim_chunked_to_dataset}}.
#'
#' @inheritParams read_delim_chunked_to_dataset
#' @keywords internal
prepare_dataset_base <- function(dataset_base_name, chunk_paths) {
  # In order to prevent conflicting chunks, first we clean up
  # everything.
  if (dir.exists(dataset_base_name)) unlink(dataset_base_name, recursive = TRUE)
  dir.create(dataset_base_name)

  # Partitioning directories need be created recursively.
  x <- dirname(chunk_paths)
  lapply(x, dir.create)
  invisible(x)
}

#' Callback function to write Parquet partition
#'
#' A function factory that creates a callback function for
#' \code{\link[readr]{read_delim_chunked}}.
#'
#' @inheritParams read_delim_chunked_to_dataset
#'
#' @return A function to be used in
#'   \code{\link{read_delim_chunked_to_dataset}}.
#'
#' @seealso \url{https://stackoverflow.com/a/49241426}
#' @keywords internal
callback_write_parquet <- function(chunk_paths, chunk_size,
                                   processing_function = NULL) {
  function(x, pos) {
    chunk_number <- (pos %/% chunk_size) + 1

    problems <- readr::problems(x)

    if (!is.null(processing_function)) {
      x <- processing_function(x)
    }

    arrow::write_parquet(x, sink = chunk_paths[chunk_number])

    return(problems)
  }
}

#' Report parsing failures
#'
#' Helper function to return parsing failures caught by \pkg{readr} in
#' \code{\link{read_delim_chunked_to_dataset}}. Idea taken from an
#' unexported function in \pkg{readr} (\code{warn_problems}) but
#' implementation is much simplified here.
#'
#' @param x A data frame
#' @keywords internal
warn_problems <- function(x) {
  n_problems <- nrow(x)

  if (n_problems != 0) {
    warning(
      n_problems, " parsing failure", if (n_problems > 1) "s", ".\n",
      paste(format(x), collapse = "\n"),
      call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
    )

    invisible(x)
  } else {
    invisible(NULL)
  }
}
