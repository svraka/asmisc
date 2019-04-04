#' Helper function to select the appropriate readr function
#'
#' Select the appropriate \code{\link[readr]{read_delim_chunked}} function.
#'
#' @param input_delim Single character used to separate fields within a record.
#' @param ... Arguments passed to the \code{\link[readr]{read_delim_chunked}}
#'   functions.
#'
#' @keywords internal
read_chunked <- function(input_delim, ...) {
  if (input_delim == ",") {
    readr::read_csv_chunked(...)
  } else if (input_delim == ";") {
    readr::read_csv2_chunked(...)
  } else if (input_delim == "\t") {
    readr::read_tsv_chunked(...)
  } else {
    readr::read_delim_chunked(delim = input_delim, ...)
  }
}

#' Helper function to list top values of a character vector
#'
#' List the first 10 unique values of a character vector.
#'
#' @param x A character vector
#'
#' @keywords internal
chr_values <- function(x) {
  x %>%
    unique %>% sort %>%
    stringr::str_subset("[^\\d]") %>%
    utils::head(n = 10)
}

#' List data frames and their sizes
#'
#' List data frames in global environment and their sizes in megabytes.
#'
#' @return
#' A tibble, arranged in decreasing order of size.
#'
#' @export
df_sizes <- function() {
  dfs <- names(which(sapply(.GlobalEnv, is.data.frame)))
  sapply(dfs, function(x) utils::object.size(get(x))) %>%
    tibble::enframe(name = "df", value = "size") %>%
    dplyr::arrange(dplyr::desc(.data$size)) %>%
    dplyr::mutate(size = as.integer(round(.data$size / 2024^2, 0)))
}
