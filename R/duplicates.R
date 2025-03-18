#' Inspect duplicate rows
#'
#' Inspect duplicate rows in a data frame by sets of columns.
#'
#' @param x A data frame, or a data frame extension, like a
#'   [tibble::tibble()], or a [data.table::data.table()].
#' @param by_list A list, where each element is a character vector
#'   with columns names from `x`, indicating which combinations of
#'   columns from `x` to use for duplicate checks.
#' @param check_all If `TRUE`, include number of duplicates by all
#'   columns in `x`.
#'
#' @return
#'
#' A `data.frame` with columns `by` (list), `N_unique` and
#' `N_duplicated` (integers), where each row corresponds to an element
#' of `by_list`. If `check_all == TRUE`, we add a row to the bottom,
#' where the value of `by` is `NULL`.
#'
#' @details
#'
#' If `x` is a `data.table`, we use \pkg{data.table}'s optimized
#' `data.table::uniqueN()`, otherwise we use `dplyr::distinct()` to
#' calculate the number of duplicates.
#'
#' We use list columns in the results, with typically short lists,
#' which in general can be printed nicely. As tibbles hide elements of
#' list columns, we return a `data.frame`.
#'
#' @export
duplicates <- function(x, by_list, check_all = FALSE) {
  if (check_all) by_list <- c(by_list, list(NULL))

  # Constructing list columns with data.frames is difficult, so we use
  # `tibble::tibble` and convert it to data.frame.
  res <- lapply(by_list,
                function(l) tibble::tibble(by = list(l),
                                           N_unique = .n_unique(x, l)))
  res <- do.call("rbind", res)
  res[["N_duplicated"]] <- nrow(x) - res[["N_unique"]]
  res <- as.data.frame(res)

  res
}

.n_unique <- function(x, by) {
  UseMethod(".n_unique")
}

.n_unique.default <- function(x, by) {
  if (is.null(by)) {
    res <- dplyr::distinct(.data = x, dplyr::pick(dplyr::everything()))
  } else {
    res <- dplyr::distinct(.data = x, dplyr::pick(dplyr::all_of(by)))
  }
  res <- nrow(res)
  res
}

.n_unique.data.table <- function(x, by) {
  data.table::uniqueN(x = x, by = by)
}
