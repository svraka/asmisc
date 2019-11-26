#' Convert Yes/No values to logical
#'
#' A helper function to convert a two-valued character vector to a
#' logical vector.
#'
#' @param x A character vector.
#' @param yn A character vector of length 2, containing the strings
#'   for "yes" (first element) and "no" (second element). Defaults to
#'   Hungarian shorthand for "igen" and "nem". Matches are case
#'   sensitive.
#' @param na_to_false Convert \code{NA}s to \code{FALSE}?
#'
#' @return A logical vector
#' @export
yesno_to_logical <- function(x,
                             yn = c("I", "N"),
                             na_to_false = FALSE) {
  # Make sure `yn` is sensible
  if (!((length(yn) == 2) & (length(unique(yn)) == 2))) {
    stop("`yn` needs two distinct elements.")
  }

  # Check for `x` values not in `yn`. `sort()` drops NAs, so this
  # suffices.
  x_values <- sort(unique(x))

  if (sjmisc::is_empty(x_values) == FALSE) {
    if (length(x_values) > 2){
      stop("`x` contains more than two distinct non-NA values.")
    }
    if (!all(x_values %in% yn)) {
      stop("`x` contains non-NA values not found in `yn`.")
    }
  }

  # We only want to match complete strings
  yn <- stringr::str_c("^", yn, "$")

  x <- stringr::str_replace(x, yn[1], "TRUE")
  x <- stringr::str_replace(x, yn[2], "FALSE")

  if (na_to_false == TRUE) x <- stringr::str_replace_na(x, "FALSE")

  as.logical(x)
}

#' Convert character mark to logical
#'
#' A helper function to convert a character vector to logical where a single character
#' string represents \code{TRUE}.
#'
#' @param x A character vector.
#' @param mark A string representing \code{TRUE}.
#' @param na_to_false Convert \code{NA}s to \code{FALSE}?
#'
#' @return A logical vector
#' @export
mark_to_logical <- function(x,
                            mark = "X",
                            na_to_false = FALSE) {
  # Make sure `mark` is sensible.
  if (!((length(mark) == 1) & (is.character(mark)))) {
    stop("`mark` must be character vector of length 1.")
  }

  # Check for values in `x`. `sort()` drops NAs, so this suffices.
  x_values <- sort(unique(x))
  morevalues_error <- "`x` contains non-NA values other than `mark`."

  if (sjmisc::is_empty(x_values) == FALSE) {
    if (length(x_values) != 1) {
      stop(morevalues_error)
    }
    if (x_values != mark) {
      stop(morevalues_error)
    }
  }

  # We only want to match complete strings
  mark <- stringr::str_c("^", mark, "$")

  x <- stringr::str_replace(x, mark, "TRUE")

  if (na_to_false == TRUE) x <- stringr::str_replace_na(x, "FALSE")

  as.logical(x)
}

#' Parse a month-day character vector as date
#'
#' A helper function to add a specified year to a month-day-like
#' character and parse as date.
#'
#' @param x A character vector containing month-day-like strings
#' @param year An integer added to \code{x}
#' @param format A format specification passed to
#'   \code{\link[readr]{parse_date}} used to parse a concatenated
#'   string of \code{year} and \code{x}.
#'
#' @return A \code{\link{Date}} vector
#' @export
parse_date_md <- function(x,
                          year,
                          format = "%Y%m%d") {
  x <- stringr::str_c(year, x)
  x <- readr::parse_date(x, format = format)

  x
}

#' Parse date string without century
#'
#' Helper function to convert dates stored as strings containing six
#' digits using \code{\link[lubridate]{parse_date_time2}}.
#'
#' @inheritParams lubridate::parse_date_time2
#' @return A character vector of class \code{\link[base]{Date}}
#' @details \code{cutoff_2000} defaults to 30 which is a convenient
#'   value for my work. Unlike
#'   \code{\link[lubridate]{parse_date_time2}}, this function returns
#'   a date.
#' @export
parse_date_ymd <- function(x, orders = "%y%m%d", cutoff_2000 = 30L) {
  x <- lubridate::parse_date_time2(
    x,
    orders = orders,
    cutoff_2000 = cutoff_2000
  )

  # lubridate::parse_date_time2 returns POSIXct
  x <- as.Date(x)

  x
}
