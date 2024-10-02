#' Convert Yes/No values to logical
#'
#' A helper function to convert a two-valued character vector to a
#' logical vector.
#'
#' @param x A character vector.
#' @param yn A character vector of length 2, containing the strings
#'   for "yes" (first element) and "no" (second element). Defaults to
#'   Hungarian shorthand for "igen" and "nem".
#' @param na_to_false Convert `NA`s to `FALSE`?
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

  # If `x` is all NA we can't do other checks.
  if (length(x_values) > 0) {
    if (length(x_values) > 2){
      stop("`x` contains more than two distinct non-NA values.")
    }
    if (!all(x_values %in% yn)) {
      stop("`x` contains non-NA values not found in `yn`.")
    }
  }

  x[x == yn[1]] <- "TRUE"
  x[x == yn[2]] <- "FALSE"

  if (na_to_false == TRUE) x[is.na(x)] <- "FALSE"

  as.logical(x)
}

#' Convert character mark to logical
#'
#' A helper function to convert a character vector to logical where a
#' single character string represents `TRUE`.
#'
#' @param x A character vector.
#' @param mark A string representing `TRUE`.
#' @param na_to_false Convert `NA`s to `FALSE`?
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

  # If `x` is all NA we can't do other checks.
  if (length(x_values) > 0) {
    if (length(x_values) != 1) {
      stop(morevalues_error)
    }
    if (x_values != mark) {
      stop(morevalues_error)
    }
  }

  x[x == mark] <- "TRUE"

  if (na_to_false == TRUE) x[is.na(x)] <- "FALSE"

  as.logical(x)
}

#' Parse a month-day character vector as date
#'
#' A helper function to add a specified year to a month-day-like
#' character and parse as date.
#'
#' @param x A character vector containing month-day-like strings
#' @param year An integer added to `x`
#' @param format A format specification passed to
#'   [readr::parse_date()] used to parse a concatenated string of
#'   `year` and `x`.
#'
#' @return A [Date()] vector
#' @export
parse_date_md <- function(x,
                          year,
                          format = "%Y%m%d") {
  x <- paste0(year, x)
  x <- readr::parse_date(x, format = format)

  x
}

#' Parse date string without century
#'
#' Helper function to convert dates stored as strings containing six
#' digits in a `yymmdd` format using [lubridate::ymd()].
#'
#' @inheritParams readr::parse_date
#' @inheritParams lubridate::parse_date_time
#' @return A vector of class [base::Date()]
#' @details `cutoff_2000` defaults to 30 which is a convenient value
#'   for my work.
#' @export
parse_date_ymd <- function(x, cutoff_2000 = 30L) {
  # `lubridate::ymd()` works on numeric vectors as well but we ensure
  # a character input so we don't run into problems with dates from
  # the 2000s, i.e leading zeros cut off.
  stopifnot(is.character(x) == TRUE)

  # Construct date string with century
  century_19 <- as.integer(substr(x, 1, 2))
  century_19 <- (century_19 > cutoff_2000)
  year_prefix <- rep("20", times = length(x))
  year_prefix[century_19] <- "19"
  year_prefix[is.na(x)] <- NA
  x <- paste0(year_prefix, x)
  x[is.na(x) | is.na(year_prefix)] <- NA

  x <- lubridate::ymd(x)

  x
}
