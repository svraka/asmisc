#' Helper function to list top values of a character vector
#'
#' List the first \code{n} unique values that cannot be coerced into
#' numeric of a character vector.
#'
#' @param x A character vector
#' @param n How many values should be reported
#'
#' @export
chr_values <- function(x, n = 5) {
  # Keep non-numeric values
  x <- x[is.na(suppressWarnings(as.numeric((x))))]

  # Grab the first n values
  x <- unique(x)
  x <- sort(x)
  x <- utils::head(x, n = n)

  # skimr does not handle list columns, so we need return a string
  x <- paste0(x, collapse = " | ")

  if (x == "") x <- as.character(NA)

  x
}

#' Check if a vector might be coercible to integer
#'
#' @param x A numeric vector.
#'
#' @return A single logical.
#'
#' @details Take the results from these function with caution, as
#'   floating point precision can affect rounding. They are intended
#'   to be used for deciding what \code{col_type} to choose for a
#'   column in a delimited file when processing it with \pkg{readr}
#'   functions. If the original data is not an integer, \pkg{readr}
#'   will issue parsing errors.
#'
#' @describeIn is_whole Check if vector only has whole values. Return
#'   \code{TRUE} for whole values larger than the largest integer
#'   which can be represented.
#'
#' @export
is_whole <- function(x) {
  all(floor(x) == x, na.rm = TRUE)
}

#' @describeIn is_whole Check if a vector can be coerced to integer.
#' @export
maybe_int <- function(x) {
  x <- stats::na.omit(x)

  all(is_whole(x)) && all(!is.na(suppressWarnings(as.integer(x))))
}

#' @importFrom skimr get_skimmers
#' @export
get_skimmers.character_asmisc <- function(column) {
  skimr::sfl(
    is_num_chr = sjmisc::is_num_chr,
    chr_values = chr_values,
    skim_type  = "character"
  )
}

# skimr treats integers and doubles as numerics. We want different
# skimmers for these types. During the first pass of a CSV parsing
# integers might be read as doubles but we want to check if they can
# be stored as integers.
#' @export
get_skimmers.integer <- function(column) {
  skimr::sfl(
    mean      = ~ mean(., na.rm = TRUE),
    sd        = ~ stats::sd(., na.rm = TRUE),
    p0        = ~ stats::quantile(., probs = 0,    na.rm = TRUE, names = FALSE),
    p1        = ~ stats::quantile(., probs = 0.01, na.rm = TRUE, names = FALSE),
    p25       = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    p50       = ~ stats::quantile(., probs = 0.50, na.rm = TRUE, names = FALSE),
    p75       = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    p99       = ~ stats::quantile(., probs = 0.99, na.rm = TRUE, names = FALSE),
    p100      = ~ stats::quantile(., probs = 1,    na.rm = TRUE, names = FALSE),
    skim_type = "integer"
  )
}

# Numeric skimmers for `codebook()`. Drop histogram and add 1st and
# 99th percentiles. The entire skim function list has to be recreated
# because there's no other way of custom ordering (other than manually
# reordering the `sfl` list but that isn't easier either).
#' @export
get_skimmers.numeric_asmisc <- function(column) {
  skimr::sfl(
    mean      = ~ mean(., na.rm = TRUE),
    sd        = ~ stats::sd(., na.rm = TRUE),
    p0        = ~ stats::quantile(., probs = 0,    na.rm = TRUE, names = FALSE),
    p1        = ~ stats::quantile(., probs = 0.01, na.rm = TRUE, names = FALSE),
    p25       = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    p50       = ~ stats::quantile(., probs = 0.50, na.rm = TRUE, names = FALSE),
    p75       = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    p99       = ~ stats::quantile(., probs = 0.99, na.rm = TRUE, names = FALSE),
    p100      = ~ stats::quantile(., probs = 1,    na.rm = TRUE, names = FALSE),
    is_whole  = is_whole,
    maybe_int = maybe_int,
    skim_type = "numeric"
  )
}

#' Create a codebook from a data frame
#'
#' Create a codebook of a data frame using \code{\link[skimr]{skim}}
#' that can help fine-tuning \code{col_types} and help with simple
#' data cleaning tasks when processing a delimited file using
#' \pkg{readr}.
#'
#' @inheritParams skimr::skim
#'
#' @return A custom \code{\link[skimr]{skim}} output. Unlike
#'   \code{\link[skimr]{skim}}, histograms are not generated but there
#'   are additional skimmers:
#'
#'   \describe{
#'     \item{\code{is_whole} (numeric types)}{Logical, whether a numeric
#'       variable has only whole values.}
#'     \item{\code{\link[sjmisc:is_num_fac]{is_num_chr}} (numeric types)}{
#'       Logical, whether a character column has only numeric values.}
#'     \item{\code{chr_values} (character types)}{A list column with the first
#'       10 unique character values.}
#'   }
#'
#' @export
#'
#' @examples
#' codebook(dplyr::starwars)
codebook <- function(data, ...) {
  skim_codebook <- skimr::skim_with(
    character      = skimr::get_sfl("character_asmisc"),
    integer        = skimr::get_sfl("integer"),
    numeric        = skimr::get_sfl("numeric_asmisc"),
  )

  skim_codebook(data, ...)
}
