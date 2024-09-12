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

#' Check if a character vector can be converted to numeric
#'
#' @param x A character vector
#' @return A single logical
#' @export
is_num_chr <- function(x) {
  stopifnot(is.character(x))

  # According to `?as.numeric`, "Conversion does trim whitespace;
  # non-numeric strings give NA + warning". Thus, first we need to
  # drop `NA`s, then try conversion
  x_num <- suppressWarnings(as.numeric(stats::na.omit(x)))
  !any(is.na(x_num))
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

#' @exportS3Method skimr::get_skimmers
get_skimmers.character_asmisc <- function(column) {
  skimr::sfl(
    is_num_chr = is_num_chr,
    chr_values = chr_values,
    skim_type  = "character"
  )
}

# skimr treats integers and doubles as numerics. We want different
# skimmers for these types. During the first pass of a CSV parsing
# integers might be read as doubles but we want to check if they can
# be stored as integers.
#' @exportS3Method skimr::get_skimmers
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
#' @exportS3Method skimr::get_skimmers
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
    hist      = NULL,
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
#'     \item{\code{integer}}{By default, \pkg{skimr} handles integers
#'       and doubles identically. We separate the two. For integers
#'       we add more quantiles to the default statistics.}
#'     \item{\code{numeric}}{For doubles. In addition to statistics
#'       in \code{integer}, we also check wether the column could be
#'       converted to integer.}
#'     \item{\code{character}}{Extend default statistics with a check
#'       whether the column could be converted to numeric and list the
#'       first couple of unique values.}
#'   }
#'
#' @export
#'
#' @examples
#' codebook(dplyr::starwars)
codebook <- function(data, ...) {
  stopifnot(requireNamespace("skimr", quietly = TRUE))

  skim_codebook <- skimr::skim_with(
    character      = skimr::get_sfl("character_asmisc"),
    integer        = skimr::get_sfl("integer"),
    numeric        = skimr::get_sfl("numeric_asmisc")
  )

  res <- skim_codebook(data, ...)

  # We are extending the default skimmers, thus computing default
  # stats and our additions (and omit those set to `NULL`). The
  # defaults contain stats named `p(0|25|50|75|100)`, which will
  # retain their column position, and new ones are added to the end.
  # To keep all quantiles together, we modify the results.
  su <- attr(res, "skimmers_used")
  if ("numeric" %in% names(su)) {
    r1 <- "^numeric\\.p\\d+$"
    ci <- min(grep(r1, colnames(res))) - 1
    cn1 <- paste0("numeric.p", c("0", "1", "25", "50", "75", "99", "100"))
    res <- dplyr::relocate(res, dplyr::all_of(cn1), .after = ci)

    # Skimmed results seem to work just fine by modifying column order
    # but to make sure everything works, we also modify attributes.
    r2 <- "^numeric\\."
    cn2 <- grep(r2, names(res), value = TRUE)
    cn2 <- gsub(r2, "", cn2)
    su[["numeric"]] <- cn2
    attr(res, "skimmers_used") <- su
  }

  res
}
