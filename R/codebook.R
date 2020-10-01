#' Helper function to list top values of a character vector
#'
#' List the first \code{n} unique values that cannot be coerced into
#' numeric of a character vector.
#'
#' @param x A character vector
#' @param n How many values should be reported
#'
#' @keywords internal
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

#' Check whether a vector has only whole values
#'
#' @param x A numeric vector
#'
#' @return A single logical
is_whole <- function(x) {
  all(floor(x) == x, na.rm = TRUE)
}

#' Check whether a vector can be coerced to integer
#'
#' @param x A numeric vector
#'
#' @return A single logical value
maybe_int <- function(x) {
  x <- stats::na.omit(x)

  all(is_whole(x)) && all(!is.na(suppressWarnings(as.integer(x))))
}

# Define custom skim function lists. The structure of this R file is a
# bit unconventional but first we have to define custom functions,
# than skim function list because these objects are needed by the main
# skim function

# Character skimmers for `codebook()`. The custom skimmers can be
# added at the end so we don't have to rebuild all skimmers.
codebook_sfl_character <- skimr::sfl(
  is_num_chr = sjmisc::is_num_chr,
  chr_values = chr_values,
  skim_type  = "character"
)

# skimr treats integers and doubles as numerics. We want different
# skimmers for these types. During the first pass of a CSV parsing
# integers might be read as doubles but we want to check if they can
# be stored as integers.
codebook_sfl_integer <- skimr::sfl(
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

# Numeric skimmers for `codebook()`. Drop histogram and add 1st and
# 99th percentiles. The entire skim function list has to be recreated
# because there's no other way of custom ordering (other than manually
# reordering the `sfl` list but that isn't easier either).
codebook_sfl_numeric <- skimr::sfl(
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

#' Create a codebook from a data frame
#'
#' Create a codebook of a data frame using \code{\link[skimr]{skim}}.
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
  # `skimr::skim_with` needs a bit of nudging. It would work by simply
  # exporting a customized skimming function generated with
  # `skimr::skim_with()` but then R CMD check would issue warnings
  # about undeclared imports without any further specifics. Here we
  # define the skimming function and apply that on our data. This hack
  # passes R CMD check. Idea came from the **mdsr** package
  # <https://github.com/beanumber/mdsr>,
  # <https://cran.r-project.org/package=mdsr>.
  skim_codebook <- skimr::skim_with(
    # Grab the default skimmers for the types where we're not
    # customizing anything. Defining a custom skim function is normally
    # easier but we want to have the percentiles in proper order for
    # numeric types and that requires a complete rewriting because . And
    # with that, we need to use `append = FALSE` and rebuild all types
    AsIs      = skimr::get_sfl("AsIs"),
    character = codebook_sfl_character,
    complex   = skimr::get_sfl("complex"),
    Date      = skimr::get_sfl("Date"),
    difftime  = skimr::get_sfl("difftime"),
    factor    = skimr::get_sfl("factor"),
    integer   = codebook_sfl_integer,
    list      = skimr::get_sfl("list"),
    logical   = skimr::get_sfl("logical"),
    numeric   = codebook_sfl_numeric,
    POSIXct   = skimr::get_sfl("POSIXct"),
    Timespan  = skimr::get_sfl("Timespan"),
    ts        = skimr::get_sfl("ts"),
    append    = FALSE
  )

  skim_codebook(data, ...)
}
