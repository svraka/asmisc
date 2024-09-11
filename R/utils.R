#' Pretty print date in several languages
#'
#' Format dates according to the conventions of several languages.
#'
#' @inheritParams lubridate::year
#' @param locale Language to use.
#'
#' @details Currently only UK and US English, and Hungarian are
#'   supported.
#'
#' @export
#'
#' @examples
#' locales <- c("en_GB", "en_US", "hu_HU")
#' sapply(locales, function(x) pretty_print_date(Sys.Date(), x))
pretty_print_date <- function(x, locale = c("en_GB", "en_US", "hu_HU")) {
  locale <- match.arg(locale)

  language <- switch(
    locale,
    en_GB = "en",
    en_US = "en",
    hu_HU = "hu"
  )

  year <- lubridate::year(x)
  month <- lubridate::month(x)
  day <- lubridate::day(x)

  month_names <- readr::locale(language)$date_names$mon
  month_name <- month_names[month]

  switch(
    locale,
    en_GB = sprintf("%s %s %s", day, month_name, year),
    en_US = sprintf("%s %s, %s", month_name, day, year),
    hu_HU = sprintf("%s. %s %s.", year, month_name, day)
  )
}
