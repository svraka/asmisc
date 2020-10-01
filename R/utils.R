tlmgr_install_svraka_pkgs <- function() {
  pkgs <- readLines(system.file(package = "asmisc", "pkgs-svraka.txt"))
  tinytex::tlmgr_install(pkgs)
}

#' List packages used in a project
#'
#' Find packages loaded in \code{\link{library}}, or \code{\link{require}} calls
#' in a non-package project.
#'
#' @param path Relative, or absolute path to a project.
#' @param not_installed Only list packages that are not installed.
#'
#' @return
#' A character vector with the names of packges loaded.
#'
#' @details
#' This function looks for \code{\link{library}}, or \code{\link{require}}
#' calls in \code{*.R}, \code{*.Rmd}, \code{*.Rnw}, \code{*.Rtex}, and
#' \code{*.Rrst} files.
#'
#' @export
find_project_packages <- function(path = ".", not_installed = FALSE) {
  files <- list.files(
    path = path,
    pattern = ".+\\.R[(md)(nw)(tex)(rst)]*$",
    recursive = TRUE,
    full.names = TRUE
  )

  list_packages <- function(file) {
    x <- readLines(file)
    x <- grep("library|require\\(", x, value = TRUE, perl = TRUE)
    x <- gsub(".*(library|require)\\((.+?)\\).*", "\\2", x, perl = TRUE)
    x <- sort(unique(x))

    x
  }

  project_packages <- lapply(files, list_packages)
  project_packages <- sort(unique(unlist(project_packages)))

  if (not_installed == TRUE) {
    installed_packages <- row.names(utils::installed.packages())

    to_install <- setdiff(project_packages, installed_packages)

    if (identical(to_install, character(0))) {
      message("All packages in the project are installed.")
    } else {
      message("You need to install some packages:")
      to_install
    }
  } else {
    project_packages
  }
}
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
