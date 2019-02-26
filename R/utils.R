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

#' List packages used in a project
#'
#' Find packages loaded in \code{\link{library}}, or \code{\link{require}} calls
#' in a non-package project.
#'
#' @param path Relative, or absolute path to a project.
#'
#' @return
#' A character vector with the names of packges loaded.
#'
#' @details
#' These functions look for \code{\link{library}}, or \code{\link{require}}
#' calls in \code{*.R}, \code{*.Rmd}, \code{*.Rnw}, \code{*.Rtex}, and
#' \code{*.Rrst} files.  \code{needed_project_packages} list all pacakges that
#' are not installed.
#'
#' @export
find_project_packages <- function(path = ".") {
  files <- list.files(
    path = path,
    pattern = ".+\\.R[(md)(nw)(tex)(rst)]*$",
    recursive = TRUE,
    full.names = TRUE
  )

  list_packages <- function(file) {
    readLines(file) %>%
      grep("library|require\\(", ., value = TRUE, perl = TRUE) %>%
      gsub(".*(library|require)\\((.+?)\\).*", "\\2", ., perl = TRUE) %>%
      unique %>% sort
  }

  lapply(files, list_packages) %>%
    unlist %>% unique %>% sort
}

#' @describeIn find_project_packages Only list packages that are not installed.
#' @export
needed_project_packages <- function(path = ".") {
  project_packages <- find_project_packages(path)

  installed_packages <- row.names(utils::installed.packages())

  to_install <- setdiff(project_packages, installed_packages)

  if (identical(to_install, character(0))) {
    message("All packages in the project are installed.")
  } else {
    message("You need to install some packages:")
    to_install
  }
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
