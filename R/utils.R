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
    readLines(file) %>%
      grep("library|require\\(", ., value = TRUE, perl = TRUE) %>%
      gsub(".*(library|require)\\((.+?)\\).*", "\\2", ., perl = TRUE) %>%
      unique %>% sort
  }

  project_packages <- lapply(files, list_packages) %>%
    unlist %>% unique %>% sort

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
