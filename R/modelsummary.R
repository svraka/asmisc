#' Create a `modelsummary_list` object from tidy and glance data
#'
#' Create a `modelsummary_list` object from a [broom::tidy()] and a
#' [broom::glance()] data.frame.
#'
#' @param tidy A [broom::tidy()] data frame
#' @param glance A [broom::glance()] data frame
#'
#' @return A `modelsummary_list` object, see [\pkg{modelsummary}'s
#'   Extension and
#'   Customization](https://modelsummary.com/vignettes/modelsummary_extension.html)
#'   vignette.
#'
#' @export
as.modelsummary_list <- function(tidy, glance) {
  stopifnot(inherits(tidy, "data.frame"),
            inherits(glance, "data.frame"),
            nrow(tidy) >= 1,
            nrow(glance) == 1)

  ret <- list(tidy = tidy, glance = glance)
  class(ret) <- "modelsummary_list"
  ret
}

#' Create a custom `modelsummary_list`
#'
#' Create a customized `modelsummary_list` from a model object.
#'
#' @param x A model object
#' @param ... Passed on to [broom::tidy()] and [broom::tidy()]
#'
#' @inherit as.modelsummary_list return
#'
#' @details
#'
#' Motivation for this generic comes from the need to save
#' \pkg{fixest} models. Saving models even with `lean = TRUE` can
#' result in hard to serialize objects, as \pkg{fixest} stores calls
#' and environments which are difficult to serialise.
#'
#' A `modelsummary_list` object made with `modelsummary(models, output
#' = "modelsummary_list")` does most of what we want, except for
#' adding confindence intervals. \pkg{modelsummary} can add confidence
#' intervals to tables but want to store it here, so that it is
#' available for any other use, including plotting. Therefore we use
#' [broom::tidy()] to obtain information on parameters. For the
#' "glance" part we use [broom::tidy()] in the default method but we
#' use data obtained from \pkg{modelsummary}'s custom internal glance
#' implementation, as it provides useful additions for \pkg{fixest}
#' models (like adding fixed effects, which we also extend via
#' \pkg{modelsummary}'s customization interface, see
#' [glance_custom.fixest()], or proper information on the types of
#' standard errors).
#'
#' The drawback of this approach is that we can't use some
#' \pkg{modelsummary} features, like [reference
#' categories](https://modelsummary.com/vignettes/modelsummary.html#reference-categories)
#' and some computations are run twice. This shouldn't be a major
#' issue for most models and the extra features can be easily
#' replicated by modifying the `modelsummary_list` object.
#'
#' @export
as.modelsummary_list_custom <- function(x, ...) {
  UseMethod("as.modelsummary_list_custom")
}

#' @rdname as.modelsummary_list_custom
#' @export
as.modelsummary_list_custom.default <- function(x, ...) {
  tidy <- broom::tidy(x, ...)
  glance <- broom::glance(x, ...)
  ret <- as.modelsummary_list(tidy, glance)
  ret
}

#' @param conf.int Logical indicating whether or not to include a
#'   confidence interval in the tidied output. Defaults to `TRUE`.
#'
#' @rdname as.modelsummary_list_custom
#' @export
as.modelsummary_list_custom.fixest <- function(x, conf.int = TRUE, ...) {
  # By default modelsummary runs a broom and an easystats extraction
  # and combines their results. Some steps used by the easystats
  # extraction methods in `parameters::model_parameters()` and
  # `performance::model_parameters()` can be very slow for fixest
  # models estimated on large datasets and lots of parameters (AFAIU
  # mainly because it tries to deparse calls and environments). None
  # of the extra information from easystats makes much difference, so
  # we skip it entirely.
  oldopt <- getOption("modelsummary_get")
  on.exit(options(modelsummary_get = oldopt), add = TRUE)
  options(modelsummary_get = "broom")
  tidy <- broom::tidy(x, conf.int = conf.int, ...)
  ms <- modelsummary::modelsummary(x, output = "modelsummary_list", ...)
  glance <- tibble::as_tibble(ms[["glance"]])

  ret <- as.modelsummary_list(tidy, glance)
  ret
}

#' Unnest a `modelsummary_list` column
#'
#' Unnest a nested data frame with a `modelsummary_list` column by
#' adding `tidy` and `glance` columns.
#'
#' @param data A data.frame
#' @param col Name of the `modelsummary_list` column, a character
#'   vector of length one.
#' @param remove If ‘TRUE’, remove input column from output data
#'   frame.
#'
#' @return The original data.frame with two added columns, optionally
#'   the input column removed.
#'
#' @details
#'
#' A `modelsummary_list` object can be directly used to print
#' regression tables but needs to be unnested for further processing,
#' e.g. for plotting from the "tidy" data.frame.
#' [tidyr::unnest_wider()] doesn't work for objects of
#' this type, and it cannot be extended with custom methods. This
#' function reimplements unnesting in a simple case.
#'
#' @export
unnest_modelsummary_list <- function(data, col = "model", remove = TRUE) {
  stopifnot(length(col) == 1,
            col %in% names(data),
            all(sapply(data[[col]],
                       function(x) inherits(x, "modelsummary_list"))))

  out_tidy <- paste0(col, "_tidy")
  out_glance <- paste0(col, "_glance")
  stopifnot(!(c(out_tidy, out_glance) %in% names(data)))

  out <- data
  out[[out_tidy]]   <- lapply(out[[col]], broom::tidy)
  out[[out_glance]] <- lapply(out[[col]], broom::glance)

  if (remove == TRUE) {
    cols <- names(out)
    cols <- cols[cols != col]
    out <- out[, cols]
  }

  out
}
