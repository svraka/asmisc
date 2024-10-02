#' Convert a `fixest_multi` object to a nested tibble
#'
#' Convert a `fixest_multi` object to a nested tibble, allowing easy
#' tidying of the models.
#'
#' @param x A `fixest_multi` object
#'
#' @return
#'
#' A tibble with model parameters and model IDs as columns and a list
#' column `model` with all models from `x`. Note that the tibble will
#' not contain columns for parameters which do not vary within `x`
#' (see examples).
#'
#' @details
#'
#' Only an `as_tibble` method is provided, no `as.data.frame` because
#' printing lists in a data.frame is often problematic.
#' [fixest::models()], which is used internally, always returns a
#' data.frame.
#'
#' @examples
#' # Results only in an `lhs` column
#' m1 <- fixest::feols(c(mpg, qsec) ~ wt, data = mtcars)
#' tibble::as_tibble(m1)
#'
#' # Results only in an `rhs` column
#' m2 <- fixest::feols(mpg ~ csw(wt, disp), data = mtcars)
#' tibble::as_tibble(m2)
#'
#' @exportS3Method tibble::as_tibble
as_tibble.fixest_multi <- function(x) {
  res <- tibble::as_tibble(fixest::models(x))
  res[["model"]] <- unname(as.list(x))
  res
}
