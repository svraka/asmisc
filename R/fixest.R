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

#' Custom glances for fixest modelsummaries
#'
#' Add model family to glances in fixest modelsummaries using the
#' [modelsummary::glance_custom()] customization interface.
#'
#' @inheritParams modelsummary::glance_custom
#' @exportS3Method modelsummary::glance_custom
glance_custom.fixest <- function(x, ...) {
  # modelsummary always extracts single models from `fixest_multi`
  # objects, thus. Just to make sure, we bail out if that is not the
  # case
  stopifnot(inherits(x, "fixest"))
  # fixest doesn't expose a function to convert model families into
  # readable labels, it's all done internally in `fixest::etable`. I
  # don't want to copy it (and fixest is licenced under GPL anyway, so
  # using it in our MIT project would be complicated), so we have to
  # extract final labels from an `etable_df` object.
  #
  # I think an IID SE is always returned with the model object and
  # after some testing, creating a table with `vcov = "iid"` is fast
  # on model objects with high N, many RHS variables and FEs.
  t <- fixest::etable(x, vcov = "iid", family = TRUE)
  # In case of a single model we work with a data.frame of two
  # columns, where the first contains row labels, and the second
  # "stats".
  family <- t[t[[1]] == "Family", ][[2]]

  out <- data.frame("Family" = family)
  return(out)
}
