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

#' Convert a `fixest` object to a nested tibble
#'
#' Convert a `fixest` object to a nested tibble, allowing easy
#' tidying, along with multiple estimation models nested using
#' `as_tibble.fixest_multi()`.
#'
#' @param x A `fixest` object
#'
#' @return
#'
#' A tibble with one row and two columns, `id = 1L`, and `model =
#' list(x)`.
#'
#' @exportS3Method tibble::as_tibble
as_tibble.fixest <- function(x) {
  tibble::tibble(id = 1L, model = list(x))
}

#' Custom glances for fixest modelsummaries
#'
#' Add further glances to fixest modelsummaries using the
#' [modelsummary::glance_custom()] customization interface.
#'
#' @inheritParams modelsummary::glance_custom
#' @inheritParams fixest::fitstat
#' @param ... Further arguments passed to [fixest::fitstat()]'s `...`
#'   argument.
#'
#' @exportS3Method modelsummary::glance_custom
#'
#' @details
#'
#' By default [modelsummary::modelsummary()] adds a lot of useful
#' information to glances (fixed effects, VCOV types) but omits some
#' others which are often important, and [fixest::etable()] makes
#' their reporting easy. We extend the custom glance with the
#' following:
#'
#' - Family (OLS, Poisson, etc.)
#' - IV fit statistics. Note, this is likely to require further
#'   processing with a custom [`modelsummary::gof_map`] to clean up
#'   names of statistics and which instrument they refer to. Due to
#'   the way \pkg{modelsummary} handles `...`s, we cannot pass on
#'   further arguments, therefore we include all but Wald tests. They
#'   are omitted because their value depends on the VCOV type which,
#'   cannot be passed on either. ([fixest::fitstat()] lists Wald and
#'   DoF for t-tests (aka G) as two examples where VCOV is needed,
#'   checking the source confirms that it is not used anywhere else.)
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
  family <- data.frame("Family" = family)

  if (is.null(x[["iv_stage"]])) {
    # A zero-column data frame can always be column-bound.
    iv_fitstats <- data.frame(matrix(ncol = 0, nrow = 1))
  } else {
    types <- c("ivf1", "ivf2", "wh", "sargan")
    iv_fitstats <- fixest::fitstat(x, type = types)
    # We drop `NA` values, as `NA`s cannot be converted to a data
    # frame. Result can be `NA` for the Sargan test as it only makes
    # sense if the number of endogenous regressors is less than the
    # number of instruments.
    iv_fitstats_keep <- sapply(iv_fitstats,
                               function(x) length(x) == 1 && is.na(x))
    iv_fitstats <- iv_fitstats[!iv_fitstats_keep]
    iv_fitstats <- lapply(iv_fitstats, function(x) do.call(data.frame, x))
    iv_fitstats <- do.call(cbind, args = iv_fitstats)
  }

  out <- cbind(family, iv_fitstats)
  return(out)
}
