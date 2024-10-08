est_s <- fixest::feols(mpg ~ wt, data = mtcars)
est_m <- fixest::feols(c(mpg, qsec) ~ wt, data = mtcars)

test_that("Converting fixest_multi to a nested tibble", {
  t <- tibble::as_tibble(est_m)

  expect_identical(names(t), c("id", "lhs", "model"))
  expect_true(all(sapply(t[["model"]], function(x) inherits(x, "fixest"))))
})

test_that("Converting fixest to a nested tibble", {
  t <- tibble::as_tibble(est_s)

  expect_identical(t, tibble::tibble(id = 1L, model = list(est_s)))
})

test_that("Converting a mixed list of `fixest` and `fixest_multi` objects to a nested tibble", {
  l <- list(est_s, est_m)
  lt <- lapply(l, tibble::as_tibble)
  lt <- dplyr::bind_rows(lt)

  expect_length(lt[["model"]], 3)
  expect_identical(names(lt),
                   # Column order depend on the order of list element,
                   # it something we shouldn't touch.
                   c("id",  "model", "lhs"))
})

test_that("glance_custom.fixest works in single fixest models", {
  expect_identical(modelsummary::glance_custom(est_s),
                   data.frame("Family" = "OLS"))
})

test_that("We extended modelsummary GoFs", {
  ms_multi <- modelsummary::modelsummary(est_m, output = "modelsummary_list")
  glance_names <- unique(lapply(ms_multi, function(x) names(x[["glance"]])))

  expect_length(glance_names, 1)
  expect_identical(glance_names,
                   list(c("aic", "bic", "r.squared", "adj.r.squared",
                          "rmse", "nobs", "vcov.type", "Family")))
})
