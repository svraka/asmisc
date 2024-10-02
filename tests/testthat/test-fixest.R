est_s <- fixest::feols(mpg ~ wt, data = mtcars)
est_m <- fixest::feols(c(mpg, qsec) ~ wt, data = mtcars)

test_that("Converting fixest_multi to a nested tibble", {
  t <- tibble::as_tibble(est_m)

  expect_identical(names(t), c("id", "lhs", "model"))
  expect_true(all(sapply(t[["model"]], function(x) inherits(x, "fixest"))))
})

test_that("Converting a single fixest model to a nested tibble fails", {
  expect_error(tibble::as_tibble(est_s),
               # This a regex, the error might contain fancy quotes
               "cannot coerce class .\"fixest\". to a data.frame")
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
