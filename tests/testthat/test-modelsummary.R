est_lm <- lm(mpg ~ wt, data = mtcars)

est_s <- fixest::feols(mpg ~ wt, data = mtcars)
est_m <- fixest::feols(c(mpg, qsec) ~ wt, data = mtcars)

est_s_t <- broom::tidy(est_s)
est_s_g <- broom::glance(est_s)

test_that("helper", {
  ms <- as.modelsummary_list(est_s_t, est_s_g)

  expect_identical(names(ms), c("tidy", "glance"))
  expect_true(inherits(ms, "modelsummary_list"))
  expect_snapshot(modelsummary::modelsummary(ms))
})

test_that("default method", {
  ms <- as.modelsummary_list_custom(est_lm)

  expect_identical(names(ms), c("tidy", "glance"))
  expect_true(inherits(ms, "modelsummary_list"))
  expect_snapshot(modelsummary::modelsummary(ms))
})

test_that("fixest method", {
  ms <- as.modelsummary_list_custom(est_s)
  expect_identical(names(ms), c("tidy", "glance"))
  expect_true(inherits(ms, "modelsummary_list"))
  expect_snapshot(modelsummary::modelsummary(ms))
})

test_that("fixest multiple estimation models are not supported", {
  expect_error(as.modelsummary_list_custom(est_m),
               "No tidy method for objects of class fixest_multi")
})

test_that("arguments are passed on", {
  sm1 <- as.modelsummary_list_custom(est_s)
  sm2 <- as.modelsummary_list_custom(est_s, vcov = "hetero")
  sm3 <- as.modelsummary_list_custom(est_s, vcov = fixest::vcov_cluster("cyl"))

  expect_identical(attr(sm1[["tidy"]][["std.error"]], "type"),
                   "IID")
  expect_identical(attr(sm2[["tidy"]][["std.error"]], "type"),
                   "Heteroskedasticity-robust")
  expect_identical(attr(sm3[["tidy"]][["std.error"]], "type"),
                   "Clustered (cyl)")
})
