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

test_that("GoFs on mulitple estimations", {
  ms_multi <- modelsummary::modelsummary(est_m, output = "modelsummary_list")
  glance_names <- unique(lapply(ms_multi, function(x) names(x[["glance"]])))

  expect_length(glance_names, 1)
  expect_identical(glance_names,
                   list(c("aic", "bic", "r.squared", "adj.r.squared",
                          "rmse", "nobs", "vcov.type", "Family")))
})

test_that("GoFs on IV models", {
  base <- iris
  names(base) <- c("y", "x1", "x_endo_1", "x_inst_1", "fe")
  set.seed(2)
  base$x_inst_2 <- 0.2 * base$y + 0.2 * base$x_endo_1 + rnorm(150, sd = 0.5)
  base$x_endo_2 <- 0.2 * base$y - 0.2 * base$x_inst_1 + rnorm(150, sd = 0.5)

  fml <-
    list(iv1 = y ~ x1 | fe | x_endo_1 ~ x_inst_1,
         iv2 = y ~ x1 | fe | x_endo_1 + x_endo_2 ~ x_inst_1 + x_inst_2)

  m <- lapply(fml, function(x) fixest::feols(x, data = base))
  ms <- lapply(m, function(x) modelsummary::modelsummary(x, output = "modelsummary_list"))
  ms_gof_names <- lapply(ms, function(x) names(x[["glance"]]))

  expect_identical(ms_gof_names[["iv1"]],
                   c("aic", "bic", "r.squared", "adj.r.squared",
                     "r2.within", "r2.within.adjusted", "rmse",
                     "nobs", "FE: fe", "vcov.type", "Family",
                     "ivf1::x_endo_1.stat", "ivf1::x_endo_1.p",
                     "ivf1::x_endo_1.df1", "ivf1::x_endo_1.df2",
                     "ivf2.stat", "ivf2.p", "ivf2.df1", "ivf2.df2",
                     "wh.stat", "wh.p", "wh.df1", "wh.df2"))
  expect_identical(ms_gof_names[["iv2"]],
                   c("aic", "bic", "r.squared", "adj.r.squared",
                     "r2.within", "r2.within.adjusted", "rmse",
                     "nobs", "FE: fe", "vcov.type", "Family",
                     "ivf1::x_endo_1.stat", "ivf1::x_endo_1.p",
                     "ivf1::x_endo_1.df1", "ivf1::x_endo_1.df2",
                     "ivf1::x_endo_2.stat", "ivf1::x_endo_2.p",
                     "ivf1::x_endo_2.df1", "ivf1::x_endo_2.df2",
                     "ivf2.stat", "ivf2.p", "ivf2.df1", "ivf2.df2",
                     "wh.stat", "wh.p", "wh.df1", "wh.df2"))
})
