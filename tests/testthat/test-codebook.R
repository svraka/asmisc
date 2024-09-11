context("Codebook")

test_that("Skimmers for `codebook()` are registered", {
  # If there are issues with registering methods we should get
  # warnings. Just to be sure, check for messages as well.
  expect_no_warning(codebook(dplyr::starwars))

  expect_no_message(codebook(dplyr::starwars))

  # By default, skimr returns a long data frame. column types and
  # stats are easier to check with partitioning.
  res <- lapply(skimr::partition(codebook(dplyr::starwars)), names)
  res_names <-
    list(character = c("skim_variable", "n_missing", "complete_rate",
                       "min", "max", "empty", "n_unique",
                       "whitespace", "is_num_chr", "chr_values"),
         integer = c("skim_variable", "n_missing", "complete_rate",
                     "mean", "sd", "p0", "p1", "p25", "p50", "p75",
                     "p99", "p100"),
         list = c("skim_variable", "n_missing", "complete_rate",
                  "n_unique", "min_length", "max_length"),
         numeric = c("skim_variable", "n_missing", "complete_rate",
                     "mean", "sd", "p0", "p25", "p50", "p75", "p100",
                     "hist", "p1", "p99", "is_whole", "maybe_int"))
  expect_identical(res, res_names)
})

test_that("`chr_values()` handles NA values", {
  expect_identical(
    chr_values(c("1", "2", NA)),
    NA_character_
  )

  expect_identical(
    chr_values(c(NA, NA, NA)),
    NA_character_
  )

  expect_identical(
    chr_values(c("a", "b", NA)),
    "a | b"
  )
})

test_that("`is_whole()` reports expected results", {
  expect_true(is_whole(c(1, 2, 3)))
  expect_true(is_whole(c(1, 2, NA)))
  expect_true(is_whole(c(2^32, 2^33, 2^34)))
  expect_true(is_whole(c(NA, NA, NA)))
  expect_false(is_whole(c(1, 2, 3.5)))
  # We can run into problems with floating point precision
  expect_true(is_whole(c(2^10) + 10 * .Machine$double.neg.eps))
})

test_that("maybe_int()` reports expected results", {
  expect_true(maybe_int(c(1, 2, 3)))
  expect_true(maybe_int(c(1, 2, NA)))
  expect_true(maybe_int(c(NA, NA, NA)))
  expect_false(maybe_int(c(1, 2, 2^32)))
})
