context("Codebook")

test_that("all default skimmers are covered", {
  default_skimmers <- sort(names(skimr::get_default_skimmer_names()))
  # This list is manually copied from `codebook()`. See comments why
  # the custom sfl cannot be defined. `integer` is left out, as skimr
  # treats them as numerics.
  codebook_skimmers <- sort(c("AsIs", "character", "complex", "Date",
                              "difftime", "factor", "list", "logical",
                              "numeric", "POSIXct", "Timespan", "ts"))

  expect_identical(default_skimmers, codebook_skimmers)
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
