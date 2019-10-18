context("Converters")

test_that("`yesno_to_logical()` fails on wrong input", {
  x <- c("I", "N", "I")

  expect_error(
    yesno_to_logical(x, yn = c("i")),
    "`yn` needs two distinct elements."
  )
  expect_error(
    yesno_to_logical(x, yn = c("i", "i")),
    "`yn` needs two distinct elements."
  )
  expect_error(
    yesno_to_logical(x, yn = c("i", "n", "i")),
    "`yn` needs two distinct elements."
  )
  expect_error(
    yesno_to_logical(c(x, "a")),
    "`x` contains more than two distinct non-NA values."
  )
  expect_error(
    yesno_to_logical(c("I", "i")),
    "`x` contains non-NA values not found in `yn`."
  )
})

test_that("`yesno_to_logical()` handles NA correctly", {
  expect_identical(
    yesno_to_logical(c("I", "N", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_identical(
    yesno_to_logical(c("I", "N", NA), na_to_false = TRUE),
    c(TRUE, FALSE, FALSE)
  )
  expect_identical(
    yesno_to_logical(c(NA_character_)),
    c(NA)
  )
  expect_identical(
    yesno_to_logical(c(NA_character_), na_to_false = TRUE),
    c(FALSE)
  )
})

test_that("`mark_to_logical()` fails on wrong input", {
  expect_error(
    mark_to_logical(c("X"), mark = c("X", "Y")),
    "`mark` must be character vector of length 1."
  )
  expect_error(
    mark_to_logical(c("X", "Y")),
    "`x` contains non-NA values other than `mark`"
  )
  expect_error(
    mark_to_logical(c("X", "Y", NA)),
    "`x` contains non-NA values other than `mark`"
  )
})

test_that("`mark_to_logical()` handles NA correctly", {
  expect_identical(
    mark_to_logical(c("X", "X", NA)),
    c(TRUE, TRUE, NA)
  )
  expect_identical(
    mark_to_logical(c("X", "X", NA), na_to_false = TRUE),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(
    mark_to_logical(c(NA_character_)),
    c(NA)
  )
  expect_identical(
    mark_to_logical(c(NA_character_), na_to_false = TRUE),
    c(FALSE)
  )
})
