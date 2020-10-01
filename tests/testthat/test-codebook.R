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
