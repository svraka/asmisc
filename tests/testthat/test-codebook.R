context("Codebook")

test_that("we cover all default skimmers", {
  defaults <- names(skimr::get_default_skimmer_names())
  # This list is manually copied from `codebook()`. See comments why
  # the custom sfl cannot be defined. `integer` is left out, as skimr
  # treats them as numerics.
  codebook_skimmers <- c("AsIs", "character", "complex", "Date",
                         "difftime", "factor", "list",
                         "logical", "numeric", "POSIXct", "Timespan",
                         "ts")

  expect_identical(defaults, codebook_skimmers)
})
