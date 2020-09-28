context("Dateset writer")

file <- readr::readr_example("mtcars.csv")
tmp <- tempfile()
dataset_path <- file.path(tmp, "mtcars")

setup({dir.create(tmp)})
teardown({unlink(tmp, recursive = TRUE)})

test_that("A roundtrip from file to dataset does not change any data", {
  df <- readr::read_csv(file)

  read_delim_chunked_to_dataset(file, dataset_path, file_nrow = nrow(df),
                                chunk_size = 5, delim = ",")

  ds <- dplyr::collect(arrow::open_dataset(dataset_path))
  ds <- dplyr::select(ds, -chunk)

  expect_equal(
    ds, df,
    check.attributes = FALSE # ignore rownames, etc.
  )
})

test_that("A roundtrip from file to dataset does not change any data", {
  test_processing_function <- function(df) {
    dplyr::mutate(df, test_col = "foo")
  }

  df <- readr::read_csv(file)
  df <- test_processing_function(df)

  read_delim_chunked_to_dataset(file, dataset_path,
                                file_nrow = nrow(df), chunk_size = 5,
                                processing_function = test_processing_function,
                                delim = ",")

  ds <- dplyr::collect(arrow::open_dataset(dataset_path))
  ds <- dplyr::select(ds, -chunk)

  expect_equal(
    ds, df,
    check.attributes = FALSE
  )
})
