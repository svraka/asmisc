withr::local_options(list(readr.show_col_types = FALSE))
file <- readr::readr_example("mtcars.csv")
df_base <- readr::read_csv(file)

test_that("A roundtrip from file to dataset does not change any data", {
  df <- df_base
  dataset_path <- withr::local_tempfile()

  read_delim_chunked_to_dataset(file, dataset_path, file_nrow = nrow(df),
                                chunk_size = 5, delim = ",")

  ds <- dplyr::collect(arrow::open_dataset(dataset_path))
  ds <- dplyr::select(ds, -chunk)

  expect_equal(
    ds, df,
    ignore_attr = TRUE # ignore rownames, etc.
  )
})

test_that("A roundtrip from file to dataset does not change any data other than modifications done with `processing_function`", {
  test_processing_function <- function(df) {
    dplyr::mutate(df, test_col = "foo")
  }
  dataset_path <- withr::local_tempfile()

  df <- df_base
  df <- test_processing_function(df)

  read_delim_chunked_to_dataset(file, dataset_path,
                                file_nrow = nrow(df), chunk_size = 5,
                                processing_function = test_processing_function,
                                delim = ",")

  ds <- dplyr::collect(arrow::open_dataset(dataset_path))
  ds <- dplyr::select(ds, -chunk)

  expect_equal(
    ds, df,
    ignore_attr = TRUE
  )
})

test_that("`write_single_partition_dataset()` writes only one partition", {
  df <- df_base
  dataset_path <- withr::local_tempfile()
  write_single_partition_dataset(df, dataset_path)

  ds <- arrow::open_dataset(dataset_path)
  n_files <- length(ds$files)

  expect_true(n_files == 1)
})

test_that("`write_single_partition_dataset()` does not change any data", {
  df <- df_base
  dataset_path <- withr::local_tempfile()
  write_single_partition_dataset(df, dataset_path)

  ds <- dplyr::collect(arrow::open_dataset(dataset_path))
  ds <- dplyr::select(ds, -chunk)

  expect_equal(
    ds, df,
    ignore_attr = TRUE
  )
})

test_that("Parsing warnings are not silenced", {
  df <- df_base
  dataset_path <- withr::local_tempfile()

  expect_warning(
    res <- read_delim_chunked_to_dataset(file, dataset_path,
                                  file_nrow = nrow(df),
                                  chunk_size = 5, delim = ",",
                                  col_types = paste0(rep("i", 11),
                                                     collapse = "")),
    regexp = "\\d+ parsing failures."
  )

  expect_true(is.data.frame(res) && nrow(res) > 0)
})

test_that("Determining chunk paths is correct", {
  dataset_path <- withr::local_tempfile()

  expect_identical(
    get_chunk_paths(dataset_path, 10, 5),
    file.path(dataset_path, sprintf("chunk=%d", 1:2), "data.parquet")
  )

  expect_identical(
    get_chunk_paths(dataset_path, 50, 5),
    file.path(dataset_path, sprintf("chunk=%02d", 1:10), "data.parquet")
  )

  expect_identical(
    get_chunk_paths(dataset_path, 50, 50),
    file.path(dataset_path, sprintf("chunk=%d", 1), "data.parquet")
  )

  expect_identical(
    get_chunk_paths(dataset_path, 50, 51),
    file.path(dataset_path, sprintf("chunk=%d", 1), "data.parquet")
  )
})
