df <- tibble::tribble(~ id1, ~ id2, ~ id3,
                      1L,    1L,    1L,
                      1L,    2L,    2L,
                      2L,    1L,    3L,
                      3L,    1L,    4L,
                      3L,    1L,    5L)
dt <- data.table::as.data.table(df, key = c("id1", "id2", "id3"))

by <- list("id1", "id2", c("id1", "id2"))

res1_check <- tibble::tribble(~ by,            ~ N_unique, ~ N_duplicated,
                              "id1",           3L,         2L,
                              "id2",           2L,         3L,
                              c("id1", "id2"), 4L,         1L)
res1_check <- as.data.frame(res1_check)
res2_check <- tibble::add_row(res1_check,
                              by = list(NULL),
                              N_unique = 5L,
                              N_duplicated = 0L)

test_that("default method", {
  res1 <- duplicates(x = df, by_list = by)
  res2 <- duplicates(x = df, by_list = by, check_all = TRUE)

  expect_identical(res1, res1_check)
  expect_identical(res2, res2_check)
})

test_that("data.table", {
  res1 <- duplicates(x = dt, by_list = by)
  res2 <- duplicates(x = dt, by_list = by, check_all = TRUE)

  expect_identical(res1, res1_check)
  expect_identical(res2, res2_check)
})
