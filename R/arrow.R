metadata_parquet <- function(file) {
  pq <- arrow::ParquetFileReader$create(file)

  pq_schema <- pq$GetSchema()
  pq_names <- pq_schema$names
  pq_types <- sapply(pq_schema$fields, function(x) x$type$ToString())

  df <- data.frame(name = pq_names, type = pq_types, stringsAsFactors = FALSE)

  if (requireNamespace("tibble")) df <- tibble::as_tibble(df)

  df
}
