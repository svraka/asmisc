#' Create a codebook from a data frame
#'
#' Create a codebook of a data frame using \code{\link[skimr]{skim}}.
#'
#' @param df A data frame
#'
#' @return
#' A list of three tibbles.
#'
#' \describe{
#'   \item{chr_cols}{A tibble of character columns from \code{df} in rows with a
#'     vector of the first 10 unique values of the original variable.  It allows
#'     you to inspect character columns that can actually be encoded as numeric
#'     columns. Especially useful when the rows higher than \code{guess_max}
#'     from \code{\link[readr]{read_delim}} contain \code{NA}s.}
#'   \item{skims}{A tibble with the results from a customized
#'     \code{\link[skimr]{skim}} output in a wide format}
#'   \item{problems}{Results from \code{\link[readr]{problems}}}
#' }
#' @export
#'
#' @seealso \code{\link{codebook_chunked}} for files that are too big to be read
#'   into memory at once.
#'
#' @examples
#' codebook(mtcars)
codebook <- function(df) {
  chr_cols <- df %>%
    dplyr::select_if(is.character)

  # Create empty tibble if there are no character columns
  if (ncol(chr_cols) == 0) {
    chr_cols <- dplyr::tibble(variable = character(), chr_values = character())
  } else {
    chr_cols <- chr_cols %>%
      dplyr::summarise_all(dplyr::funs(list(unique(.) %>% sort %>%
                                       stringr::str_subset("[^\\d]") %>%
                                       utils::head(n = 10)))
      ) %>%
      tidyr::gather() %>%
      dplyr::rename(variable = .data$key, chr_values = .data$value)
  }

  # Drop histogram from `skim` results

  skimr::skim_with(
    numeric = list(hist = NULL),
    integer = list(hist = NULL)
  )

  skimr::skim_with(
    numeric = list(is_whole = ~all(floor(.) == ., na.rm = TRUE))
  )

  # Skim the data frame and spread it wide for a better overview. `skim_to_wide`
  # outputs formatted characters, which are inconvenient, so we roll our own
  # solution.  Additionally, the variables are sorted in the original order and
  # not by type.

  varnames <- dplyr::tibble(variable = names(df)) %>%
    dplyr::mutate(var_number = dplyr::row_number())

  skims <- skimr::skim(df) %>%
    unclass %>% dplyr::as_tibble() %>%
    dplyr::select(-.data$level, -.data$formatted) %>%
    tidyr::spread(key = .data$stat, value = .data$value) %>%
    dplyr::left_join(varnames, by = c("variable" = "variable")) %>%
    dplyr::arrange(.data$var_number) %>%
    dplyr::select(-.data$var_number)

  # Add `is_whole` column if it was not created (in case of no double columns)

  if (!rlang::has_name(skims, "is_whole")) {
    skims <- skims %>%
      dplyr::mutate(is_whole = NA)
  }

  # Reorder columns. Some of these only exist if there are character columns and
  # others only exist if there are temporal columns.  `one_of` reorders the
  # columns as needed but issues warnings, which we suppress.
  skims <- suppressWarnings(dplyr::select(
      skims,
      dplyr::one_of("variable", "type", "missing", "complete", "n", "is_whole",
                    "min", "max", "empty", "n_unique", "mean", "sd", "median",
                    "p0", "p25", "p50", "p75", "p100"),
      dplyr::everything()
    )
  )

  # Restore defaults

  skimr::skim_with_defaults()

  problems <- readr::problems(df)

  list(chr_cols = chr_cols, skims = skims, problems = problems)
}

#' Create codebook by chunks
#'
#' \code{codebook_chunked} uses \code{codebook} to create codebooks for chunks
#' of a file, as read by \code{\link[readr]{read_delim_chunked}}.
#'
#' @param file Path to a file.  Passed to
#'   \code{\link[readr]{read_delim_chunked}}, see further details there.
#' @param input_delim Single character used to separate fields within a record.
#' @param chunk_size The number of rows to include in each chunk.  See
#'   \code{\link[readr]{read_delim_chunked}}
#' @param process_function A function that takes a data frame as its input and
#'   processes this data frame.  \code{process_function} is applied to each
#'   chunk before creating the codebook for that chunk.  There are no
#'   restrictions what this function can be.  When left as \code{NULL}, no
#'   processing is done.
#' @param simplify Simplify the results.
#' @param ... Additional arguments passed to
#'   \code{\link[readr]{read_delim_chunked}}
#'
#' @return If \code{simplify} is \code{TRUE}, a list with the same structure as
#'   \code{\link{codebook}} but each tibble has an additional column named
#'   \code{chunk} with the number of the chunk it is describing and there are
#'   additional rows for each variable in each chunk.  Otherwise, a list of
#'   lists returned by \code{\link{codebook}}.
#'
#' @export
#'
#' @examples
#' codebook_chunked(
#'   file = readr::readr_example("mtcars.csv"),
#'   input_delim = ",",
#'   chunk_size = 10
#' )
codebook_chunked <- function(file,
                              input_delim,
                              chunk_size,
                              process_function = NULL,
                              simplify = TRUE,
                              ...) {
  if (is.null(process_function)) {
    callback_codebook <- function(x, pos) {
      problems <- readr::problems(x)

      codebook(x)
    }
  }
  else {
    callback_codebook <- function(x, pos) {
      problems <- readr::problems(x)

      process_function(x) %>% codebook
    }
  }

  chunk <- read_chunked(
    input_delim = input_delim,
    file = file,
    chunk_size = chunk_size,
    guess_max = chunk_size,
    readr::ListCallback$new(callback_codebook),
    ...
  )

  if (simplify == TRUE) {
    chunk <- simplify_chunked_codebook(chunk)
  }

  chunk
}

#' Simplify chunked codebook
#'
#' Simplify a list in \code{\link{codebook_chunked}} into a list of three
#' tibbles like the output of \code{\link{codebook}}.
#'
#' @param codebook A list of lists from \code{\link{codebook}}.
#'
#' @keywords internal
simplify_chunked_codebook <- function(codebook) {
  chr_cols <- codebook %>%
    purrr::map(purrr::pluck, "chr_cols") %>%
    dplyr::bind_rows(.id = "chunk") %>%
    dplyr::mutate(chunk = as.integer(.data$chunk)) %>%
    dplyr::group_by(.data$chunk) %>%
    dplyr::mutate(var_number = dplyr::row_number()) %>%
    dplyr::group_by(.data$variable, .data$var_number) %>%
    dplyr::summarise(chr_values = list(.data$chr_values)) %>%
    dplyr::mutate(
      chr_values = purrr::map(
        .data$chr_values,
        function(x) unlist(x) %>% unique %>% utils::head(n = 10)
      )
    ) %>%
    dplyr::arrange(.data$var_number) %>%
    dplyr::select(-.data$var_number) %>%
    dplyr::ungroup()

  skims <- codebook %>%
    purrr::map(purrr::pluck, "skims") %>%
    dplyr::bind_rows(.id = "chunk") %>%
    dplyr::mutate(chunk = as.integer(.data$chunk)) %>%
    dplyr::group_by(.data$chunk) %>%
    dplyr::mutate(var_number = dplyr::row_number()) %>%
    dplyr::arrange(.data$var_number, .data$chunk) %>%
    dplyr::select(-.data$var_number) %>%
    dplyr::ungroup()

  problems <- codebook %>%
    purrr::map(purrr::pluck, "problems") %>%
    dplyr::bind_rows(.id = "chunk") %>%
    dplyr::mutate(chunk = as.integer(.data$chunk))

  list(
    chr_cols = chr_cols,
    skims = skims,
    problems = problems
  )
}
