#' Create a codebook from a data frame
#'
#' Create a codebook of a data frame using \code{\link[skimr]{skim}}.
#'
#' @param df A data frame
#'
#' @return
#' A tibble from a custom wide format \code{\link[skimr]{skim}} output.  Unlike
#' \code{\link[skimr]{skim}}, histograms are not generated but there are three
#' additional columns: \code{\link[sjmisc:is_float]{is_whole}},
#' \code{\link[sjmisc:is_num_fac]{is_num_chr}}, and \code{chr_values}, a list
#'  column with the first 10 unique character values.
#'
#' @export
#'
#' @seealso \code{\link{codebook_chunked}} for files that are too big to be read
#'   into memory at once.
#'
#' @examples
#' codebook(mtcars)
codebook <- function(df) {
  # Drop histogram from `skim` results

  skimr::skim_with(
    numeric = list(hist = NULL),
    integer = list(hist = NULL),
    logical = list(count = NULL)
  )

  # Add custom skims

  skimr::skim_with(
    numeric = list(is_whole = ~all(floor(.) == ., na.rm = TRUE)),
    character = list(is_num_chr = sjmisc::is_num_chr)
  )

  # Skim the data frame and spread it wide for a better overview.
  # `skim_to_wide` outputs formatted characters, which are inconvenient, so we
  # roll our own solution.  Additionally, the variables are sorted in the
  # original order and not by type.

  varnames <- dplyr::tibble(variable = names(df)) %>%
    dplyr::mutate(var_number = dplyr::row_number())

  codebook <- skimr::skim(df) %>%
    unclass %>% dplyr::as_tibble() %>%
    dplyr::select(-.data$level, -.data$formatted) %>%
    tidyr::spread(key = .data$stat, value = .data$value) %>%
    dplyr::left_join(varnames, by = c("variable" = "variable")) %>%
    dplyr::arrange(.data$var_number) %>%
    dplyr::select(-.data$var_number)

  # Create `is_whole` and `is_num_chr` if they don't exist (in case of no
  # double, or no character columns).

  if (!rlang::has_name(codebook, "is_whole")) {
    codebook <- codebook %>%
      dplyr::mutate(is_whole = NA)
  }

  if (!rlang::has_name(codebook, "is_num_chr")) {
    codebook <- codebook %>%
      dplyr::mutate(is_num_chr = NA)
  }

  # Convert both to logical

  codebook <- codebook %>%
    dplyr::mutate_at(dplyr::vars(.data$is_whole, .data$is_num_chr), as.logical)

  # Add `chr_values` column

  chr_values <- df %>%
    dplyr::select_if(is.character) %>%
    purrr::map(chr_values) %>%
    tibble::enframe(name = "variable", value = "chr_values")

  codebook <- codebook %>%
    dplyr::left_join(chr_values, by = "variable")

  # Add `lgl_counts` column

  lgl_counts <- df %>%
    dplyr::select_if(is.logical) %>%
    purrr::map(skimr::sorted_count) %>%
    tibble::enframe(name = "variable", value = "lgl_counts")

  codebook <- codebook %>%
    dplyr::left_join(lgl_counts, by = "variable")

  # Reorder columns. Some of these only exist if there are character columns and
  # others only exist if there are temporal columns.  `one_of` reorders the
  # columns as needed but issues warnings, which we suppress.
  codebook <- suppressWarnings(dplyr::select(
      codebook,
      dplyr::one_of(
        "variable",
        "type",
        "missing",
        "complete",
        "n",
        "min",
        "max",
        "is_num_chr",
        "chr_values",
        "empty",
        "n_unique",
        "is_whole",
        "mean",
        "sd",
        "median",
        "p0",
        "p25",
        "p50",
        "p75",
        "p100"
      ),
      dplyr::everything()
    )
  )

  # Restore defaults

  skimr::skim_with_defaults()

  codebook
}

#' Create codebook by chunks
#'
#' \code{codebook_chunked} uses \code{codebook} to create codebooks for chunks
#' of a delimited file, as read by \code{\link[readr]{read_delim_chunked}}.
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
#' @param ... Additional arguments passed to
#'   \code{\link[readr]{read_delim_chunked}}
#'
#' @return A tibble like from \code{\link{codebook}} with number of variables
#' \eqn{\times}{*} number of chunks rows and an additional column named
#' \code{chunk} containing the chunk number.  Parsing problmes by
#' \code{\link[readr]{read_delim_chunked}} are stored in an attribute of the
#'  tibble, which can be accessed by \code{\link[readr]{problems}}.
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
                              ...) {
  if (is.null(process_function)) {
    callback_codebook <- function(x, pos) {
      problems <- readr::problems(x)

      list(codebook = codebook(x), problems = problems)
    }
  }
  else {
    callback_codebook <- function(x, pos) {
      problems <- readr::problems(x)

      codebook <- process_function(x) %>% codebook

      list(codebook = codebook, problems = problems)
    }
  }

  chunks <- read_chunked(
    input_delim = input_delim,
    file = file,
    chunk_size = chunk_size,
    guess_max = chunk_size,
    readr::ListCallback$new(callback_codebook),
    ...
  )

  # Bind chunks and arrange by original variable order

  codebook <- chunks %>%
    purrr::map_dfr(~purrr::pluck(., "codebook"), .id = "chunk") %>%
    dplyr::mutate(chunk = as.integer(.data$chunk)) %>%
    dplyr::group_by(.data$chunk) %>%
    dplyr::mutate(var_number = dplyr::row_number()) %>%
    dplyr::arrange(.data$var_number, .data$chunk) %>%
    dplyr::select(-.data$var_number) %>%
    dplyr::ungroup()

  # Bind `problems()` and set as attribute to make it accessible by
  # `readr::problems()`.

  problems <- chunks %>%
    purrr::map_dfr(~purrr::pluck(., "problems"), .id = "chunk") %>%
    dplyr::mutate(chunk = as.integer(.data$chunk))

  attr(codebook, "problems") <- problems

  n_problems <- nrow(problems)

  if (n_problems != 0) {
    warning(
      n_problems,
      " parsing failures. See problems(...) for more details.",
      call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
    )
  }

  codebook
}
