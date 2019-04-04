#' Clean column names
#'
#' Clean column names in a data frame.
#'
#' @param df A data frame
#'
#' @return The data frame with cleaned column names.
#'
#' @details This function makes all column names lowercase, strips diacritics
#'   and punctuation marks, and replaces whitespace with underscores.
#' @export
clean_names <- function(df) {
  df %>%
    dplyr::rename_all(~stringi::stri_trans_general(., id = "latin-ascii")) %>%
    dplyr::rename_all( stringr::str_to_lower) %>%
    dplyr::rename_all(~stringr::str_replace_all(., "[\\s-]+", "_")) %>%
    dplyr::rename_all(~stringr::str_replace_all(., "[\\.',]", "")) %>%
    dplyr::rename_all(~stringr::str_replace_all(., "(^_|_$)", ""))
}
