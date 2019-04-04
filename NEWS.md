# asmisc 0.2.0

First public release.

## New features

* Full support for logical and factor columns in `codebook()`: `lgl_counts` and `fct_count` are list columns with frequencies.
* `find_project_packages()` has a new `not_installed` argument to list packages that are not installed.  `needed_project_packages()` was removed.
* New function `clean_names` to sanitize column names in a data frame.

## Bug fixes

* `is_whole` works on all-NA columns.
* `codebook_chunked()` always uses `readr:read_delim_chunked` and all of its arguments can be set in `codebook_chunked()`.


# asmisc 0.1.0

* Support for logical vectors in `codebook()`: `codebook()` now creates a list column of frequencies.
* Fixes incorrect `NA` results for the `is_whole` column in `codebook()`.


# asmisc 0.0.1

* First release
