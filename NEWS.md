# asmisc 1.0.0

## Breaking changes

Or, rather unbreaking some.

  * Updated `codebook()` to use **skimr** v2 along with a few additional minor changes
      * Removed `codebook_chunked()`, as it is superseded by `read_delim_chunked_to_dataset()` (see below)
      * Added skimmers for 1st and 99th percentiles
      * Added skimmers for deciding if a column should be integer, or double
      * Removed customizations in factor and logical skimmers
  * Removed `clean_names`: The **janitor** package has a much better implementation
  * Removed `df_sizes()`: ESS's `rdired` is a good substitute

Per [Semantic Versioning](https://semver.org/) 1.0.0 reflects on these breaking changes and does not refer to any sort of maturity.

## New features

  * Added a few converter functions for data cleaning
      * `yesno_to_logical()`, `mark_to_logical()` to easily parse vectors with values like `"X"`, or `"Y"` and `"N"` into logicals.
      * `parse_date_md` to parse year-month and `parse_date_ymd` to parse YY-MM-DD type dates.
  * Added `read_delim_chunked_to_dataset()` to read a single delimited file in chunks and save them in an Arrow dataset using Hive-style partitioning
  * Added `tlmgr_install_svraka_pkgs()` to install often used TeX Live packages using **tinytex**.

## Miscellaneous

  * Set up GitHub Actions CI
  * Added tests for most the new code

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
