# asmisc

<!-- badges: start -->
[![R-CMD-check](https://github.com/svraka/asmisc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/svraka/asmisc/actions/workflows/R-CMD-check.yaml)
[![CRAN](https://www.r-pkg.org/badges/version/asmisc)](https://cran.r-project.org/package=asmisc)
<!-- badges: end -->

A small collection of utility functions for personal use.

## Installation

I have no intention of keeping the package in sort of stable state. The recommended way to use it in a project is to pin it to a specific commit with [renv](https://rstudio.github.io/renv). With that in mind, you can install asmisc from [GitHub](https://github.com/svraka/asmisc) with:

``` r
remotes::install_github("svraka/asmisc")
```

Or:

``` r
renv::install("github::svraka/asmisc")
```

### A note on dependencies

This package is a collection of heterogeneous functionality, extending various packages with wrappers and helpers. Thus none of this package's dependencies are true hard dependencies. Nevertheless, everything is kept under `Imports`. My projects always use the
[tidyverse](https://www.tidyverse.org) and
[targets](https://docs.ropensci.org/targets/),
almost always use
[data.table](https://r-datatable.com),
[fixest](https://lrberge.github.io/fixest/), and
[DeclareDesign](https://declaredesign.org/r/declaredesign/).
These cover almost all dependencies of this package (directly, or indirectly). Additionally, using hard dependencies simplifies maintenance (e.g. `codebook` registers S3 methods defined in skimr, and I don't want to deal with conditional registration).
