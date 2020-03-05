
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmstats

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specr)](https://CRAN.R-project.org/package=specr)
<!-- badges: end -->

This package includes some custom-made functions to facilitate
common statistical procedures as well as extracting and reporting
results from various statistical models. The goal is to provide tools and functions to facilitate computational reproducibility using R and RMarkdown.

Please note that at the moment, most functions are still highly costumized to my personal workflow. They may hence
break in more general frameworks or when used in a different or non-intended way…

## Dependencies

Most functions require the following packages:

  - For data wrangling procedures: `tidyverse` and `magrittr`
  - For printing and typesetting: `papaja`
  - For SEM procedures: `lavaan` and `semTools`

These packages should be installed prior to using this package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("masurp/pmstats")
```
