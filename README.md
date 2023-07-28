
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getACS

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of getACS is to make it easier to work with [American Community
Survey data](https://www.census.gov/programs-surveys/acs) from the
[tidycensus package](https://walker-data.com/tidycensus/). The package
includes:

- Helpers for creating tables of ACS data using the [gt
  package](https://gt.rstudio.com/)
- Helpers that extend the existing `tidycensus::get_acs()` function to
  work with multiple years or geographies

Note that I don’t love the current name for this package and expect to
rename it as soon as I think of a better one.

## Installation

You can install the development version of getACS from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("elipousson/getACS")
```

## Related packages

- [easycensus](https://github.com/CoryMcCartan/easycensus)
- [cwi](https://ct-data-haven.github.io/cwi/)