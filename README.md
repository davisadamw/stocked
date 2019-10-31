
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stocked

<!-- badges: start -->

<!-- badges: end -->

The goal of stocked is to make it easier to keep all the code straight
for weird iterative bass models like what we’re using for the market
share tool.

## Installation

And the development version from
[GitHub](https://github.com/davisadamw/stocked) with:

``` r
# install.packages("devtools")
devtools::install_github("davisadamw/stocked")
```

## Example

This is a basic example of how to use this package’s functions to
calculate the slope of a bass curve at a given level of market
penetration:

``` r
library(stocked)
bass(M = 0.5, p = 0.05, q = 0.4)
#> [1] 0.125
```
