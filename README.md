
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggshapes

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/EmilHvitfeldt/ggshapes.svg?branch=master)](https://travis-ci.org/EmilHvitfeldt/ggshapes)
[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/ggshapes/branch/master/graph/badge.svg)](https://codecov.io/gh/EmilHvitfeldt/ggshapes?branch=master)
<!-- badges: end -->

The goal of ggshapes is to add more shapes such and stars and other
curves to ggplot2 the same way ggforce adds regular polygons and
circles.

## Installation

You can install the released version of ggshapes from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Not released yet
install.packages("ggshapes")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/ggshapes")
```

## Example

``` r
library(ggshapes)
#> Loading required package: ggplot2
ggplot() +
  geom_lissajous(aes(a = 5, b = 4, delta = 2))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
data <- data.frame(a = rep(1:4, times = 4),
                   b = rep(1:4, each = 4),
                   delta = 1)

ggplot(data) +
  geom_lissajous(aes(a = a, b = b, delta = delta)) +
  facet_grid(a ~ b)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />