
# mcvis: visualisation of multicollinearity in data

[![Travis build
status](https://travis-ci.org/kevinwang09/mcvis.svg?branch=master)](https://travis-ci.org/kevinwang09/mcvis)
[![Codecov test
coverage](https://codecov.io/gh/kevinwang09/mcvis/branch/master/graph/badge.svg)](https://codecov.io/gh/kevinwang09/mcvis?branch=master)

<img src="inst/mcvis_logo.png" align="right" width="200" />

## Introduction

`mcvis` is a R package for visualising multicollinearity in a data
design matrix. The underlying methodology uses resampling techniques to
identify groups of variables that causes multicollinearity.

You can learn more about `mcvis` from [this
vignette](https://leaffur.github.io/mcvis/articles/mcvis.html).

## Installation

`mcvis` can be installed using the `devtools` package.

``` r
devtools::install_github("leaffur/mcvis")
```

## A quick example

Using a `mcvis` bipartite plot, variables (bottom row) that cause strong
collinearity are visualised as bolded lines connecting with our “tau”
statistics (top row).

``` r
library(mcvis)

set.seed(1)
p = 10
n = 100

X = matrix(rnorm(n*p), ncol = p)
## Inducing collinearity into the design matrix
X[,1] = X[,2] + rnorm(n, 0, 0.1) 


mcvis_result = mcvis(X)
ggplot_mcvis(mcvis_result)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Reference

**mcvis: A new framework for collinearity discovery, diagnostic and
visualisation**. Lin. C, Wang, Y.X. K and Mueller S.
