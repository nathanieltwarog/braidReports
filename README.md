
<!-- README.md is generated from README.Rmd. Please edit that file -->

# braidReports <img src="man/figures/logo.png" align="right" height="126" alt="" />

<!-- badges: start -->
<!-- badges: end -->

The goal of braidReports is to provide simple, intuitive, flexible tools
to visualize combined action analyses and response surfaces, building on
the power and breadth of the `ggplot2` plotting system.

## Example BRAID Analsyis and Report

Here is how you would run a full BRAID analysis (using the `braidrm`
example dataset `synergisticExample`) and produce a one-page report:

``` r
library(braidReports)
#> Loading required package: braidrm
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.4.1
```

``` r
## Run and Report a BRAID Analysis:
surface <- synergisticExample

analysis <- runBraidAnalysis(measure ~ concA + concB, surface, defaults=c(0,1))

report <- makeBraidReport(analysis, c("A Drug","B Drug"),
                          levels=c(0.5, 0.9), limits=c(5,5))
```
