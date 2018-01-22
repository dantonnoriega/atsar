<!-- README.md is generated from README.Rmd. Please edit that file -->
atsar (Applied Time Series Analysis in R)
=========================================

The atsar R package implements Bayesian time series models using STAN, primarily for illustrative purposes and teaching (University of Washington's Fish 507, Winter quarter 2017). The Stan webpage, and appropriate citation guidelines are [here](http://mc-stan.org/). You can cite the package as:

#### Citation: Ward, E.J., M.D. Scheuerell, and E.E. Holmes. 2017. 'statss': Stan for Analyzing Time Series: an introduction to time series analysis for ecological and fisheries data. [![DOI](https://zenodo.org/badge/84236127.svg)](https://zenodo.org/badge/latestdoi/84236127)

You can install the development version of the package with:

``` r
# install.packages("devtools")
devtools::install_github("nwfsc-timeseries/atsar")
```

An example model
----------------

Simulate data:

``` r
library(rstan)
#> Warning: package 'rstan' was built under R version 3.4.3
#> Loading required package: ggplot2
#> Loading required package: StanHeaders
#> Warning: package 'StanHeaders' was built under R version 3.4.3
#> rstan (Version 2.17.3, GitRev: 2e1f913d3ca3)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
library(atsar)
#> Loading required package: Rcpp
#> Warning: package 'Rcpp' was built under R version 3.4.3
set.seed(123)
s = cumsum(rnorm(50))
```

``` r
plot(s)
```

![](README-figs/plot-1.png)

Fit several models to this data:

``` r
# Regression, no slope
regression_model = fit_stan(y = s, x = model.matrix(lm(s~1)), model_name="regression")

# Regression, with slope
regression_model = fit_stan(y = s, x = model.matrix(lm(s~seq(1,length(s)))), model_name="regression")

# AR(1) time series model
ar1_model = fit_stan(y = s, est_drift=FALSE, P = 1, model_name = "ar")

# ARMA(1,1) time series model
arma1_model = fit_stan(y = s, model_name = "arma11")

# univariate ss model -- without drift but mean reversion estimated
ss_model = fit_stan(y = s, model_name = "ss_ar", est_drift=FALSE)
```

References
==========

The 507 class website is here, but will moved shortly because of transitions to Canvas.

[Fish 507 class website](https://catalyst.uw.edu/workspace/fish203/35553/243766)

Additional information can be found on the Github respository which includes several additional books and packages, [Github dashboard](https://github.com/orgs/nwfsc-timeseries/dashboard) ...
