
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cost-effectiveness analysis of LTBI testing combinations using decision trees

<!-- badges: start -->

<!-- badges: end -->

The goal of LTBIdiagTST is to perform a cost-effectiveness analysis for
Latent TB testing for a range of tests and combinations.

The model is formed of a decision tree and Markov model components.

![alt text](/inst/LTBI-screening-model.png)

Probability sensitivity analysis (PSA) is a key part of this analysis.
The Markov model is implemented using the `heemod` package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("n8thangreen/LTBIdiagTST")
```
