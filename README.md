
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tdid

<!-- badges: start -->
<!-- badges: end -->

These R functions calculate triple difference-in-differences (TDID) with
controls using double-robust estimators.

## Installation

You can install the development version of tdid from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("dorleventer/tdid")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'tdid' from a github remote, the SHA1 (b3dbc96a) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

Load a sample based on the DGP of the example in the paper.

``` r
# library(tdid)
# library(tidyverse)
# library(fixest)
# data = example_dgp(n = 1000, seed = 1)
# head(data)
```

The causal estimand is equal to three.

Running a three-way linear specification with controls.

``` r
# data = data |> 
#   mutate(P = 1*(time == 1), 
#          A = 1*(group == "A"))
# tdid_linear = coef(lm(Y ~ W*P*A + X, data = data))[9]
# cat("Estimated diff in ATT via linear specification: ",round(tdid_linear,2))
```

A difference in DR estimates by group

``` r
# dr_A = DRDID::drdid(yname = "Y", tname = "time", idname = "id", dname = "W", xformla = ~X, data = data |> filter(group == "A"))$ATT
# dr_B = DRDID::drdid(yname = "Y", tname = "time", idname = "id", dname = "W", xformla = ~X, data = data |> filter(group == "B"))$ATT
# diff = as.numeric(dr_A - dr_B)
# cat("Estimated diff in ATT via difference in DR estimators within groups: ",round(diff,2))
```

The DR TDID estimator

``` r
# tdid_dr = drtdid(data)$tau
# cat("Estimated diff in ATT via difference in DR of group A and reweighted DR of group B: ",round(tdid_dr,2))
```
