
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
#> Skipping install of 'tdid' from a github remote, the SHA1 (2c5ee114) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

Load a sample based on the DGP of the example in the paper.

``` r
library(tdid)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(fixest)
data = example_dgp(n = 1000, seed = 1)
head(data)
#> # A tibble: 6 × 6
#>      id  time group     W      X     Y
#>   <int> <dbl> <chr> <int>  <dbl> <dbl>
#> 1     1     1 A         0  1.08  1.82 
#> 2     2     1 B         0  2.07  2.46 
#> 3     3     1 A         0 -0.183 1.11 
#> 4     4     1 A         1  1.01  0.208
#> 5     5     1 B         0  3.54  1.94 
#> 6     6     1 A         0  2.59  3.53
```

The causal estimand is equal to three.

Running a three-way linear specification with controls.

``` r
data = data |>
  mutate(P = 1*(time == 2),
         A = 1*(group == "A"))
tdid_linear = coef(lm(Y ~ W*P*A + X, data = data))[9]
cat("Estimated diff in ATT via linear specification: ",round(tdid_linear,2))
#> Estimated diff in ATT via linear specification:  -1.03
```

A difference in DR estimates by group

``` r
dr_A = DRDID::drdid(yname = "Y", tname = "time", idname = "id", dname = "W", xformla = ~X, data = data |> filter(group == "A"))$ATT
dr_B = DRDID::drdid(yname = "Y", tname = "time", idname = "id", dname = "W", xformla = ~X, data = data |> filter(group == "B"))$ATT
diff = as.numeric(dr_A - dr_B)
cat("Estimated diff in ATT via difference in DR estimators within groups: ",round(diff,2))
#> Estimated diff in ATT via difference in DR estimators within groups:  -1.03
```

The DR TDID estimator

``` r
tdid_dr = drtdid(data)$tau
cat("Estimated diff in ATT via difference in DR of group A and reweighted DR of group B: ",round(tdid_dr,2))
#> Estimated diff in ATT via difference in DR of group A and reweighted DR of group B:  2.79
```
