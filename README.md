
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
```

## Example

An example, as well as replication of the graph from the paper, is given
in the [example
vignette](https://dorleventer.github.io/tdid/articles/tdid-example.html).

## In Progress

Currently this is a bare-bones package repo for replication of the
results in the paper. In the future will upgrade such that

- [ ] Handle different names from outcome, time, …
- [ ] Allow inputting covariate formulas
- [ ] Useage of different ML learners for the propensity score and
  outcome regression
- [ ] Cross-fitting of the propensity score and outcome regression
