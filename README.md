
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpdeploy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `dpdeploy` is to simplify and standardize how a data product
is deployed to a remote data platform or repository.

## Installation

For released version

``` r
remotes::install_git(url = "https://github.com/amashadihossein/dpdeploy.git")
```

For dev version

``` r
remotes::install_git(url = "https://github.com/amashadihossein/dpdeploy.git",
                     ref = "dev")
```

## Example

Assuming you have properly configured your project and workflow using
`dpbuild`, only a single call is needed to deploy.

``` r
library(dpdeploy)
dpdeploy::dp_deploy(project_path = "<PATH_TO_PROJECT>")
```
