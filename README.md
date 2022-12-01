
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpdeploy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/amashadihossein/dpdeploy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/amashadihossein/dpdeploy?branch=main)
[![R-CMD-check](https://github.com/amashadihossein/dpdeploy/workflows/R-CMD-check/badge.svg)](https://github.com/amashadihossein/dpdeploy/actions)
<!-- badges: end -->

The goal of `dpdeploy` is to simplify and standardize how a data product
is deployed to a remote data platform or repository.

## Installation

For released version

``` r
remotes::install_github("amashadihossein/dpdeploy")
```

For dev version

``` r
remotes::install_github("amashadihossein/dpdeploy", ref = "dev")
```

## Example

Assuming you have properly configured your project and workflow using
`dpbuild`, only a single call is needed to deploy.

``` r
dpdeploy::dp_deploy(project_path = "<PATH_TO_PROJECT>")
```

## Related documentation

- `daapr`: <https://amashadihossein.github.io/daapr/>
- `dpbuild`: <https://amashadihossein.github.io/dpbuild/>
- `dpi`: <https://amashadihossein.github.io/dpi/>
