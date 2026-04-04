# Sync Input Data to Remote

Sync input data for a data product build to a remote such as AWS S3

## Usage

``` r
dpinput_sync(conf, input_map, verbose = F, type = "rds", ...)
```

## Arguments

- conf:

  environment containing all config.R variables. See
  [`dpconf_get()`](https://amashadihossein.github.io/dpdeploy/reference/dpconf_get.md)

- input_map:

  object containing all input data to be synced. See `map_input()`

- verbose:

  T/F

- type:

  data format to pin input data to remote, default: rds

- ...:

  other parameters e.g. verbose = T

## Value

synced_map this is input_map with sync status added to metadata
