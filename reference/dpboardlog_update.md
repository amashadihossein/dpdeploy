# Update dpboard log

Updates the metadata associated with the board and retrievable with
dp_list. When deploying dlog is needed when archiving dp_name and
pin_version are needed.

## Usage

``` r
dpboardlog_update(
  conf,
  git_info,
  dlog = NULL,
  dp_name = character(0),
  pin_version = character(0)
)
```

## Arguments

- conf:

  output of `dpconf_get`

- git_info:

  a list returned from `gitinfo_validate`

- dlog:

  daap_log. This is only needed when adding record

- dp_name:

  name of the pin to be archived. Ignored when dlog is provided.

- pin_version:

  version of the pin to be archived. Ignored when dlog is provided

## Value

TRUE
