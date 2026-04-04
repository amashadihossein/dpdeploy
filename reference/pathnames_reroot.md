# Re-root path names

if pathnames are of path format, it sets the root to `new_root` dropping
all upstream paths beyond `new_root`. If not of path format, it keeps
the pathnames unchanged

## Usage

``` r
pathnames_reroot(pathnames, new_root = "input_files")
```

## Arguments

- pathnames:

  a vector of characters to be re-rooted

- new_root:

  a directory relative to which all paths be renamed
