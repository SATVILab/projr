# Check active `projr` settings.

Checks correctness of active `projr` settings.

## Usage

``` r
projr_yml_check(profile = NULL)
```

## Arguments

- profile:

  character(). Profile whose file needs to be checked. If not supplied,
  then the merged profile is used. Default is NULL.

## Value

Returns `TRUE` if all checks pass. Otherwise throws an error.
