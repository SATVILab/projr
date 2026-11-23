# Read project settings without validation

With `profile = NULL`, local settings in `_projr-local.yml` override the
active profile settings, which override the defaults in `_projr.yml`.
Supply a profile name to read its own configuration.

## Usage

``` r
.yml_get(profile)
```

## Arguments

- profile:

  A profile name, or `NULL` for the merged active settings.

## Value

A named list of project settings.

## See also

[`projr_yml_get()`](https://satvilab.github.io/projr/reference/projr_yml_get.md),
[`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
