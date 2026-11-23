# Read project settings

With `profile = NULL`, local settings in `_projr-local.yml` override the
active profile settings, which override the defaults in `_projr.yml`.
Supply a profile name to read its own configuration.

## Usage

``` r
projr_yml_get(profile = NULL, check = FALSE)
```

## Arguments

- profile:

  A profile name, or `NULL` for the merged active settings. Use
  `"default"` to read the base configuration, `"local"` to read local
  overrides, or a named profile to read `_projr-<profile>.yml`.

- check:

  Whether to validate the settings and report an error if they are
  invalid. Defaults to `FALSE`.

## Value

A named list of project settings.

## See also

[`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
