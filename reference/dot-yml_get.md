# Get active `projr` settings and do no check

A list of the active `projr` settings, without doing any error checking.
Gets active `projr` settings, which merges settings and resolves
conflicts between local (`_projr-local.yml`), profile
(`_projr-<projr>.yml` or as - keys in `_projr.yml`) and default
(`_projr.yml`) settings. Where there are conflicts, local settings has
highest precedence (i.e. are always preferred) and default settings have
lowest precedence (i.e. are never preferred).

## Usage

``` r
.yml_get(profile)
```

## Arguments

- profile:

  character. If supplied, the specific profile file to read in.
  "default" loads `_projr.yml`, but another value loads
  `_projr-<profile>.yml`. If NULL, then the active profile is used. It
  not supplied, then treated as `NULL`.

## Value

A named list.

## See also

.yml_get.yml_check
