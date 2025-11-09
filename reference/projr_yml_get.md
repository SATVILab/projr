# Get active `projr` settings and checks for validity

Gets active `projr` settings, which merges settings and resolves
conflicts between local (`_projr-local.yml`), profile
(`_projr-<projr>.yml` or as - keys in `_projr.yml`) and default
(`_projr.yml`) settings. Where there are conflicts, local settings has
highest precedence (i.e. are always preferred) and default settings have
lowest precedence (i.e. are never preferred).

Note that an error is thrown if the active settings are invalid.

## Usage

``` r
projr_yml_get(profile = NULL, check = FALSE)
```

## Arguments

- profile:

  character. `projr` profile to use. If `NULL`, then the active profile
  is used. Default is `NULL`.

- check:

  logical. Whether to check the validity of the settings. Default is
  `FALSE`.

## Value

A named list, if the settings are valid.

## See also

.yml_get_unchecked.yml_check
