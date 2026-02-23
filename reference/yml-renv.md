# Set renv snapshot options

`projr_yml_renv_set` sets renv snapshot options for the project.

The option controls whether
[`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html)
is called before and after project builds.

If the setting is not present in `_projr.yml`, then the default is
`TRUE` (renv snapshots are performed).

## Usage

``` r
projr_yml_renv_set(renv = NULL, simplify_default = TRUE, profile = "default")
```

## Arguments

- renv:

  logical. Whether to snapshot renv before and after builds. If `NULL`,
  then setting is not changed. Default is `NULL`.

- simplify_default:

  logical. If `TRUE`, then if the setting is the same as the default
  (which is `TRUE`), then the setting is removed from `_projr.yml`.
  Default is `TRUE`.

- profile:

  character. Profile to add the setting to. If `"default"` (the
  default), the setting is added to the default profile, which is
  `_projr.yml`. If `NULL`, then the active profile is used (i.e the
  merge of `_projr-local.yml`, `_projr-<profile>.yml` and `_projr.yml`)
  and written to `_projr.yml`. If another character vector, then the
  corresponding profile is used and written to `_projr-<profile>.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
# enable renv snapshots (default)
projr_yml_renv_set(TRUE)

# disable renv snapshots
projr_yml_renv_set(FALSE)

# revert to default (removes setting from YAML)
projr_yml_renv_set(TRUE, simplify_default = TRUE)
} # }
```
