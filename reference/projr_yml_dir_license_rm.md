# Remove license configuration for a directory

Remove license configuration for a specific directory label from
`_projr.yml`.

## Usage

``` r
projr_yml_dir_license_rm(label, profile = "default")
```

## Arguments

- label:

  character. Directory label (e.g., "output", "raw-data", "docs",
  "cache").

- profile:

  character. Profile to use. Default is "default".

## Value

Invisible TRUE if license configuration was removed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove license configuration for output directory
projr_yml_dir_license_rm("output")
} # }
```
