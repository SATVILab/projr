# Get license configuration for a directory

Get license configuration for a specific directory label from
`_projr.yml`.

## Usage

``` r
projr_yml_dir_license_get(label, profile = "default")
```

## Arguments

- label:

  character. Directory label (e.g., "output", "raw-data", "docs",
  "cache").

- profile:

  character. Profile to use. Default is "default".

## Value

License configuration (character or list) or NULL if no license is
configured.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get license for output directory
projr_yml_dir_license_get("output")
} # }
```
