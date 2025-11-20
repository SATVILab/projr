# Update LICENSE files with current DESCRIPTION authors

Regenerates LICENSE files for directories with existing license
configurations, using authors from the DESCRIPTION file. This is useful
after updating package authors or for propagating authors to raw data
directories.

## Usage

``` r
projr_yml_dir_license_update(labels = NULL, profile = "default")
```

## Arguments

- labels:

  character vector. Directory labels to update. If NULL (default),
  updates all directories with license configurations.

- profile:

  character. Profile to use. Default is "default".

## Value

Invisible character vector of labels that were updated.

## Examples

``` r
if (FALSE) { # \dontrun{
# Update all directories with license configurations
projr_yml_dir_license_update()

# Update specific directories
projr_yml_dir_license_update(c("raw-data", "output"))
} # }
```
