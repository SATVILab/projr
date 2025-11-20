# Set directory path

`projr_yml_dir_path_set` sets the path for a directory label in the
project.

`projr_yml_dir_path_rm` removes the custom path setting for a directory,
reverting to default behavior.

## Usage

``` r
projr_yml_dir_path_set(label, path, profile = "default")

projr_yml_dir_path_rm(label, profile = "default")
```

## Arguments

- label:

  character. Directory label to configure (e.g., "output", "raw-data",
  "cache"). Must be a valid directory label.

- path:

  character. Path to the directory.

- profile:

  character. Profile to modify. If `"default"` (the default), modifies
  the default profile (`_projr.yml`). If another character vector,
  modifies `_projr-<profile>.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set path for output directory
projr_yml_dir_path_set("output", "_my_output")

# Set path for cache directory
projr_yml_dir_path_set("cache", "_my_cache")

# Revert to default path
projr_yml_dir_path_rm("output")
} # }
```
