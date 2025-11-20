# Set license for a directory

Set license configuration for a specific directory label in
`_projr.yml`. Licenses are automatically generated during builds.

## Usage

``` r
projr_yml_dir_license_set(
  type,
  label,
  authors = NULL,
  year = NULL,
  profile = "default"
)
```

## Arguments

- type:

  character. License type. Supported types: "CC-BY", "CC0",
  "Apache-2.0", "MIT", "Proprietary". Common variations are also
  accepted (e.g., "ccby", "apache", "mit").

- label:

  character. Directory label (e.g., "output", "raw-data", "docs",
  "cache").

- authors:

  character vector. Authors or copyright holders. If NULL, attempts to
  get from DESCRIPTION file or uses "Project Authors" as default.

- year:

  integer. Copyright year. If NULL, uses current year.

- profile:

  character. Profile to use. Default is "default".

## Value

Invisible TRUE if license was set successfully.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple format - just license type
projr_yml_dir_license_set("CC-BY", "output")

# Full format with custom authors and year
projr_yml_dir_license_set(
  "MIT",
  "raw-data",
  authors = c("Jane Doe", "John Smith"),
  year = 2024
)

# Set license for multiple directories
projr_yml_dir_license_set("Apache-2.0", "output")
projr_yml_dir_license_set("Apache-2.0", "docs")
} # }
```
