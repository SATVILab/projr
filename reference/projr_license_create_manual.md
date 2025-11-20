# Manually create LICENSE files without YAML configuration

Creates LICENSE files in directories without adding license
configuration to `_projr.yml`. This allows manual editing of licenses
without them being overwritten during builds. If a license configuration
exists in the YAML for a directory, it will take precedence and
overwrite the manual license.

## Usage

``` r
projr_license_create_manual(
  type,
  labels = NULL,
  authors = NULL,
  year = NULL,
  profile = "default"
)
```

## Arguments

- type:

  character. License type. Supported types: "CC-BY", "CC0",
  "Apache-2.0", "MIT", "Proprietary".

- labels:

  character vector. Directory labels to create licenses for. If NULL,
  creates for all raw data directories (raw-data, cache).

- authors:

  character vector. Authors or copyright holders. If NULL, attempts to
  get from DESCRIPTION file or uses "Project Authors" as default.

- year:

  integer. Copyright year. If NULL, uses current year.

- profile:

  character. Profile to use. Default is "default".

## Value

Invisible character vector of labels where licenses were created.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create MIT license in raw-data directory
projr_license_create_manual("MIT", "raw-data")

# Create CC-BY license in all raw data directories
projr_license_create_manual("CC-BY")

# Create with custom authors
projr_license_create_manual(
  "Apache-2.0",
  "raw-data",
  authors = c("Jane Doe", "John Smith"),
  year = 2024
)
} # }
```
