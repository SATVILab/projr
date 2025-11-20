# Directory Licenses

Separate licensing for raw data, outputs, and documentation allows for
appropriate intellectual property management across different components
of your research project. This is especially useful when data has
different licensing requirements than code, or when outputs need
specific attribution.

This documentation provides a comprehensive guide to projr’s directory
licensing system, enabling both automated and manual license management.

## Overview

projr allows you to add license files to specific directories in your
project:

- **Raw data directories** (e.g., `raw-data`, `cache`)
- **Output directories** (e.g., `output`, `docs`, `data`)
- **Any labeled directory** in your project configuration

### Two Approaches to Directory Licensing

projr supports two distinct approaches to license management:

1.  **YAML Configuration (Automatic)** - Licenses managed through
    `_projr.yml` and regenerated during builds
2.  **Manual Creation** - Licenses created independently and preserved
    across builds

Both approaches can coexist, with YAML configuration taking precedence
when specified.

## Supported License Types

projr includes templates for common open-source and proprietary
licenses:

| License Type                                 | Common Use Case                    |
|----------------------------------------------|------------------------------------|
| **CC-BY** (Creative Commons Attribution 4.0) | Research data and analysis outputs |
| **CC0** (Creative Commons Zero)              | Public domain dedication for data  |
| **Apache-2.0**                               | Code with patent protection        |
| **MIT**                                      | Permissive code licensing          |
| **Proprietary**                              | Private or restricted-use content  |

**License type variations**: Common variations are automatically
normalized: - `"ccby"`, `"cc-by"` → `"CC-BY"` - `"apache"`,
`"Apache 2.0"` → `"Apache-2.0"` - `"mit"` → `"MIT"`

## YAML Configuration (Automatic Approach)

### Setting License Configurations

Use
[`projr_yml_dir_license_set()`](https://satvilab.github.io/projr/reference/projr_yml_dir_license_set.md)
to configure licenses in `_projr.yml`:

``` r
library(projr)

# Simple format - just specify license type
projr_yml_dir_license_set("CC-BY", "output")

# Full format with custom authors and year
projr_yml_dir_license_set(
  "MIT",
  "raw-data",
  authors = c("Jane Doe", "John Smith"),
  year = 2024
)

# Set licenses for multiple directories
projr_yml_dir_license_set("Apache-2.0", "output")
projr_yml_dir_license_set("Apache-2.0", "docs")
projr_yml_dir_license_set("CC0", "raw-data")
```

### YAMLConfiguration Format

Licenses can be specified in simple or full format in `_projr.yml`:

**Simple format:**

``` yaml
directories:
  output:
    path: _output
    license: CC-BY
  docs:
    path: docs
    license: Apache-2.0
```

**Full format:**

``` yaml
directories:
  raw-data:
    path: _raw_data
    license:
      type: MIT
      authors:
        - Jane Doe
        - John Smith
      year: 2024
```

### Build-Time Behavior

When licenses are configured in YAML:

**Pre-build phase** (input directories): - LICENSE files created/updated
in `raw-data` and `cache` directories - Occurs in **all builds** (both
dev and production) - Files **always regenerated** to match current
configuration - Ensures consistency with project metadata

**Post-build phase** (output directories): - LICENSE files created in
`output`, `docs`, and `data` directories  
- Occurs only in **production builds** (not dev builds) - Files
generated fresh for each build

**Key point**: LICENSE files are **tracked in the manifest** and
versioned with your project, ensuring license information is preserved
across versions.

### Managing YAML Configurations

**Retrieve current configuration:**

``` r
# Get license configuration for a directory
projr_yml_dir_license_get("output")
```

**Remove configuration:**

``` r
# Remove license configuration
projr_yml_dir_license_rm("output")
```

**Update all configurations with current authors:**

``` r
# Update all directories with DESCRIPTION file authors
projr_yml_dir_license_update()

# Update specific directories only
projr_yml_dir_license_update(c("raw-data", "output"))
```

### Author Precedence

When using YAML configuration, authors are determined in the following
order:

1.  **Authors in license configuration** (highest priority)
2.  **Authors from DESCRIPTION file** (if exists)
3.  **“Project Authors” fallback** (lowest priority)

## Manual License Creation

### Creating Manual Licenses

Use
[`projr_license_create_manual()`](https://satvilab.github.io/projr/reference/projr_license_create_manual.md)
to create licenses **outside** of the YAML configuration:

``` r
# Create MIT license in raw-data directory
projr_license_create_manual("MIT", "raw-data")

# Create CC-BY license in all raw data directories
projr_license_create_manual("CC-BY")

# Create with custom authors and year
projr_license_create_manual(
  "Apache-2.0",
  "raw-data",
  authors = c("Jane Doe", "John Smith"),
  year = 2024
)
```

### Manual License Behavior

**Key characteristics**:

- **Not in YAML configuration** - No entry added to `_projr.yml`
- **Build-time preservation** - Never overwritten during builds (unless
  YAML config added)
- **Manual editing allowed** - You can modify the LICENSE file directly
- **YAML override** - If you later add YAML configuration, it takes
  precedence

## Choosing an Approach

**Use YAML configuration when:** - Licenses should stay synchronized
with project metadata - You want automated updates when authors change -
Standard license templates meet your needs

**Use manual creation when:** - You need custom license text or
modifications - Author attribution is complex or frequently changing -
You want manual control over license content

## Example Workflows

### Workflow 1: Automatic Licensing

``` r
library(projr)

# Configure licenses
projr_yml_dir_license_set("CC-BY", "output")
projr_yml_dir_license_set("CC0", "raw-data")
projr_yml_dir_license_set("Apache-2.0", "docs")

# Run build - LICENSE files automatically created
projr_build_dev()
```

### Workflow 2: Manual Licensing

``` r
# Create manual license for raw data
projr_license_create_manual(
  "MIT",
  "raw-data",
  authors = c("Original Researcher", "Data Curator"),
  year = 2020
)

# Build project - manual LICENSE preserved
projr_build_dev()
```

### Workflow 3: Mixed Approach

``` r
# Automatic licensing for outputs
projr_yml_dir_license_set("CC-BY", "output")

# Manual licensing for curated datasets
projr_license_create_manual("MIT", "raw-data")

# Build - automatic licenses regenerate, manual license preserved
projr_build_patch()
```

## Best Practices

1.  **Choose appropriate licenses**: Use CC-BY or CC0 for data,
    Apache-2.0 or MIT for code
2.  **Decide on approach**: YAML for automation, manual for custom
    control
3.  **Be consistent**: Use the same license type across similar
    directories
4.  **Document your choices**: Explain licensing decisions in your
    README

## Troubleshooting

### License Not Created During Build

Check if YAML configuration exists:

``` r
projr_yml_dir_license_get("raw-data")
```

### Manual License Overwritten

Remove YAML configuration to preserve manual edits:

``` r
projr_yml_dir_license_rm("raw-data")
```

### Wrong Authors in License

For YAML licenses, specify authors explicitly or update DESCRIPTION
file:

``` r
projr_yml_dir_license_set(
  "CC-BY",
  "output",
  authors = c("Correct Author")
)
```

## See Also

- **Environment Variables**:
  [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md)
  for build control
- **Build System**:
  [`vignette("design")`](https://satvilab.github.io/projr/articles/design.md)
  for build process details
- **YAML Configuration**:
  [`vignette("concepts")`](https://satvilab.github.io/projr/articles/concepts.md)
  for `_projr.yml` structure
