# Get the previous version from manifest

Get the previous version from manifest

## Usage

``` r
.build_change_summary_get_previous_version(
  version_current = projr_version_get()
)
```

## Arguments

- version_current:

  Project version to compare.

## Value

Character version string (without 'v' prefix), or NULL if no previous
version.
