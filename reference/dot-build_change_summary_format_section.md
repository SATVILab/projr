# Format a section of the change summary (inputs or outputs)

Format a section of the change summary (inputs or outputs)

## Usage

``` r
.build_change_summary_format_section(
  section_name,
  labels,
  manifest_all,
  version_prev,
  version_curr
)
```

## Arguments

- section_name:

  Character. Name of the section ("Inputs" or "Outputs").

- labels:

  Character vector. Directory labels to check.

- manifest_all:

  Data frame. The full manifest.

- version_prev:

  Character. Previous version (no 'v' prefix).

- version_curr:

  Character. Current version (no 'v' prefix).

## Value

Character vector of formatted lines for this section.
