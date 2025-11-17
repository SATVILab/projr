# Format change summary for a single label

Format change summary for a single label

## Usage

``` r
.build_change_summary_format_label(
  label,
  change_list,
  n_added,
  n_removed,
  n_modified,
  n_unchanged
)
```

## Arguments

- label:

  Character. Directory label.

- change_list:

  List. Output from .change_get_hash().

- n_added:

  Integer. Number of added files.

- n_removed:

  Integer. Number of removed files.

- n_modified:

  Integer. Number of modified files.

- n_unchanged:

  Integer. Number of unchanged files.

## Value

Character vector of formatted lines.
