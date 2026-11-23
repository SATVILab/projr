# Get change summary for debug output

Similar to .build_change_summary_get() but formatted for console output.

## Usage

``` r
.build_change_summary_get_debug(
  output_run,
  summary = .build_change_summary_get(output_run)
)
```

## Arguments

- output_run:

  Logical. Whether this is an output build.

- summary:

  Change summary lines for this build.

## Value

List with 'has_changes' (logical) and 'message' (character vector).
