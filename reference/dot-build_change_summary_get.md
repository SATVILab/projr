# Get change summary for build

Compares the current build's manifest with the previous version's
manifest to identify what files have changed in input and output
directories.

## Usage

``` r
.build_change_summary_get(output_run)
```

## Arguments

- output_run:

  Logical. Whether this is an output build.

## Value

Character vector of formatted change summary lines, or NULL if no
previous version.
