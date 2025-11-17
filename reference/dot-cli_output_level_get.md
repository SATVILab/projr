# Get the current output level

Get the current output level

## Usage

``` r
.cli_output_level_get(output_level = NULL, output_run = FALSE)
```

## Arguments

- output_level:

  Character. Explicit output level ("none", "std", "debug"). If NULL,
  determined from environment variable PROJR_OUTPUT_LEVEL.

- output_run:

  Logical. Whether this is an output build (vs dev build). Used to set
  default if output_level is NULL.

## Value

Character. The output level to use.
