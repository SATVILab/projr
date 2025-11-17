# Show a debug message (only shown at debug level)

Show a debug message (only shown at debug level)

## Usage

``` r
.cli_debug(..., output_level = "std", .envir = parent.frame(), log_file = NULL)
```

## Arguments

- ...:

  Message components passed to cli::cli_text

- output_level:

  Character. Current output level.

- .envir:

  Environment for variable evaluation

- log_file:

  Character. Path to log file (optional).
