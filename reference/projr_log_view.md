# View build log (last n lines)

Display the last N lines of a detailed build log file.

## Usage

``` r
projr_log_view(
  log_file = NULL,
  build_type = "auto",
  n_lines = 10,
  show_header = TRUE
)
```

## Arguments

- log_file:

  Character. Path to a log file. If NULL, the most recent log file
  across both output and dev builds will be used. To view a specific
  build type, use the build_type parameter.

- build_type:

  Character. Either "output", "dev", or "auto" (default). When "auto",
  selects the most recent log across both types. When "output" or "dev",
  selects the most recent log of that specific type.

- n_lines:

  Integer. Number of lines to show from the end of the file. Default
  is 10. Set to NULL or NA to show the entire file.

- show_header:

  Logical. Whether to print a short header including the logfile path
  and last modification time. Default is TRUE.
