# Show a standard informational message

Show a standard informational message

## Usage

``` r
.cli_message(
  ...,
  log_type,
  console_level = "std",
  console_fn,
  .envir = parent.frame()
)
```

## Arguments

- ...:

  Message components passed to cli::cli_alert_info

- log_type:

  Character. Type of log message ("info", "debug", etc.).

- console_level:

  Character. Minimum output level to show in console.

- console_fn:

  Function. CLI function to use for console output.

- .envir:

  Environment for variable evaluation
