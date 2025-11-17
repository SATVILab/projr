# Mark a process as done

Mark a process as done

## Usage

``` r
.cli_process_done(
  id = NULL,
  msg_done = NULL,
  msg_failed = NULL,
  .envir = parent.frame(),
  output_level = "std"
)
```

## Arguments

- id:

  Process ID from cli_process_start

- msg_done:

  Success message

- msg_failed:

  Failure message

- .envir:

  Environment for the process

- output_level:

  Character. Current output level.
