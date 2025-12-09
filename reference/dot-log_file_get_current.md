# Get the most recent detailed log file path for the current build

Get the most recent detailed log file path for the current build

## Usage

``` r
.log_file_get_current(build_type = "output", date = NULL)
```

## Arguments

- build_type:

  Character. Either "output" or "dev".

- date:

  Character. Optional date (YYYY-MMM-DD) to restrict search to a single
  date directory. Default is NULL (search across all date directories).

## Value

Character or NULL. Path to the most recent log file, or NULL if none
found.
