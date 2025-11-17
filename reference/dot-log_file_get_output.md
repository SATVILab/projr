# Get output log file path for current build

Get output log file path for current build

## Usage

``` r
.log_file_get_output(build_type = "output", timestamp = NULL)
```

## Arguments

- build_type:

  Character. Either "output" or "dev".

- timestamp:

  Character. Timestamp in HH-MM-SS format. Default is now.

## Value

Character. Path to the log file for this build.
