# Clear build logs

Delete build logs based on specified criteria.

## Usage

``` r
projr_log_clear(
  build_type = "all",
  history = TRUE,
  output = TRUE,
  before_date = NULL,
  before_version = NULL
)
```

## Arguments

- build_type:

  Character. Either "output", "dev", or "all". Default is "all".

- history:

  Logical. Clear history files. Default is TRUE.

- output:

  Logical. Clear output log files. Default is TRUE.

- before_date:

  Character. Clear logs before this date (YYYY-MM-DD). Default is NULL
  (no date filter).

- before_version:

  Character. Clear logs before this version. Default is NULL (no version
  filter).
