# Get project parameters

Get project parameters from `param` key in `projr` configuration.

## Usage

``` r
projr_par_get(..., profile = NULL)

projr_param_get(..., profile = NULL)
```

## Arguments

- ...:

  character. Sequential names to specify path in list. For example,
  .param_get("a", "b")`returns the value of`projr\$param\$a\$b\`.

- profile:

  character. If `NULL`, then the active profile is used. Default is
  `NULL`.
