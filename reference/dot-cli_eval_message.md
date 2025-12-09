# Evaluate glue expressions in message

Evaluate glue expressions in message

## Usage

``` r
.cli_eval_message(..., .envir = parent.frame())
```

## Arguments

- ...:

  Message components that may contain glue expressions. Can include
  named arguments for glue substitution.

- .envir:

  Environment for variable evaluation

## Value

Character. The evaluated message string.
