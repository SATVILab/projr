# Set environment variables from files

Activate environment variables by reading default values from a set of
files. If `file` is `NULL`, all existing files are used in order of
decreasing priority: first `_environment.local` (machine-specific
overrides), then any `_environment-<profile>` files (profile-specific),
and finally `_environment` (global defaults).

The profiles activated are those set in the `QUARTO_PROFILE` and the
`PROJR_PROFILE` environment variables, with `QUARTO_PROFILE` priorities.
The `QUARTO_PROFILE` variable can specify multiple profiles, separated
by commas. The `PROJR_PROFILE` variable can also specify multiple
profiles, separated by either commas or semi-colons. In both cases, the
earlier profile takes precedence, e.g. `QUARTO_PROFILE=test,basic` will
activate the `test` profile first.

## Usage

``` r
projr_env_set(file = NULL)
```

## Arguments

- file:

  character vector. Paths to files with environment variables to
  activate. If provided, only these files will be read; otherwise the
  default set (`_environment.local`, `_environment-<profile>`,
  `_environment`) is used.

## Value

Invisibly returns `TRUE` if any files were successfully activated, or
`FALSE` if none existed.

## Examples

``` r
# Activate only the local overrides
if (FALSE) { # \dontrun{
  projr_env_set("_environment.local")
} # }
# Activate all available defaults in the standard order
if (FALSE) { # \dontrun{
  projr_env_set()
} # }
```
