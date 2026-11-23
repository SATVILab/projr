# Return path

Returns path to `projr` profile-specific directory. Differs from
`projr_dir_get` in that it does not assume that the path is to a
directory.

Will create the parent directory of the specified path if it does not
exist, and ignore it if requested by `_projr.yml`.

## Usage

``` r
projr_path_get(
  label,
  ...,
  create = TRUE,
  format = c("auto", "relative", "absolute"),
  safe = TRUE
)
```

## Arguments

- label:

  character. A directory label declared under `directories` in the
  active project configuration.

- ...:

  Specifies sub-path of directory returned.

- create:

  logical. If `TRUE`, then the parent directory is created if it does
  not exist and it is ignored (or not) from `.gitignore` and
  `.Rbuildignore` as specified in `_projr.yml`. Default is `TRUE`.

- format:

  character. One of `"auto"`, `"relative"` or `"absolute"`. If `"auto"`,
  then the format of the path returned is inherited from the format of
  the path specified in `_projr.yml`. If `"relative"`, then the path
  returned is relative to the current working directory. If
  `"absolute"`, then the path returned is absolute.

- safe:

  logical. When `TRUE` (the default), stage outputs, documentation and
  package data under the configured cache for the current build version.
  When `FALSE`, return their configured final paths. Inputs and cache
  directories use their configured paths.

## Value

Character. Path to directory requested.

## Details

DETAILS

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  # EXAMPLE1
}
} # }
```
