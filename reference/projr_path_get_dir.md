# Return path to profile-specific directory

Returns path to `projr` profile-specific directory. Also creates the
directory if it does not exist, and ignores it if requested by
`_projr.yml`. If the path is relative, then it is calculated with
respect to the project directory, but returned relative to the current
working directory.

## Usage

``` r
projr_path_get_dir(
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
  active project configuration. Each label must have a `path`, which may
  be inherited from another profile.

- ...:

  Specifies sub-directory of directory returned. Passed to `file.path`.

- create:

  logical. If `TRUE`, then the directory is created if it does not exist
  and it is ignored (or not) from `.gitignore` and `.Rbuildignore` as
  specified in `_projr.yml`. Default is `TRUE`.

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
