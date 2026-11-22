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

  character. One of `"raw"`, `"cache"`,`"output"`, `"archive"` and
  `"docs"`. Class of directory to return. The `"docs"` option returns
  the path to the output directory from
  [`bookdown::render_book`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)
  (as specified in `"_bookdown.yml"`), whereas the others returns paths
  as specified in `"_projr.yml"`.

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

  logical. If `TRUE`, then the output directory is set to be
  `"<path_to_cache>.output"` instead of `<path_to_output>` (as specified
  in `_projr.yml`). The only time that this should be set to `TRUE`
  should be when
  .build_output` is being run, as otherwise "development" or test runs will add to, delete or overwrite fabciles from the previous run of .build_output`.
  Default is `TRUE`. Do not change this unless you know what you are
  doing.

## Value

Character. Path to directory requested.
