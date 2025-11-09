# Return path to profile-specific directory

Returns path to `projr` profile-specific directory. Also creates the
directory if it does not exist, and ignores it if requested by
`_projr.yml`.

## Usage

``` r
projr_path_get_dir(
  label,
  ...,
  create = TRUE,
  relative = FALSE,
  absolute = FALSE,
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

- relative:

  logical. If `TRUE`, then forces that the returned path is relative to
  the project root. Default is `FALSE`.

- absolute:

  logical. If `TRUE`, then forces the returned path to be absolute.
  Default is `FALSE`.

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
