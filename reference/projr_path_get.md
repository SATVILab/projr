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

  character. One of `"raw_data"`, `"cache"`,`"output"`, `"archive"` and
  `"docs"`. Class of directory to return. The `"docs"` option returns
  the path to the output directory from
  [`bookdown::render_book`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)
  (as specified in `"_bookdown.yml"`), whereas the others returns paths
  as specified in `"_projr.yml"`.

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

  logical. If `TRUE`, then the output directory is set to be
  `"<path_to_cache>.output"` instead of `<path_to_output>` (as specified
  in `_projr.yml`). The only time that this should be set to `TRUE`
  should be when
  .build_output` is being run, as otherwise "development" or test runs will add to, delete or overwrite fabciles from the previous run of .build_output`.
  Default is `TRUE`. Do not change this unless you know what you are
  doing.

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
