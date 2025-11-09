# Build dev project

Builds project to output, which means recording the input and output
data hashes, building the actual bookdown document and saving and
archiving selected output.

## Usage

``` r
projr_build_dev(
  file = NULL,
  bump = FALSE,
  old_dev_remove = TRUE,
  args_engine = list(),
  profile = NULL,
  clear_output = "never"
)
```

## Arguments

- file:

  character vector. Paths to files to build. Paths may be relative to
  project root, or absolute. Default is NULL, in which case all files
  are built.

- bump:

  logical. Whether to increment dev version for build. Default is
  `FALSE`.

- old_dev_remove:

  logical. If `TRUE`, then previous development builds are deleted after
  a successful run.

- args_engine:

  list. Arguments passed to the rendering engine
  ([`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
  `quarto::render` or
  [`bookdown::render_book`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)).

- profile:

  character. `projr` profile to use. Will set the environment variable
  .PROFILE\` to this value at the start of the build,
