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
  clear_output = "never",
  output_level = NULL
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

- clear_output:

  character. When to clear output directories: "pre" (before build),
  "post" (after build), or "never" (default for dev builds). Can also be
  set via PROJR_CLEAR_OUTPUT environment variable.

- output_level:

  character. Level of CLI output: "none" (no additional messages,
  default for dev builds), "std" (standard messages), or "debug"
  (verbose messages for debugging). Can also be set via
  PROJR_OUTPUT_LEVEL environment variable.
