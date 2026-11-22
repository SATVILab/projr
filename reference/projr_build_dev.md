# Build dev project

Builds project to output, which means recording the input and output
data hashes, building the actual bookdown document and saving and
archiving selected output.

## Usage

``` r
projr_build_dev(
  scripts = NULL,
  bump = FALSE,
  old_dev_remove = TRUE,
  args_engine = list(),
  profile = NULL,
  clear_output = "never"
)
```

## Arguments

- scripts:

  character vector. Paths or glob patterns for scripts to build,
  relative to the project root or absolute. Prefix a pattern with `!` to
  exclude matches. Use an empty character vector or `""` to skip running
  scripts. The default, `NULL`, checks `dev.scripts` and then
  `build.scripts`. If neither is set, a Quarto or Bookdown project
  renders before automatically detected R scripts. Without a project,
  scripts are detected in the project root and run alphabetically.

  Explicit selections run in order, with each glob expanded
  alphabetically. If a selection includes Quarto or Bookdown project
  documents, those documents render together at the position of the last
  selected project document. The project YAML is restored before the
  build returns to the development version.

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

## Value

Invisibly returns `TRUE` after a successful build.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_build_dev(scripts = c("reports/*.Rmd", "!reports/draft.Rmd"))
} # }
```
