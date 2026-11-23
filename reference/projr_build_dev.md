# Build dev project

Runs the selected scripts and renders documents for development.
Development builds preserve the project's version and can run before its
first versioned build. Staged outputs are available in the build cache.

## Usage

``` r
projr_build_dev(
  scripts = NULL,
  bump = FALSE,
  old_dev_remove = TRUE,
  args_engine = list(),
  profile = NULL,
  clear_output = "never",
  project = NULL
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
  scripts are detected in the project root and run alphabetically. When
  every entry is an exclusion, use these automatic rules and skip the
  matching files. In a YAML scripts list, use an entry such as
  `- exclude: scratch.R` or a quoted string such as `"!scratch.R"`.

  Explicit selections run in order, with each glob expanded
  alphabetically. Listing an R script or standalone document twice runs
  it twice. A repeated project document is an error. If a selection
  includes Quarto or Bookdown project documents, those documents render
  together at the position of the last selected project document. The
  project YAML is restored after rendering and post-build hooks.

- bump:

  logical. Retained for compatibility. Supplying `TRUE` warns and runs a
  development build with the project's existing version, if any.

- old_dev_remove:

  logical. If `TRUE`, then previous development builds are deleted after
  a successful run.

- args_engine:

  list. Arguments passed to the rendering engine
  ([`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
  [`quarto::quarto_render`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  or
  [`bookdown::render_book`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)).
  Quarto renders in the foreground so subsequent scripts and hooks see
  the completed output.

- profile:

  character. `projr` profile to use. Will set the environment variable
  .PROFILE\` to this value at the start of the build,

- clear_output:

  character. When to clear output directories: "pre" (before build),
  "post" (after build), or "never" (default for dev builds). Can also be
  set via PROJR_CLEAR_OUTPUT environment variable.

- project:

  Path to the project root. The default discovers the project from the
  working directory. The first interactive build asks permission to run
  its scripts and hooks, change project files and publish configured
  outputs. projr remembers approval for this checkout in the user's R
  configuration directory. In a noninteractive session, pass the root
  explicitly (for example, `project = "."`) to authorize this build
  without saving approval.

## Value

Invisibly returns `TRUE` after a successful build.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_build_dev(scripts = c("reports/*.Rmd", "!reports/draft.Rmd"))
} # }
```
