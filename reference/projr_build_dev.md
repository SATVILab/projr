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
  pipeline = NULL,
  clear_output = "never",
  project = NULL
)
```

## Arguments

- scripts:

  character vector. Paths or glob patterns for scripts to build,
  relative to the project root or absolute. Prefix a pattern with `!` to
  exclude matches. Use an empty character vector or `""` to skip running
  scripts. With a pipeline selected, these scripts take the place of
  `dev.scripts` and combine with the pipeline's; add `pipeline = FALSE`
  to run only them. The default, `NULL`, checks `dev.scripts` and then
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

  List of options for the selected build engines. For one engine, use
  its native arguments directly, e.g. `list(quiet = TRUE)` for one or
  more standalone R Markdown documents. When multiple engines run, group
  options by engine, e.g.
  `list(quarto_project = list(profile = "print"), rmd = list(quiet = TRUE))`.
  Use `quarto_project` or `quarto_document` for
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html),
  `rmd` for
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  and `bookdown` for
  [`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html).
  Plain R scripts (`r`) take no render options. Omit any engine to use
  its defaults. The default [`list()`](https://rdrr.io/r/base/list.html)
  works for any selection. Quarto renders in the foreground so later
  scripts and hooks see its output.

- profile:

  character. `projr` profile to use. Sets `PROJR_PROFILE` for the build,
  so it overrides that variable and the `profile` key in
  `_projr-local.yml` and `_projr.yml`.

- pipeline:

  character. Names of pipelines under `pipelines` to run in order, or
  `FALSE` to run none. Entries can be globs; a glob's matches run in
  their order under `pipelines`, and each pipeline runs once. Overrides
  `build.pipeline` (or `dev.pipeline` for development builds) for this
  build. Inline `scripts` and `hooks`, and the `scripts` argument of
  `projr_build_dev()`, still combine with it. Default is `NULL`, using
  the configured selection.

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
