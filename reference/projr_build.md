# Build project to output

.build_output\` Builds project to output, which means recording the
input and output data hashes, building the actual bookdown document and
saving and archiving selected output.

.build_major`, .build_minor` and
.build_patch` are wrappers around .build_output` with the version
component bumped set automatically, e.g.
projr_build_major()` is equivalent projr_build(bump_component = "major")`.

## Usage

``` r
projr_build(
  bump_component,
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  pipeline = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
)

projr_build_major(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  pipeline = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
)

projr_build_minor(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  pipeline = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
)

projr_build_patch(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  pipeline = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
)
```

## Arguments

- bump_component:

  "major", "minor", "patch" or missing. Specifies version component to
  increment. If missing, then is set equal to the lowest version
  component in used version format. No default (i.e. is missing by
  default).

- msg:

  character. Message to append to Git commit messages. Default is
  `NULL`, in which case the user is prompted for a message or, if the
  session is not interactive, it is left empty. Default is `NULL`. Note
  that the Git messages in this case would not be blank - they would
  simply consist of details as to the version being bumped to and the
  stage in the build process at which the commit was made.

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
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md),
  still combine with it. Default is `NULL`, using the configured
  selection.

- archive_github:

  `TRUE`,`FALSE` orcharacter vector of directory labels. If `TRUE`, then
  all directories (`raw-data`, `output`, etc) are uploaded to a GitHub
  release named `archive` as versioned files (e.g. `output-v0.1.2.zip`).
  If `FALSE`, then no directories are uploaded. If a character vector,
  then only the directories specified are uploaded. Default is `FALSE`.
  Ignored if a release named `archive` is already selected under
  `build.dest` in the `projr` configuration file. Otherwise,
  `remotes.github.archive` is registered in `_projr.yml` if absent,
  without adding it to `build.dest`.

- archive_local:

  `TRUE`, `FALSE` or character vector of directory labels. If `TRUE`,
  then all directories (`raw-data`, `output`, etc) are archived to a
  local directory. If `FALSE`, then no directories are archived locally.
  If a character vector, then only the directories specified are
  archived. Default is `FALSE`. When used, `remotes.local.archive` (with
  `path: _archive`) is registered in `_projr.yml` if absent, without
  adding it to `build.dest`.

- always_archive:

  logical. If `TRUE`, then the directories are uploaded regardless of
  whether the directory to be uploaded has exactly the same contents as
  the latest version of the directory on the GitHub release. Default is
  `TRUE`. Ignored if a release named `archive` is already selected under
  `build.dest` in the `projr` configuration file.

- clear_output:

  character. When to clear output directories: "pre" (before build,
  default), "post" (after build), or "never". Can also be set via
  PROJR_CLEAR_OUTPUT environment variable.

- project:

  Path to the project root. The default discovers the project from the
  working directory. The first interactive build asks permission to run
  its scripts and hooks, change project files and publish configured
  outputs. projr remembers approval for this checkout in the user's R
  configuration directory. In a noninteractive session, pass the root
  explicitly (for example, `project = "."`) to authorize this build
  without saving approval.
