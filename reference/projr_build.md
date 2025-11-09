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
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL
)

projr_build_major(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL
)

projr_build_minor(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL
)

projr_build_patch(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL
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

  list. Arguments passed to the rendering engine
  ([`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
  `quarto::render` or
  [`bookdown::render_book`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)).

- profile:

  character. `projr` profile to use. Will set the environment variable
  .PROFILE\` to this value at the start of the build,

- archive_github:

  `TRUE`,`FALSE` orcharacter vector of directory labels. If `TRUE`, then
  all directories (`raw-data`, `output`, etc) are uploaded to a GitHub
  release named `archive` as versioned files (e.g. `output-v0.1.2.zip`).
  If `FALSE`, then no directories are uploaded. If a character vector,
  then only the directories specified are uploaded. Default is `FALSE`.
  Ignored if there is a release named `archive` already specified as a
  destination in the `projr` configuration file.

- always_archive:

  logical. If `TRUE`, then the directories are uploaded regardless of
  whether the directory to be uploaded has exactly the same contents as
  the latest version of the directory on the GitHub release. Default is
  `TRUE`. Ignored if there is a release named `archive` already
  specified as a destination in the `projr` configuration file.
