# Download directories from another projr analysis

Copy whole directories from a completed build into `out/<label>/`. projr
verifies each file against the build manifest before installing any of
the selected directories.

## Usage

``` r
projr_get(type, ...)

projr_get_local(
  path = NULL,
  remote = NULL,
  label = NULL,
  version = NULL,
  out = ".",
  overwrite = FALSE
)

projr_get_github(
  repo = NULL,
  remote = NULL,
  tag = NULL,
  label = NULL,
  version = NULL,
  out = ".",
  overwrite = FALSE
)
```

## Arguments

- type:

  A source type, `"local"` or `"github"`.

- ...:

  Arguments for `projr_get_local()` or `projr_get_github()`.

- path:

  Path to the local analysis that produced the files.

- remote:

  Title of a remote in the current project's configuration. Supply
  exactly one of this title and the direct source (`path` or `repo`).

- label:

  Directory labels to copy. By default, copy the labels recorded for the
  selected build at the source.

- version:

  Exact build version, with or without a leading `v`. Defaults to the
  producer's completed build or the release's recorded project version.

- out:

  Directory to receive a subdirectory for each label.

- overwrite:

  Allow replacement of changed or extra files in the selected output
  directories. Matching files can be reused with either setting.

- repo:

  A producing GitHub project in `owner/repo` or `owner/repo@branch`
  form. Without a branch, use its default branch.

- tag:

  Concrete GitHub release tag. With a direct repository, a tag
  identifies the release without looking up the producer's
  configuration.

## Value

A named character vector of absolute paths to the verified directories.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_get_local(path = "../analysis", label = "output", out = "downloads")
projr_get_github(repo = "owner/analysis@main", label = "raw-data")
projr_get("github", remote = "reports", version = "1.2.0", out = "downloads")
} # }
```
