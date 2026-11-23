# Rebuild an analysis and compare its files with a completed build

Clone a trusted analysis, restore its recorded inputs and run its
production scripts from empty output folders. projr compares the
resulting files with the build's recorded filenames and hashes. The
scripts and hooks run with your usual R permissions. The replay itself
creates no version or release.

## Usage

``` r
projr_reproduce(
  repo,
  branch = NULL,
  version = NULL,
  commit = NULL,
  path = tempfile("projr-reproduce-"),
  destination = "archive"
)
```

## Arguments

- repo:

  A GitHub repository in `owner/repo` form, optionally followed by
  `@branch`, or a local Git repository.

- branch:

  Branch to search. Defaults to the repository's default branch.

- version:

  Exact completed build version, with or without a leading v.

- commit:

  Full or abbreviated commit SHA on the selected branch. Alone, it
  selects the last completed build at or before this commit and runs
  that build's code. With `version`, it selects the code to run against
  that version's inputs and expected outputs.

- path:

  New or empty directory for the clone. By default it is created in the
  R session's temporary directory. Supply a path to keep the results.

- destination:

  GitHub archive title or release tag for recovering missing snapshots
  and inputs. Defaults to `"archive"`.

## Value

A list with `exact` for the whole comparison, a `directories` data frame
with one row per output or documentation directory and counts of
identical, changed, missing and extra files, and a `files` data frame
with each label, relative path and status. The result also identifies
the checkout path, version, code commit, build record commit and
dependency method. `dependencies$packages` lists package names and
versions and `dependencies$pinned` indicates whether the run used a
lockfile. A missing baseline cannot produce an exact match.

## Details

With neither selector, reproduce the latest completed build on the
branch. A version on its own selects that build and its code. With a
lockfile, projr restores its package versions with renv. With only a
DESCRIPTION, pak installs its dependencies and their versions are
unpinned. With neither file, the analysis uses the available R
libraries. The input, output and cache paths used during replay must be
inside the clone. Archives can be stored elsewhere.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- projr_reproduce("owner/study", branch = "analysis", version = "1.2.3")
result$exact
subset(result$files, status != "identical")
} # }
```
