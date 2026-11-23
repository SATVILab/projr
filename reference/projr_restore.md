# Restore the files used by a checked-out build

`projr_restore()` restores raw inputs from the GitHub release published
by `projr_build_patch(archive_github = TRUE)` and the other production
builds. Run it inside the checked-out project. It verifies file paths
and MD5s before installing the inputs. Files already matching the
recorded snapshot are kept.

## Usage

``` r
projr_restore(
  label = NULL,
  version = NULL,
  destination = "archive",
  clear = FALSE
)
```

## Arguments

- label:

  character vector or NULL. Specifies labels of artefacts to restore.
  Default is `NULL`, restoring all `raw` artefacts (e.g. `raw-data`).
  Must be NULL or a non-empty character vector with valid directory
  labels.

- version:

  Character. An exact production build version. By default projr reads
  the checkout's version. On the follow-up development commit it uses
  the most recent completed build in the checkout's
  `.projr/BUILDLOG.md`.

- destination:

  Character. Concrete GitHub release tag in the checkout's repository.
  The default is `"archive"`. For templated releases, use the tag
  recorded in the build log.

- clear:

  Logical. Allow replacement of existing input files that differ from
  the snapshot, including extra files. The default fills missing files
  and stops if local changes would be lost.

## Value

Invisibly returns `TRUE` after restoring and verifying the selected
directories, or `FALSE` if the checkout declares no raw directories.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_restore()
projr_restore(label = "raw-measurements", version = "1.2.3")
} # }
```
