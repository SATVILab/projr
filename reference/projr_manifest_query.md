# Query Files Changed Between Versions

Query which files changed between two project versions based on the
manifest. Returns files that were added, modified, or removed between
the versions.

List the first and last recorded versions of each file in the range,
including builds that reused an earlier directory snapshot.

Show the most recent version that recorded files in each directory,
including builds that reused an earlier directory snapshot.

## Usage

``` r
projr_manifest_changes(version_from = NULL, version_to = NULL, label = NULL)

projr_manifest_range(version_start = NULL, version_end = NULL, label = NULL)

projr_manifest_last_change(version = NULL)
```

## Arguments

- version_from:

  character. Starting version (e.g., "0.0.1" or "v0.0.1"). If NULL, uses
  the earliest version in the manifest.

- version_to:

  character. Ending version (e.g., "0.0.2" or "v0.0.2"). If NULL, uses
  the current project version.

- label:

  character. Optional directory label to filter by.

- version_start:

  character. Starting version (inclusive). If NULL, uses earliest
  version.

- version_end:

  character. Ending version (inclusive). If NULL, uses current version.

- version:

  character. Version to query. If NULL, uses current project version.

## Value

A data.frame with columns:

- label:

  Directory label

- fn:

  File path relative to directory

- change_type:

  Type of change: "added", "modified", or "removed"

- hash_from:

  File hash in version_from (NA for added files)

- hash_to:

  File hash in version_to (NA for removed files)

Returns a 0-row data.frame if no changes found.

A data.frame with columns:

- label:

  Directory label

- fn:

  File path relative to directory

- version_first:

  First version where file appeared

- version_last_change:

  Last version in the range that recorded this file

- hash:

  File hash at that last version

A data.frame with columns:

- label:

  Directory label

- version_last_change:

  Most recent version that recorded files in this directory

- n_files:

  Number of files in directory at this version

## Details

Run these queries in the project checkout whose history you want to
inspect. Production builds record each directory's history under
`.projr/manifests/`. Builds with identical filenames and file contents
share one file list for that directory. Older projects can also be
queried from their root `manifest.csv`.

To retrieve the files recorded for a build, use
[`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md)
or
[`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Query changes between v0.0.1 and v0.0.2
projr_manifest_changes("0.0.1", "0.0.2")

# Query changes in output directory only
projr_manifest_changes("0.0.1", "0.0.2", label = "output")

# Query changes from earliest version to current
projr_manifest_changes()
} # }

if (FALSE) { # \dontrun{
# Query all changes from v0.0.1 to current
projr_manifest_range("0.0.1")

# Query changes in a specific range
projr_manifest_range("0.0.1", "0.0.5")
} # }

if (FALSE) { # \dontrun{
# Query last changes for current version
projr_manifest_last_change()

# Query last changes as of v0.0.5
projr_manifest_last_change("0.0.5")
} # }
```
