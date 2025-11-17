# Query Files Changed Between Versions

Query which files changed between two project versions based on the
manifest. Returns files that were added, modified, or removed between
the versions.

Query which files changed across a range of versions. For each file,
shows the version where it last changed.

Query when files in each directory last changed. Shows the most recent
version where any file in the directory was modified.

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

  Last version where file was modified

- hash:

  Current file hash

A data.frame with columns:

- label:

  Directory label

- version_last_change:

  Most recent version with changes

- n_files:

  Number of files in directory at this version

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
