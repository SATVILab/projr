# Query When a Specific File Last Changed

Query when a specific file last changed in the manifest. Returns the
most recent version where the file's hash was different from the
previous version, or when it first appeared.

Check if a specific file changed between two versions. Returns TRUE if
the file's hash is different between the versions, was added, or was
removed.

Get all versions where a specific file changed or appeared. Returns a
chronological list of all versions in the manifest where the file's hash
is different from the previous version.

Get the version when a specific file first appeared in the manifest.

## Usage

``` r
projr_manifest_file_last_change(fn, label = NULL, version_end = NULL)

projr_manifest_file_changed(
  fn,
  label = NULL,
  version_from = NULL,
  version_to = NULL
)

projr_manifest_file_history(fn, label = NULL)

projr_manifest_file_first(fn, label = NULL)
```

## Arguments

- fn:

  character. File path relative to the directory (e.g., "data.csv",
  "subdir/file.txt").

- label:

  character. Directory label (e.g., "output", "raw-data"). If NULL,
  searches all directories for the file.

- version_end:

  character. End version to search up to. If NULL, uses current project
  version.

- version_from:

  character. Starting version (e.g., "0.0.1" or "v0.0.1"). If NULL, uses
  the earliest version in the manifest.

- version_to:

  character. Ending version (e.g., "0.0.2" or "v0.0.2"). If NULL, uses
  the current project version.

## Value

A data.frame with columns:

- label:

  Directory label

- fn:

  File path

- version_last_change:

  Version when file last changed

- hash:

  Current file hash at that version

Returns a 0-row data.frame if file not found.

A data.frame with columns:

- label:

  Directory label

- fn:

  File path

- changed:

  Logical - TRUE if file changed

- change_type:

  Type of change: "added", "modified", "removed", or "unchanged"

- hash_from:

  File hash in version_from (NA for added files)

- hash_to:

  File hash in version_to (NA for removed files)

Returns a 0-row data.frame if file not found in either version.

A data.frame with columns:

- label:

  Directory label

- fn:

  File path

- version:

  Version where file changed or appeared

- hash:

  File hash at this version

- change_type:

  Type of change: "first_appearance", "modified", or "current"

Returns a 0-row data.frame if file not found.

A data.frame with columns:

- label:

  Directory label

- fn:

  File path

- version_first:

  Version when file first appeared

- hash:

  File hash at first appearance

Returns a 0-row data.frame if file not found.

## Examples

``` r
if (FALSE) { # \dontrun{
# Query when a specific file last changed
projr_manifest_file_last_change("data.csv", label = "output")

# Search all directories for a file
projr_manifest_file_last_change("report.pdf")
} # }

if (FALSE) { # \dontrun{
# Check if a file changed between versions
projr_manifest_file_changed("data.csv", "output", "0.0.1", "0.0.2")

# Check against current version
projr_manifest_file_changed("data.csv", "output", "0.0.1")
} # }

if (FALSE) { # \dontrun{
# Get full history for a file
projr_manifest_file_history("data.csv", label = "output")

# Search all directories
projr_manifest_file_history("config.yml")
} # }

if (FALSE) { # \dontrun{
# Get when a file first appeared
projr_manifest_file_first("data.csv", label = "output")

# Search all directories
projr_manifest_file_first("README.md")
} # }
```
