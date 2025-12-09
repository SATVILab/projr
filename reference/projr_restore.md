# Restore project artefact directories

Use `projr_content_update()` to restore all artefacts needed for the
current project. If the project isn't available locally yet,
`projr_restore_repo()` will clone it and then restore its artefacts.

## Usage

``` r
projr_restore_repo(
  repo,
  path = NULL,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL
)

projr_restore_repo_wd(
  repo,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL
)

projr_content_update(
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  clear = FALSE
)
```

## Arguments

- repo:

  character. GitHub repository (`"owner/repo"` or `"repo"`). (Only for
  repository restoration functions.) Must be a single non-empty
  character string.

- path:

  character or NULL. Local path for cloning the repository. Default is
  `NULL`, creating a subdirectory named after the repo. `"."` restores
  directly into the current directory. Must be NULL or a single
  non-empty character string.

- label:

  character vector or NULL. Specifies labels of artefacts to restore.
  Default is `NULL`, restoring all `raw` artefacts (e.g. `raw-data`).
  Must be NULL or a non-empty character vector with valid directory
  labels.

- pos:

  character vector or NULL. Specifies preferred source: `"source"`
  (directories) or `"dest"` (build destinations). Default is `NULL`,
  checking both in order. Must be NULL or one/both of `"source"` and
  `"dest"`.

- type:

  character or NULL. Remote type: `"local"`, `"osf"` or `"github"`.
  Default is `NULL`, automatically choosing the first available remote.
  Must be NULL or one of the valid remote types.

- title:

  character or NULL. Remote title as specified in `_projr.yml`. Default
  is `NULL`, using the first available title for the selected type. Must
  be NULL or a single non-empty character string.

- clear:

  logical. If `TRUE`, clears existing local artefact directories before
  restoration. Default is `FALSE`.

## Value

Invisibly returns `TRUE` if all restorations are successful, `FALSE`
otherwise. For `projr_content_update()`, returns `FALSE` if no labels
are found to restore or if any restoration fails. For
`projr_restore_repo()`, returns `FALSE` if cloning or restoration fails.

## Details

These functions restore artefact directories from remote sources:

- `projr_content_update()` restores artefacts in an existing local
  project without any cloning required. Requires a `manifest.csv` file
  in the project root.

- `projr_restore_repo()` clones a GitHub repository into a subdirectory
  (or specified path), then restores artefacts from that repository's
  remote sources.

- `projr_restore_repo_wd()` clones directly into the current working
  directory, then restores artefacts.

**Input Validation:** All parameters are validated before execution:

- `label`: Must be NULL or a non-empty character vector of valid
  directory labels

- `pos`: Must be NULL or contain only "source" and/or "dest"

- `type`: Must be NULL or one of "local", "osf", or "github"

- `title`: Must be NULL or a single character string

- `repo`: Must be a single non-empty character string

- `path`: Must be NULL or a single non-empty character string

**Error Handling:** The functions handle errors gracefully:

- Missing `manifest.csv` triggers an informative error

- Invalid labels or missing remote sources result in warning messages
  and skipped restoration

- Git clone failures are caught and reported

- Errors during restoration are caught per label, allowing partial
  success

## Examples

``` r
if (FALSE) { # \dontrun{
  # Restore all raw artefacts in existing local project
  projr_content_update()

  # Restore specific labels
  projr_content_update(label = c("raw-data", "cache"))

  # Restore from specific source type
  projr_content_update(type = "local", title = "archive")

  # Clone repository into subdirectory and restore artefacts
  projr_restore_repo("owner/repo")

  # Clone to specific path
  projr_restore_repo("owner/repo", path = "my-project")

  # Clone repository into current directory and restore artefacts
  projr_restore_repo_wd("owner/repo")
} # }
```
