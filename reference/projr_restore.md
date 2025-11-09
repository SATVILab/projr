# Restore project artefact directories

Use `projr_restore()` to restore all artefacts needed for the current
project. If the project isn't available locally yet,
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

projr_restore_repo_wd(repo, label = TRUE)

projr_restore(label = NULL, pos = NULL, type = NULL, title = NULL)
```

## Arguments

- repo:

  character. GitHub repository (`"owner/repo"` or `"repo"`). (Only for
  repository restoration functions.)

- path:

  character. Local path for cloning the repository. Default is `NULL`,
  creating a subdirectory named after the repo. `"."` restores directly
  into the current directory.

- label:

  character vector. Specifies labels of artefacts to restore. Default is
  `NULL`, restoring all `raw` artefacts (e.g. `raw-data`).

- pos:

  character vector. Specifies preferred source: `"source"` (directories)
  or `"dest"` (build). Default is `NULL`, checking both.

- type:

  character. Remote type: `"local"`, `"osf"` or `"github"`. Default is
  `NULL`, automatically choosing the first available remote.

- title:

  character. Remote title as specified in `_projr.yml`. Default is
  `NULL`.

## Value

Invisibly returns `TRUE` if restoration is successful.

## Details

- `projr_restore()` restores artefacts in an existing local project
  without any cloning required.

- `projr_restore_repo()` clones a GitHub repository into a subdirectory
  (or specified path), then restores artefacts.

- `projr_restore_repo_wd()` clones directly into the current working
  directory, then restores artefacts.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Restore all artefacts in existing local project
  projr_restore()

  # Clone repository into subdirectory and restore artefacts
  projr_restore_repo("owner/repo")

  # Clone repository into current directory and restore artefacts
  projr_restore_repo_wd("owner/repo")
} # }
```
