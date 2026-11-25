# Restore project artefact directories

`projr_content_update()` is deprecated. Use
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md),
which it now calls with the latest version and `cue = "always"`.
`projr_restore_repo()` clones a GitHub repository and then fetches its
directories with
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).

## Usage

``` r
projr_restore_repo(
  repo,
  path = NULL,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  destination = NULL
)

projr_restore_repo_wd(
  repo,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  destination = NULL
)

projr_content_update(
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  clear = FALSE,
  destination = NULL
)
```

## Arguments

- repo:

  character. GitHub repository (`"owner/repo"` or `"repo"`). (Only for
  repository restoration functions.)

- path:

  character or NULL. Local path for cloning the repository. Default is
  `NULL`, creating a subdirectory named after the repo. `"."` restores
  directly into the current directory.

- label:

  character vector or NULL. Directory labels to restore. Default is
  `NULL`, restoring every declared raw directory.

- pos:

  Ignored. Kept for compatibility.

- type:

  character or NULL. Remote type, `"local"` or `"github"`, used with
  `title` to name the remote as `<type>:<title>`.

- title:

  character or NULL. Title of a remote registered under `remotes`.
  Default is `NULL`, using `build.source` or the single registered
  remote that published each label.

- destination:

  No longer supported. Must be `NULL`;
  [`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
  reads the address of the registered remote.

- clear:

  logical. If `TRUE`, local changes are replaced
  (`conflict = "remote"`). Otherwise, local changes stop the restore
  (`conflict = "error"`). Default is `FALSE`.

## Value

Invisibly returns `TRUE`. `projr_restore_repo()` returns `FALSE` if
cloning or restoration fails.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch raw directories in an existing project
projr_source()

# Clone repository into subdirectory and restore artefacts
projr_restore_repo("owner/repo")

# Clone to specific path
projr_restore_repo("owner/repo", path = "my-project")

# Clone repository into current directory and restore artefacts
projr_restore_repo_wd("owner/repo")
} # }
```
