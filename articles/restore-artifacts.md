# Restoring Artifacts

projr can restore versioned project artifacts (raw data, outputs,
documents) from remote sources. This is useful when setting up on a new
machine, collaborating, or recovering archived data.

Three functions handle restoration:

- [`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  – clone a repository and restore artifacts
- [`projr_restore_repo_wd()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  – clone into the current directory and restore
- [`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  – restore artifacts in an existing local project

## Quick start

``` r
library(projr)

# Clone and restore (most common)
projr_restore_repo("owner/repo")

# Restore artifacts in an existing project
projr_content_update()

# Restore only raw data
projr_content_update(label = "raw-data")
```

## projr_restore_repo()

Clones a GitHub repository and restores artifacts in one step.

``` r
projr_restore_repo(
  repo,          # "owner/repo" or "repo"
  path = NULL,   # clone destination (default: subdirectory named after repo)
  label = NULL,  # which artifacts (default: all raw-* labels)
  pos = NULL,    # "source", "dest", or both (default: both)
  type = NULL,   # "github", "local", or "osf" (default: first available)
  title = NULL   # remote title from _projr.yml (default: first available)
)
```

``` r
# Clone into a subdirectory
projr_restore_repo("owner/repo")

# Clone to a specific path
projr_restore_repo("owner/repo", path = "~/projects/my-analysis")
```

The function reads `_projr.yml` in the cloned repo to locate configured
remotes, then downloads all `raw-*` artifacts by default. Cache is not
included unless requested explicitly. Returns `TRUE` on success.

## projr_restore_repo_wd()

A convenience wrapper that clones directly into the current working
directory.

``` r
projr_restore_repo_wd(
  repo,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL
)
```

``` r
setwd("~/projects/my-analysis")
projr_restore_repo_wd("owner/repo")
```

This writes files into your current directory, so make sure you are in
the right place before calling it.

## projr_content_update()

Restores artifacts in an existing local project (no cloning). The
project must have a `manifest.csv` in its root.

``` r
projr_content_update(
  label = NULL,  # which artifacts (default: all raw-* labels)
  pos = NULL,    # "source", "dest", or both
  type = NULL,   # "github", "local", or "osf"
  title = NULL   # remote title from _projr.yml
)
```

``` r
# Restore everything raw
projr_content_update()

# Restore raw data and cache
projr_content_update(label = c("raw-data", "cache"))

# Restore from a specific remote
projr_content_update(type = "local", title = "network-backup")
```

If `manifest.csv` is missing, the project needs at least one successful
`projr_build_*()` run first.

## Parameters

- `label` – character vector of directory labels to restore. `NULL`
  (default) restores all `raw-*` labels. Valid labels include
  `raw-data`, `cache`, `output`, and `docs`. Prefer regenerating
  `output`/`docs` with
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  rather than restoring them.

- `type` – remote type: `"github"`, `"local"`, or `"osf"`. `NULL`
  (default) uses the first available remote in `_projr.yml` order.

- `title` – selects a specific remote configuration when multiple exist
  for the same type. `NULL` (default) uses the first available.

- `pos` – `"source"` for source directories, `"dest"` for build
  destinations, or both (the default). Usually the default is fine.

- `repo` (restore_repo functions only) – GitHub repository in
  `"owner/repo"` or `"repo"` format.

- `path` (projr_restore_repo only) – where to clone. `NULL` creates a
  subdirectory; `"."` clones into the current directory.

## Authentication

GitHub restoration requires a Personal Access Token (PAT). Run
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for setup instructions.

``` r
projr_instr_auth_github()
```

Set the token in `_environment.local` (which should be git-ignored):

``` bash
GITHUB_PAT=ghp_your_token_here
```

Verify with:

``` r
Sys.getenv("GITHUB_PAT")
```

OSF restoration requires `OSF_PAT` set in the same way. Local directory
remotes need no authentication.

## Configuring remotes

Before artifacts can be restored, they must be archived during builds.
Use
[`projr_yml_dest_add_github()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_github.md)
or
[`projr_yml_dest_add_local()`](https://satvilab.github.io/projr/reference/projr_yml_dest_add.md)
to configure destinations in `_projr.yml`:

``` r
# Archive raw data to GitHub releases
projr_yml_dest_add_github(
  title = "raw-data-@version",
  content = "raw-data",
  send_cue = "if-change"
)

# Archive to a local directory
projr_yml_dest_add_local(
  title = "backup",
  content = "raw-data",
  path = "/mnt/shared/project-data"
)
```

Add restoration instructions to your project README so collaborators
know what to run:

``` markdown
## Setup

1. Restore project: `projr::projr_restore_repo("owner/repo")`
2. Install dependencies: `renv::restore()`
3. Build project: `projr::projr_build_dev()`
```

## Examples

### New collaborator setup

``` r
# Clone and restore artifacts
projr_restore_repo("satvilab/my-study")

# Enter project, install dependencies, build
setwd("my-study")
renv::restore()
projr_build_dev()
```

### Selective restoration in an existing project

``` r
# Restore only raw data (skip cache, output, etc.)
projr_content_update(label = "raw-data")

# Regenerate everything else
projr_build_dev()
```

### Falling back between remotes

``` r
# Try GitHub first, fall back to local
result <- tryCatch(
  projr_content_update(type = "github"),
  error = function(e) FALSE
)

if (!result) {
  projr_content_update(type = "local")
}
```

## Notes

- For archive remotes, restoration always fetches the latest available
  version regardless of your current Git checkout. To get an older
  version, download it manually from the remote source (e.g., GitHub
  Releases).

- Remote sources are checked in the order they appear in `_projr.yml`.

- Errors during restoration are caught per label, so some labels may
  succeed even if others fail.

- For debug output, set `Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")`
  before restoring.

## See also

- [`?projr_restore`](https://satvilab.github.io/projr/reference/projr_restore.md)
  – full function documentation
- [`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md)
  – configuring remotes for archiving
- [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md)
  – environment variables including `GITHUB_PAT`
