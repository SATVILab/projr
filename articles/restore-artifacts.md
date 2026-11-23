# Restoring Artifacts

Use
[`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md)
or
[`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md)
to copy files for a completed build into a folder of your choice. To
rerun an analysis, use
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
in its checkout, then run
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md).

## Get files from a completed build

These examples work outside a projr project. The first downloads reports
from a GitHub Release. The second copies results from an analysis on
disk and works without Git or a network connection.

``` r

library(projr)

reports <- projr_get_github(repo = "owner/study@analysis",
                            label = "docs", version = "1.2.3", out = "received")
list.files(reports[["docs"]])

results <- projr_get_local(path = "../study", label = "output", out = "received")
list.files(results[["output"]])
```

In the GitHub example, `analysis` is the branch containing the study
code and `1.2.3` is the build you want. Omit the branch to use the
repository’s default branch. When you know the release address, use its
repository and tag directly. For example, this reads from the release
named `reports` in a separate repository.

``` r

projr_get_github(repo = "owner/study-results", tag = "reports",
                 label = "docs", version = "1.2.3", out = "received")
```

Omit `version` to select the most recent completed build recorded at the
source. Supply one label or a vector of labels, such as
`c("raw-data", "docs")`. With `label = NULL`, projr looks up the labels
recorded for that build in the source manifest. A historical copy
requires files matching the recorded paths and hashes.

Each label gets its own subfolder, such as `received/docs/`. A relative
`out` starts in your working directory. Its default is `"."`. Downloads
to the same folder use a small `.projr-get.lock` file to take turns. The
returned character vector gives the absolute path of each subfolder,
named by label. projr stages and verifies the files before installing
them. It keeps matching files and reports conflicting local changes or
extra files. Set `overwrite = TRUE` when you want projr to replace the
contents of the requested subfolders.

Inside a project, you can read a named archive from its `remotes`
configuration. For example, the following reads the local archive called
`shared` in the [configuration
reference](https://satvilab.github.io/projr/articles/configuration.html#saving-artifacts).

``` r

projr_get_local(remote = "shared", label = "output-figures", out = "received")
```

Use either `remote` or the direct address (`path` for local projects,
`repo` for GitHub). A registered GitHub archive can likewise be selected
with `projr_get_github(remote = "reports", ...)`. If the type is stored
in a variable, `projr_get(type, ...)` accepts `"local"` and `"github"`
and forwards the remaining arguments to the corresponding function.

## Rebuild a checked-out version

After checking out a production build or its follow-up development
commit, run
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
in the clone. For a project published with
`projr_build_patch(archive_github = TRUE)`, the release named `archive`
already contains the required inputs. Each raw directory is restored to
its configured path. You can then render the checked-out scripts with
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md).

``` r

projr_restore()
projr_build_dev()
```

projr first tries files archived for the checked-out version, then
earlier uploads with the same directory contents. It recovers missing
manifest data from the release and verifies the complete file list and
MD5 hashes before installing any downloads. If the checkout lacks both
`DESCRIPTION` and `VERSION`, projr can recover the version from its
`.projr/BUILDLOG.md`. You can also supply `version = "1.2.3"`.

A partially populated input directory is filled when its existing files
match. To replace local edits or remove extra files, supply
`clear = TRUE`. For a templated release, pass its recorded concrete tag
with `destination`.

### When builds share the same inputs

For example, the Tutu analysis used the same four raw input files in
v0.2.0 and v0.6.1. Its checkout records v0.2.0 as the first build with
that file list. Its GitHub archive first recorded the list at v0.6.1.
You can see both records in the [checkout’s version
history](https://github.com/MiguelRodo/comp-upskilling/blob/8d9e476fa7d73794a882f39a7f3b22d2e74172d0/.projr/manifests/raw-data/versions.csv)
and the [release’s
`manifest.zip`](https://github.com/MiguelRodo/comp-upskilling/releases/tag/archive).

In a checkout containing those builds, this comparison returns zero
changed files.

``` r

projr_manifest_changes("v0.2.0", "v0.6.1", label = "raw-data")
```

The comparison reads the checkout’s history. When you restore the
earlier build, projr 0.7.0 and later can use a later upload if the exact
and earlier copies are unavailable. The release must record the same
snapshot, and every downloaded filename and hash must match.

## Check whether a build reproduces

Run
[`projr_reproduce()`](https://satvilab.github.io/projr/reference/projr_reproduce.md)
to check whether an analysis still produces the exact files it recorded.
It clones the project, restores its raw inputs and runs the production
scripts from empty output folders. The configured inputs, outputs and
cache must be inside the clone. Archives can live elsewhere. Choose a
project whose scripts and hooks you trust.

``` r

result <- projr_reproduce("owner/study", branch = "analysis", path = "study-check")
result$exact
result$directories
subset(result$files, status != "identical")
```

| Arguments | Code and expected files |
|----|----|
| Neither `version` nor `commit` | The latest completed build on the selected branch. |
| `version = "1.2.3"` | The code, inputs and expected outputs for that build. |
| `commit` alone | The last completed build at or before the commit, using that build’s code. |
| Both | The specified commit’s code with the named version’s inputs and expected outputs. |

A lockfile supplies the package versions for the run. With only a
`DESCRIPTION`, pak installs the dependencies and their versions are
unpinned. Inspect `result$dependencies$packages` to see which versions
were available. The overall result is true only when every expected
directory has a recorded baseline and the relative filenames and bytes
all match. Inspect `result$files` to see which files changed, went
missing or appeared. For example, a PDF can have matching text and still
differ in its metadata. The results stay in the checkout you selected
with `path`.

## projr_restore_repo()

Clones a GitHub repository and restores artifacts in one step.

``` r

projr_restore_repo(
  repo,          # "owner/repo" or "repo"
  path = NULL,   # clone destination (default: subdirectory named after repo)
  label = NULL,  # which artifacts (default: all raw-* labels)
  pos = NULL,    # "source", "dest", or both (default: both)
  type = NULL,   # "github" or "local" (default: first available)
  title = NULL,  # remote title from _projr.yml
  destination = NULL  # original concrete release tag or local path
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
  title = NULL,
  destination = NULL
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
project must have a build history in `.projr/manifests/` or a legacy
root `manifest.csv`.

``` r

projr_content_update(
  label = NULL,  # which artifacts (default: all raw-* labels)
  pos = NULL,    # "source", "dest", or both
  type = NULL,   # "github" or "local"
  title = NULL,  # remote title from _projr.yml
  clear = FALSE,
  destination = NULL
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

If the project has no manifest, the project needs at least one
successful `projr_build_*()` run first.

## Parameters

- `label` – character vector of directory labels to restore. `NULL`
  (default) restores all `raw-*` labels. Valid labels include
  `raw-data`, `cache`, `output`, and `docs`. Prefer regenerating
  `output`/`docs` with
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  rather than restoring them.

- `type` – remote type: `"github"` or `"local"`. `NULL` (default) uses
  the first available remote in `_projr.yml` order.

- `title` – selects a specific remote configuration when multiple exist
  for the same type. `NULL` (default) uses the first available.

- `pos` – `"source"` for source directories, `"dest"` for build
  destinations, or both (the default). Usually the default is fine.

- `repo` (restore_repo functions only) – GitHub repository in
  `"owner/repo"` or `"repo"` format.

- `path` (projr_restore_repo only) – where to clone. `NULL` creates a
  subdirectory; `"."` clones into the current directory.

## Authentication

projr 0.7.0 installs `httr` for GitHub downloads. If you restore an
older renv project pinned to projr 0.6.1 and see a missing-`httr` error,
run `renv::install("httr")` in that project, then retry the download.

For private GitHub artifacts, use a credential with access to the
project. Run
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for instructions. A local destination needs read access to the folder.

## Selecting an earlier destination

When a build expands `@branch` or `@uuid` in a destination, copy the
concrete tag or path from its `.projr/BUILDLOG.md`. Select the
configuration by its `title` and supply the earlier location as
`destination`.

``` r

projr_content_update(
  label = "output", pos = "dest", type = "github",
  title = "study-@branch", destination = "study-site-a"
)
```

Within that destination, restore selects its latest available version
for the label. By default, it retains extra local files. Use
`clear = TRUE` if you want the local label to match the restored
payload, including an explicitly recorded empty directory.

## Configuring remotes

To publish files for other readers, define a named archive under
`remotes` and select its labels under `build`, as shown in the
[configuration
reference](https://satvilab.github.io/projr/articles/configuration.html#saving-artifacts).
A registered archive remains readable when it is absent from the current
build settings.

For an existing configuration that keeps the address and publishing
settings together under `build`, use
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

- [`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md)
  selects the latest available version in the chosen destination. Use
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  for the verified inputs of a specific build.

- The default content-update search looks for a directory’s configured
  source before checking a build destination that contains that label.

- [`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md)
  catches errors per label.
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
  verifies all downloaded directories before installing them.

- For debug output, set `Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")`
  before restoring.

## See also

- [`?projr_restore`](https://satvilab.github.io/projr/reference/projr_restore.md)
  – full function documentation
- [`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md)
  – configuring remotes for archiving
- [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md)
  – environment variables including `GITHUB_PAT`
