# Sending to Remotes

projr can automatically send artifacts to remote destinations during
production builds
([`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md),
[`projr_build_minor()`](https://satvilab.github.io/projr/reference/projr_build.md),
[`projr_build_major()`](https://satvilab.github.io/projr/reference/projr_build.md)).
Development builds
([`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md))
never upload to remotes.

Supported remote types:

- GitHub Releases in the analysis repository or a specified destination
  repository
- Local directories – local, network-mounted, or cloud-synced folders

Archive directories by their configured labels, such as `raw-data`,
`output`, or `docs`. Select `cache` explicitly to include it. GitHub
supplies its own source-code download for the commit at the release tag.

With the development version of projr, put the address of a named
archive under `remotes` and select it under `build`. This example makes
reports available in a GitHub release named `reports` in the analysis
repository.

``` yaml
remotes:
  github:
    reports: {}
build:
  github:
    reports:
      content: [docs]
```

To pause uploads, remove `reports` from `build.github`. Readers can
still get previously published reports with
`projr_get_github(remote = "reports", label = "docs")` from a project
where that name is registered. See the [configuration
reference](https://satvilab.github.io/projr/articles/configuration.html#saving-artifacts)
for local archives, separate GitHub repositories, and the all-label
selection. The inline build configurations below are also supported.

## GitHub Destinations

Configure a GitHub remote for the checkout and a credential with write
access to releases in the destination repository. Run
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for setup.

``` r

# Add a GitHub release for raw data
projr_yml_dest_add_github(
  title = "raw-data-@version",
  content = "raw-data"
)

# Add a GitHub release for outputs
projr_yml_dest_add_github(
  title = "output-@version",
  content = "output"
)
```

YAML equivalent:

``` yaml
build:
  github:
    "raw-data-@version":
      content: [raw-data]
    "output-@version":
      content: [output]
```

Parameters:

- `title` sets both the release name and the Git tag. For version 1.2.3,
  `"archive-@version"` produces `archive-v1.2.3`. The literal
  `"archive"` shares one release across versions.
- `branch` optionally sets the branch in the GitHub repository whose
  commit a newly created release should target. Use `branch: "@branch"`
  for the current checkout’s branch. A missing branch or a conflicting
  existing Git tag stops release creation.
- `content` – directory label to archive (for example, `raw-data` or
  `output`).

## Local Destinations

Choose a writable local or mounted folder. Relative paths resolve from
the project root.

``` r

# Archive to local directory
projr_yml_dest_add_local(
  title = "local-backup",
  content = "output",
  path = "~/project-archive/output"
)

# Archive to network drive
projr_yml_dest_add_local(
  title = "network-backup",
  content = "raw-data",
  path = "/mnt/shared/projects/my-project/data"
)
```

YAML equivalent:

``` yaml
build:
  local:
    local-backup:
      title: "local-backup"
      content: [output]
      path: "~/project-archive/output"
```

Parameters:

- `title` – descriptive name (identification only).
- `content` – directory label to archive.
- `path` – destination directory (absolute or project-relative).

In YAML, `path-append-label: false` omits the label directory. An
archive then stores files under `<path>/v1.2.3/`, while a latest
destination stores them directly under `<path>/`. Both keep their
directory histories in `<path>/manifests/`. A latest destination with
this layout reserves the top-level names `manifests`, `manifest.csv`,
`VERSION`, and `CHANGELOG.md` for release metadata. Files in
subdirectories can use those names. For this latest layout, the
destination’s `VERSION` and matching manifest record an empty build.
When all archived files go missing after a populated build,
`projr_content_update(clear = TRUE)` leaves the local files in place.

Local destination names and paths and GitHub release names accept
`@branch`, `@version`, and `@uuid`. For example,
`archives/@branch/@version-@uuid` keeps a separate local archive for
each build. Find the concrete title and destination in the **Resolved
archive destinations** section of `.projr/BUILDLOG.md`. Use
`projr_restore(version = "1.2.3")` in a checkout to restore the inputs
from a build published with `archive_github = TRUE`. To copy published
directories elsewhere, use
[`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md)
or
[`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md)
with `version` and `out`. The [restore
guide](https://satvilab.github.io/projr/articles/restore-artifacts.md)
covers both workflows.

## Customization Options

Both GitHub and local remotes accept the same optional parameters.
Defaults are shown first.

### structure

How versions are organized on the remote.

| Value | Behaviour |
|:---|:---|
| `"archive"` (default) | Keeps uploaded files under their build version. Unchanged files can be retrieved from a matching earlier upload. |
| `"latest"` | Overwrites the same location each build |

Use `"archive"` for reproducibility; `"latest"` for always-current
snapshots.

### send_cue

When to create a new version on the remote.

| Value | Behaviour |
|:---|:---|
| `"if-change"` (default) | Upload only when content changed since last version |
| `"always"` | Upload every build, even if unchanged |

### send_strategy

How files are transferred.

| Value | Behaviour |
|:---|:---|
| `"sync-diff"` (default) | Upload changed/new files, remove deleted files |
| `"sync-purge"` | Replace the files for the selected label at the destination |
| `"upload-all"` | Upload all files (may overwrite) |
| `"upload-missing"` | Upload only files not present on remote |

Use `"sync-diff"` for efficiency. Use `"sync-purge"` to guarantee no
stale files. Avoid `"upload-missing"` if you delete files locally.

### send_inspect

How projr determines what already exists on the remote.

| Value | Behaviour |
|:---|:---|
| `"manifest"` (default) | Read the remote manifest (fast) |
| `"file"` | Download and hash actual remote files (slower, always accurate) |
| `"none"` | Treat remote as empty (forces full upload) |

Local archives keep their history in `manifests/`. A GitHub Release
holds one `manifest.zip`. While an upload updates that ZIP, it claims
`projr-manifest.lock` on the same release. If a second upload encounters
the lock, retry it after the first upload finishes. If the first process
was terminated, confirm that it has stopped before removing its lock
asset.

## Complete Configuration Examples

### Comprehensive GitHub setup

``` r

# Raw data: reuse the earlier upload on the same release when unchanged
projr_yml_dest_add_github(
  title = "raw-data",
  content = "raw-data",
  structure = "archive",
  send_cue = "if-change",
  send_strategy = "sync-diff",
  send_inspect = "manifest"
)

# Output: archive every version
projr_yml_dest_add_github(
  title = "output-@version",
  content = "output",
  structure = "archive",
  send_cue = "always",
  send_strategy = "sync-diff",
  send_inspect = "manifest"
)

# Docs: overwrite with latest
projr_yml_dest_add_github(
  title = "docs-latest",
  content = "docs",
  structure = "latest",
  send_cue = "always",
  send_strategy = "sync-purge",
  send_inspect = "none"
)
```

### Local archive setup

``` r

# Raw data to local archive
projr_yml_dest_add_local(
  title = "raw-data-archive",
  content = "raw-data",
  path = "~/research-archive/my-project/raw-data",
  structure = "archive",
  send_cue = "if-change",
  send_strategy = "sync-diff",
  send_inspect = "manifest"
)

# Output to network drive
projr_yml_dest_add_local(
  title = "output-network",
  content = "output",
  path = "/mnt/shared/project/output",
  structure = "latest",
  send_cue = "always",
  send_strategy = "sync-purge",
  send_inspect = "none"
)
```

### Multiple remotes

projr uploads to all configured remotes during each production build.

``` r

# GitHub for public sharing
projr_yml_dest_add_github(
  title = "output-@version",
  content = "output",
  structure = "archive"
)

# Local for quick backup
projr_yml_dest_add_local(
  title = "output-local",
  content = "output",
  path = "~/backup/output",
  structure = "archive"
)

# Network for team access
projr_yml_dest_add_local(
  title = "output-network",
  content = "output",
  path = "/mnt/shared/output",
  structure = "latest"
)
```

## Build Workflow

Production builds publish to the destinations selected under
`build.local` or `build.github` and to any one-off archive requested in
the call.

``` r

projr_build_dev()   # does NOT upload to remotes
projr_build_patch() # uploads (0.1.0 -> 0.1.1)
projr_build_minor() # uploads (0.1.0 -> 0.2.0)
projr_build_major() # uploads (0.1.0 -> 1.0.0)
```

Build steps with remotes configured:

1.  Clear output directories, run pre-build hooks, hash input files
2.  Bump version
3.  Render documents and scripts
4.  Hash output files, update manifest
5.  Git commit (if configured)
6.  Upload to remotes
7.  Run post-build hooks

Check and validate your configuration:

``` r

projr_yml_get()$build$github
projr_yml_get()$build$local
projr_yml_check()
```

If a destination has no new files, check its configured labels and
whether they changed during a production build. For GitHub, also check
release write access.

## Best Practices

- Use a literal tag such as `archive` to keep several builds in one
  GitHub release. Use `raw-data-@version` when each build needs its own
  release.
- Archive `raw-data` separately from `output`.
- Start with the defaults: `structure = "archive"`,
  `send_strategy = "sync-diff"`, `send_cue = "if-change"`,
  `send_inspect = "manifest"`.
- Use `send_cue = "if-change"` for large, rarely-changing datasets.
- For local remotes, prefer absolute paths (`~/archive/`,
  `/mnt/shared/`).
- Store tokens in `_environment.local` only – never commit secrets.
- Test configuration with
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
  first (no uploads).
- For slow uploads, switch to `"sync-diff"` + `"manifest"` inspection.

## Quick Reference

``` r

# Minimal GitHub remote
projr_yml_dest_add_github(
  title = "output-@version",
  content = "output"
)

# Minimal local remote
projr_yml_dest_add_local(
  title = "output-backup",
  content = "output",
  path = "~/archive/output"
)

# Run production build
projr_build_patch()
```

Recommended defaults:

| Parameter       | Default       | Purpose                   |
|:----------------|:--------------|:--------------------------|
| `structure`     | `"archive"`   | Version tracking          |
| `send_cue`      | `"if-change"` | Skip unchanged content    |
| `send_strategy` | `"sync-diff"` | Transfer only differences |
| `send_inspect`  | `"manifest"`  | Fast remote inspection    |

## See Also

- [`?projr_yml_dest_add_github`](https://satvilab.github.io/projr/reference/projr_yml_dest_add_github.md)
  – GitHub remote documentation
- [`?projr_yml_dest_add_local`](https://satvilab.github.io/projr/reference/projr_yml_dest_add.md)
  – Local remote documentation
- [`vignette("dest-send-workflow")`](https://satvilab.github.io/projr/articles/dest-send-workflow.md)
  – Internal workflow for sending to destinations (with diagrams)
- [`vignette("restore-artifacts")`](https://satvilab.github.io/projr/articles/restore-artifacts.md)
  – Restoring archived artifacts
- [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md)
  – Environment variables including `GITHUB_PAT`
