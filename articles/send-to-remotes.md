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

- GitHub Releases – version-controlled releases on your GitHub
  repository
- Local directories – local, network-mounted, or cloud-synced folders
- OSF (Open Science Framework) – not covered in this vignette

Content types you can archive: `raw-data`, `cache`, `output`, `docs`,
`code`.

## GitHub Destinations

Prerequisites:

- Git repository connected to GitHub
- GitHub PAT with `repo` scope (run
  [`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  for setup)
- Token stored in `_environment.local` (git-ignored):
  `GITHUB_PAT=ghp_...`

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
    raw-data-release:
      title: "raw-data-@version"
      content: [raw-data]
    output-release:
      title: "output-@version"
      content: [output]
```

Parameters:

- `title` – release name. Use `@version` for automatic versioning
  (e.g. `"raw-data-v0.1.0"`). Spaces become hyphens. Must be unique per
  repository.
- `content` – directory label to archive (`raw-data`, `cache`, `output`,
  `docs`, `code`).

Note: GitHub has a 2 GB limit per release asset. For larger datasets,
use local remotes or split across multiple releases.

## Local Destinations

No authentication required. Parent directories must exist before the
first build. Use absolute paths for clarity; relative paths resolve from
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

## Customization Options

Both GitHub and local remotes accept the same optional parameters.
Defaults are shown first.

### structure

How versions are organized on the remote.

| Value                 | Behaviour                                                |
|:----------------------|:---------------------------------------------------------|
| `"archive"` (default) | Creates a separate version per build (v0.1.0, v0.2.0, …) |
| `"latest"`            | Overwrites the same location each build                  |

Use `"archive"` for reproducibility; `"latest"` for always-current
snapshots.

### send_cue

When to create a new version on the remote.

| Value                   | Behaviour                                           |
|:------------------------|:----------------------------------------------------|
| `"if-change"` (default) | Upload only when content changed since last version |
| `"always"`              | Upload every build, even if unchanged               |
| `"never"`               | Disable uploads without removing configuration      |

### send_strategy

How files are transferred.

| Value                   | Behaviour                                      |
|:------------------------|:-----------------------------------------------|
| `"sync-diff"` (default) | Upload changed/new files, remove deleted files |
| `"sync-purge"`          | Delete everything on remote, then upload all   |
| `"upload-all"`          | Upload all files (may overwrite)               |
| `"upload-missing"`      | Upload only files not present on remote        |

Use `"sync-diff"` for efficiency. Use `"sync-purge"` to guarantee no
stale files. Avoid `"upload-missing"` if you delete files locally.

### send_inspect

How projr determines what already exists on the remote.

| Value                  | Behaviour                                                       |
|:-----------------------|:----------------------------------------------------------------|
| `"manifest"` (default) | Read `manifest.csv` on the remote (fast)                        |
| `"file"`               | Download and hash actual remote files (slower, always accurate) |
| `"none"`               | Treat remote as empty (forces full upload)                      |

## Complete Configuration Examples

### Comprehensive GitHub setup

``` r
# Raw data: archive, only when changed
projr_yml_dest_add_github(
  title = "raw-data-@version",
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

Remotes activate automatically during production builds:

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

If remotes aren’t uploading, verify you are running a production build
(not
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)),
`send_cue` is not `"never"`, and authentication is configured for GitHub
destinations.

## Best Practices

- Use `@version` in GitHub release titles; use descriptive prefixes like
  `"raw-data-@version"`.
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
