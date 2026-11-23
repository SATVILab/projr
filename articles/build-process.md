# Build Process

## The projr Build Process

This article describes the three stages of a projr build and what
happens at each step.

### Overview

projr has two build types:

Production builds create versioned releases:

``` r

projr_build_patch(msg = "Fix analysis bug")   # 0.0.X
projr_build_minor(msg = "Add new section")    # 0.X.0
projr_build_major(msg = "Complete rewrite")   # X.0.0
```

Development builds iterate without incrementing the version:

``` r

projr_build_dev()
projr_build_dev(scripts = "analysis.qmd")
```

[`projr_build()`](https://satvilab.github.io/projr/reference/projr_build.md)
is a wrapper that accepts a `bump_component` argument:

``` r

projr_build(bump_component = "patch", msg = "Release")
```

Both follow a three-stage architecture:

    ┌─────────────┐
    │  Pre-Build  │ ─── Preparation, validation, versioning
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │    Build    │ ─── Document rendering and script execution
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │ Post-Build  │ ─── Finalization, distribution, commits
    └─────────────┘

------------------------------------------------------------------------

### Stage 1: Pre-Build

#### Validation

The build validates the environment before starting:

1.  [`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
    validates `_projr.yml`
2.  All configured scripts and hooks must exist on disk
3.  Required packages (quarto, rmarkdown, etc.) must be installed
4.  GitHub authentication checked when a GitHub destination is
    configured
5.  Git repository must be initialized
6.  If push is enabled, the GitHub remote must exist and the local
    branch must not be behind

Run
[`projr_build_check_packages()`](https://satvilab.github.io/projr/reference/projr_build_check_packages.md)
at any time to check package requirements.

#### Remote destination preparation

Creates GitHub releases or local archive directories as configured in
`_projr.yml`.

#### Documentation and dependency snapshot

- Records dependencies in `renv.lock` before rendering and again
  afterward. The default `build.renv: auto` detects an existing lockfile
  or `renv/activate.R`. Set `build.renv: true` to create the first
  lockfile from installed packages, or `false` to skip both snapshots. A
  missing dependency stops the build with its name.
- Updates `.gitignore` and `.Rbuildignore`
- Sets up docs directory paths

#### Version calculation

projr tracks three version numbers during a build:

- Pre-run version: the version before the build starts
- Run version: the version during the build
- Failure version: the version to revert to on failure

Version transitions:

    Production (patch) from dev:
      0.0.1-1 → 0.0.2 → 0.0.2-1 (success)
      0.0.1-1 → 0.0.2 → 0.0.1-1 (failure)

    Dev build from release (auto-bumps):
      0.0.1 → 0.0.1-1

    Dev build from dev (no change):
      0.0.1-1 → 0.0.1-1

You can check or set the version directly:

``` r

projr_version_get()
projr_version_set("0.1.0")
```

#### Hooks

Pre-build hooks run after validation, before rendering.

``` r

# Add hooks from R
projr_yml_hooks_add_pre("setup-data.R")
projr_yml_hooks_add_post("cleanup.R")

# Or add to any stage
projr_yml_hooks_add("logger.R", stage = "both")
```

See
[`vignette("scripts-and-hooks")`](https://satvilab.github.io/projr/articles/scripts-and-hooks.md)
for full configuration details.

#### Output directory preparation

Sets the project to the run version, then clears output directories
based on the `clear_output` setting:

- `"pre"` (default for production): clear now
- `"post"`: clear after build completes
- `"never"`: don’t clear

``` r

projr_build_patch(clear_output = "pre")
# or
Sys.setenv(PROJR_CLEAR_OUTPUT = "pre")
```

Safe directories (in cache, e.g. `_tmp/projr/v0.0.2/output`) are always
cleared. Unsafe directories (final locations, e.g. `_output`) follow the
setting above.

#### Pre-build git commit

Commits version files, ignore files, and documentation changes with the
message `"Snapshot pre-build"`. Only runs if `build.git.commit` is
`TRUE`.

#### Pre-build manifest

Hashes all files in input directories (`raw-data`, `cache`) and stores
the result in a temporary manifest in the cache directory. This is
merged with output hashes after the build.

| label    | fn            | version | hash    |
|----------|---------------|---------|---------|
| raw-data | dataset.csv   | v0.0.2  | abc123… |
| cache    | processed.rds | v0.0.2  | def456… |

------------------------------------------------------------------------

### Stage 2: Build

#### Script selection

Scripts are selected in this priority order:

1.  `scripts` argument to
    [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
2.  `dev.scripts` in `_projr.yml` (dev builds only)
3.  `build.scripts` in `_projr.yml`
4.  Automatic detection from the project configuration and top-level
    scripts

The `scripts` argument and YAML settings accept paths and glob patterns.
Prefix a pattern with `!` to exclude matching scripts. Pass
`scripts = ""` to skip running scripts.

#### Document rendering

The rendering engine is detected from the selected scripts and project
configuration:

- Quarto for `.qmd` files or Quarto projects
- Bookdown for `.Rmd` files selected in a project with `_bookdown.yml`
- R Markdown for `.Rmd` files

Explicitly selected scripts run in order. A project renders its selected
documents together at the position of its last selected document. For an
automatic build, a project renders first, followed by top-level R
scripts.

Pass custom arguments to the document renderers:

``` r

projr_build_patch(
  msg = "Update analysis",
  args_engine = list(quiet = FALSE, clean = TRUE)
)
```

#### Script execution

R build scripts and hooks run via
[`source()`](https://rdrr.io/r/base/source.html). Use
[`projr_path_get_dir()`](https://satvilab.github.io/projr/reference/projr_path_get_dir.md)
inside a script to write to the correct output directories.

``` r

out_dir <- projr_path_get_dir("output", safe = TRUE)
write.csv(results, file.path(out_dir, "results.csv"))
```

------------------------------------------------------------------------

### Stage 3: Post-Build

#### Artifact finalization

Copies files from safe (cache) directories to final (unsafe)
directories:

- `_tmp/projr/v0.0.2/output` → `_output`
- Safe `docs` directory → `docs`

If `clear_output = "post"`, the final directories are cleared before
copying.

An output directory can include other declared directories by label. For
example, to publish a bundle of raw measurements, rendered reports and
tables, configure the receiving output:

``` yaml
directories:
  cache:
    path: _tmp
  raw-measurements:
    path: inputs/measurements
  docs:
    path: reports
  output-tables:
    path: tables
  output-bundle:
    path: publication
    include: [raw-measurements, docs, output-tables]
```

This creates `publication/raw-measurements/`, `publication/docs/` and
`publication/output-tables/`. Development builds collect the same layout
in the build cache. The receiving output’s manifest and archive include
the copies. An included output may have its own includes. projr finishes
those first and reports circular includes during configuration checks.
Reserve each included label’s subdirectory for projr. When you retain
previous output, projr refreshes those subdirectories and preserves
their siblings.

#### Post-build manifest

1.  Hashes files in `output` and `docs` directories
2.  Merges with the pre-build manifest
3.  Records the build version in each hashed directory’s manifest index

See the Manifest System section below for query functions.

#### Documentation updates

For production builds, projr updates:

- roxygen2 `.Rd` files (if roxygen comments exist)
- `CITATION.cff`, `codemeta.json`, `inst/CITATION` (version numbers)
- `README.Rmd` (rendered if it exists)
- `BUILDLOG.md` (with a change summary)
- `CHANGELOG.md` (if configured)

Example BUILDLOG entry:

``` markdown
## v0.0.2 (2024-01-15)

Build completed in 45.2 seconds

### Changes from v0.0.1 → v0.0.2

#### Input Changes
- Modified: 2 files
- Added: 1 file

#### Output Changes
- Modified: 5 files
- Added: 3 files
```

When total changes are fewer than 10, individual filenames are listed.
Otherwise only counts are shown.

#### Post-build git commit

Commits outputs, documentation, manifest, and metadata. The commit
message follows the format `"Build v{version}: {message}"`. Does not
push yet.

#### Remote distribution

Sends artifacts to configured destinations (GitHub Releases or local
directories). The behavior is controlled by four parameters in
`_projr.yml`:

| Parameter | Options | Description |
|----|----|----|
| `structure` | `archive`, `latest` | Versioned subdirs vs overwrite |
| `send.cue` | `always`, `if-change` | When to send |
| `send.strategy` | `sync-diff`, `sync-purge`, `upload-all`, `upload-missing` | How to update |
| `send.inspect` | `manifest`, `file`, `none` | How to detect changes |

For quick archiving without editing `_projr.yml`, use the
`archive_github` or `archive_local` parameters:

``` r

# Archive all directories to a GitHub release
projr_build_patch(msg = "Release v0.0.2", archive_github = TRUE)

# Archive specific directories
projr_build_patch(msg = "Archive outputs", archive_github = c("output", "docs"))

# Archive locally
projr_build_patch(msg = "Local backup", archive_local = TRUE)
```

See
[`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md)
for full configuration details.

#### Post-build hooks

Post-build hooks run after distribution, before the final push. See
[`vignette("scripts-and-hooks")`](https://satvilab.github.io/projr/articles/scripts-and-hooks.md).

#### Dev version bump

Production builds only. Appends a dev suffix (e.g. `0.0.2` → `0.0.2-1`)
and commits with the message `"Begin v{version}"`.

#### Git push

Pushes all commits (pre-build, post-build, dev version) if
`build.git.push` is `TRUE`.

Configure git behavior in `_projr.yml`:

``` yaml
build:
  git:
    commit: true
    push: true
    add-untracked: true
```

Or from R:

``` r

projr_yml_git_set(commit = TRUE, push = TRUE, add_untracked = TRUE)
```

------------------------------------------------------------------------

### Manifest System

Production builds record input directories before rendering and output
directories afterward. Each hashed label has its own history under
`_projr/manifests/`. For example, `raw-data/versions.csv` might contain
the following rows (snapshot IDs are abbreviated here).

``` csv
label,version,directory_version,snapshot
raw-data,v1.0.0,v1.0.0,abcd…
raw-data,v1.1.0,v1.0.0,abcd…
raw-data,v1.2.0,v1.2.0,ef01…
raw-data,v1.3.0,v1.0.0,abcd…
```

Here the data changed in v1.2.0 and returned to its original contents in
v1.3.0. The four builds share two file lists. Open
`raw-data/files-<snapshot>.csv` to see each relative filename (`fn`) and
its MD5 (`hash`). The full SHA-256 in the index identifies those exact
CSV bytes. A change in `output/` updates that label’s history
independently.

An existing project can continue reading its root `manifest.csv`. Its
next production build imports the old history and writes subsequent
updates under `_projr/manifests/`.

Query the manifest:

``` r

# Changes between two versions
changes <- projr_manifest_changes("0.0.1", "0.0.2")
changes$added
changes$removed
changes$modified

# Changes across multiple versions
projr_manifest_range("0.0.1", "0.0.5")

# Last build's changes
projr_manifest_last_change()
```

------------------------------------------------------------------------

### Logging

#### Output levels

Control console verbosity with `PROJR_OUTPUT_LEVEL`:

| Level     | Description                                    |
|-----------|------------------------------------------------|
| `"none"`  | Errors only                                    |
| `"std"`   | Major step progress (default)                  |
| `"debug"` | Detailed operations, file counts, remote plans |

``` r

Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
projr_build_patch(msg = "Debug run")
```

At the `"debug"` level you will see messages like:

    → Checking required packages
    → Snapshotting renv
    → Setting build version
    → Running build hook: setup.R
    → Remote: github/archive (12 files)
    → Strategy: sync-diff — 3 modified, 2 added, 1 removed

#### Log files

Detailed logs are written to the cache directory:

    cache/projr/log/
    ├── output/                    # Production build logs
    │   ├── history/builds.md      # All build records
    │   └── output/YYYY-MMM-DD/
    │       └── HH-MM-SS-UUID.qmd  # Detailed log
    └── dev/                       # Development build logs
        └── ...

History tracking (`builds.md`) is always maintained. Detailed per-build
logs can be disabled:

``` r

Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
```

View or clear logs:

``` r

projr_log_view()
projr_log_clear()
```

#### Debugging a failed build

1.  Enable debug output and re-run:

``` r

Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
projr_build_patch(msg = "Debug run")
```

2.  Review the detailed log file in the cache directory.

3.  Check git status:

``` r

system("git status")
system("git diff")
```

4.  Verify package requirements:

``` r

projr_build_check_packages()
```

5.  If authentication is the issue:

``` r

projr_instr_auth_github()
```

------------------------------------------------------------------------

### Build Function Quick Reference

#### projr_build_patch / projr_build_minor / projr_build_major

``` r

projr_build_patch(
  msg = "Build message",
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = "pre"
)
```

#### projr_build_dev

``` r

projr_build_dev(
  scripts = NULL,
  bump = FALSE,
  old_dev_remove = TRUE,
  args_engine = list(),
  profile = NULL,
  clear_output = "never"
)
```

#### Configuration helpers

``` r

# Validate configuration
projr_yml_check()

# Read configuration
projr_yml_get()

# Get project paths
projr_path_get("output", "results.csv")
projr_path_get_dir("output", safe = TRUE)
```

------------------------------------------------------------------------

### Complete Lifecycle Example

A typical workflow from setup through production build:

``` r

# 1. Check configuration is valid
projr_yml_check()

# 2. Check the current version
projr_version_get()

# 3. Run a dev build while iterating
projr_build_dev()

# 4. When ready, do a production build with archiving
projr_build_patch(
  msg = "Add regression analysis",
  archive_github = TRUE
)

# 5. Check what changed
projr_manifest_last_change()

# 6. Review the build log
projr_log_view()
```

After
[`projr_build_patch()`](https://satvilab.github.io/projr/reference/projr_build.md)
completes, the project version is bumped to a dev version
(e.g. `0.0.2-1`), all artifacts are committed and pushed, and remote
destinations have received the new files.

------------------------------------------------------------------------

### Further reading

- [`vignette("scripts-and-hooks")`](https://satvilab.github.io/projr/articles/scripts-and-hooks.md)
  – build scripts and hooks
- [`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md)
  – remote distribution
- [`vignette("dest-send-workflow")`](https://satvilab.github.io/projr/articles/dest-send-workflow.md)
  – destination send workflow diagrams
- [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md)
  – environment variables reference
- [`vignette("how-to-guides")`](https://satvilab.github.io/projr/articles/how-to-guides.md)
  – task-focused guides
- [`vignette("concepts")`](https://satvilab.github.io/projr/articles/concepts.md)
  – core concepts
