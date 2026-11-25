# How-to guides

Task-focused guides for common projr workflows.

------------------------------------------------------------------------

## 1. Initialise a project

``` r

library(projr)

# Interactive prompts (recommended for first-time users)
projr_init_prompt()

# Sensible defaults, no prompts
projr_init()

# Full setup: Git, GitHub, license, citation, renv, etc.
projr_init_all()
```

This creates `_projr.yml`, `DESCRIPTION`, `README.md`, and directories
(`_raw_data`, `_output`, `_tmp`, `docs`). Git, GitHub, license, and
citation setup are optional and prompted or skipped depending on the
function used.

``` r

# Set license and citation separately
projr_init_license(license = "MIT")
projr_init_cite()
```

If GitHub credential prompts fail, run
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for setup instructions. Initialisation won’t overwrite existing config
files.

------------------------------------------------------------------------

You can keep `_projr.yml`, `_projr-<profile>.yml` and `_projr-local.yml`
in `.projr/config/` as well as the project root. You can split the files
between these locations. A root file takes precedence over a nested file
with the same name. Configuration updates write back to that file. New
files and all relative paths in the configuration start at the project
root.

## 2. Run a development build

Use
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
when iterating on code and documents before creating a versioned
release.

``` r

projr_build_dev()

# Build specific documents only
projr_build_dev(scripts = "analysis.Rmd")
projr_build_dev(scripts = c("methods.Rmd", "results.Rmd"))
```

On a new project, dev builds stage outputs in `_tmp/projr/dev/`. Once
the project has a version, they use `_tmp/projr/v<version>/`. Use
[`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
to get the output path in development and production builds.

``` r

# In your R Markdown or scripts:
fig_path <- projr_path_get("output", "figures", "plot.png")
png(filename = fig_path)
# ... plotting code ...
dev.off()

# Dev build:  _tmp/projr/v0.1.0/output/figures/plot.png
# Final build: _output/figures/plot.png
```

Hardcoded paths like `"_output/plot.png"` bypass the cache and write
directly to `_output` during dev builds.

------------------------------------------------------------------------

## 3. Run a final (versioned) build

``` r

# Patch bump (0.1.0 -> 0.1.1) — projr_build() is equivalent
projr_build_patch()

# Minor bump (0.1.0 -> 0.2.0)
projr_build_minor()

# Major bump (0.1.0 -> 1.0.0)
projr_build_major()
```

A final build clears output directories, renders documents, bumps the
version, writes the manifest, uploads to configured destinations, and
commits to Git (if configured).

Control output clearing with the `PROJR_CLEAR_OUTPUT` env var (`"pre"`,
`"post"`, or `"never"`):

``` r

Sys.setenv(PROJR_CLEAR_OUTPUT = "pre")
projr_build()
```

Git behaviour is configured in `_projr.yml`:

``` yaml
build:
  git:
    commit: true
    push: true
```

------------------------------------------------------------------------

## 4. Archive artefacts

This configuration publishes raw data to a shared folder and output to a
GitHub release named `figures`. Keep the addresses and sending options
under `remotes`. Select the archives and the labels to send under
`build.dest`.

``` yaml
remotes:
  local:
    backup:
      path: ~/project-archives/study
  github:
    figures: {}
build:
  dest:
    backup:
      content: [raw-data]
    figures:
      content: [output]
```

To pause new uploads to `backup`, remove its entry under `build.dest`.
The address under `remotes.local` still lets you retrieve earlier files.

Use `structure: archive` to retain versioned artifacts or
`structure: latest` to keep the current payload. See
[`vignette("configuration")`](https://satvilab.github.io/projr/articles/configuration.md)
for all the options.

------------------------------------------------------------------------

## 5. Restore artefacts

To reproduce a checked-out production build from its GitHub archive, run
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)
inside that checkout. It fills in missing raw data after verifying the
recorded file hashes. Use `projr_restore(version = "1.2.3")` to select a
particular build.

``` r

# Clone a repo and restore archived content
projr_restore_repo("owner/repo")
projr_restore_repo("owner/repo", path = "~/projects/my-project")

# Restore the inputs for the build in an existing checkout
projr_restore()
```

To copy the raw data from build 1.2.3 of the named local archive above
into `downloads/raw-data/`, run the following in the project that
defines `backup`.

``` r

projr_get_local(remote = "backup", label = "raw-data", version = "1.2.3", out = "downloads")
```

To update configured directories in place from their latest available
files, use
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).
It replaces the deprecated
[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_content_update.md).

``` r

projr_source(label = "raw-data")
projr_source(label = c("raw-data", "output"))
projr_source(label = "raw-data", remote = "local:backup")
```

Without `remote`,
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md)
uses `build.source`, or else the single remote registered under
`remotes` that published each label.

To fetch inputs before every build, list their remotes under
`build.source`. The build stops on local edits before changing any
files. See
[`vignette("configuration")`](https://satvilab.github.io/projr/articles/configuration.md)
for the settings.

``` yaml
build:
  source: [backup]
```

------------------------------------------------------------------------

## 6. Define directories and labels

The first build writes a starter `_projr.yml` if the project has no base
configuration. On that first build, projr disables Git operations
outside a repository and disables pushing in a repository without an
upstream. Existing configurations keep their Git settings. You can also
create the starter with
[`projr_init()`](https://satvilab.github.io/projr/reference/projr_init.md).
It declares these paths:

- `raw-data` → `_raw_data`
- `cache` → `_tmp`
- `output` → `_output`
- `docs` → `docs`

Add custom labels in `_projr.yml`:

``` yaml
directories:
  raw-data:
    path: _raw_data
  raw-data-public:
    path: _raw_data_public
  raw-data-private:
    path: _raw_data_private
  cache:
    path: _tmp
  output:
    path: _output
  output-figures:
    path: _output/figures
  docs:
    path: docs
```

Declare the labels your project uses, with one path per label. A profile
can inherit that path from the default configuration. Relative paths
start at the project root even when a nested document runs from its own
directory. Staging outputs requires a cache label, and rendering
documents requires `docs`. Use `safe = FALSE` to retrieve a declared
final output path.

``` r

projr_path_get("raw-data-public")
projr_path_get("output-figures", "plot.png")
```

------------------------------------------------------------------------

## 7. Use profiles

Profiles let you maintain different configurations for different
contexts (development vs production, different machines, etc.).

``` r

# Create a profile (creates _projr-dev.yml)
projr_profile_create("dev")
```

Edit `_projr-dev.yml` to override specific settings. Unspecified keys
fall back to `_projr.yml`.

``` yaml
directories:
  output:
    path: _output_dev
```

Activate a profile:

``` r

Sys.setenv(PROJR_PROFILE = "dev")
projr_build()

# Check active profile
projr_profile_get()

# Delete a profile
projr_profile_delete("dev")
```

Remember to unset `PROJR_PROFILE` when switching back to the default
configuration.

### Keep profile directories out of Git

Suppose `raw-data` points to `lab/measurements` in `_projr.yml`. On a
cluster, you keep the data in `incoming/assays`. Set the latter path in
`_projr-cluster.yml` and give temporary work its own rule.

``` yaml
directories:
  raw-data:
    path: incoming/assays
  cache-scratch:
    path: runs/scratch
    ignore-even-if-inactive: false
```

Run
[`projr_ignore_auto()`](https://satvilab.github.io/projr/reference/projr_ignore_auto.md)
to refresh the rules. Git ignores both data directories whichever
profile is active. The cluster profile contributes the rule for
`runs/scratch` only while it is active. Paths are literal and relative
to the project root. An absolute path contributes a rule only when it
lies inside the checkout. An inactive profile can name a drive on
another machine without requiring access to it.

`ignore-even-if-inactive` defaults to `true`. It extends the directory’s
existing `ignore`, `ignore-git`, and `ignore-rbuild` settings across
profile switches. Each ignore file includes a path if any contributing
profile enables it for that file. projr applies this to `.gitignore`
and, when the project root contains `DESCRIPTION`, to `.Rbuildignore`.

If you change `raw-data` to another path, run
[`projr_ignore_auto()`](https://satvilab.github.io/projr/reference/projr_ignore_auto.md).
The next refresh drops the old path’s generated rule as soon as its last
qualifying profile reference is removed. Adding a manual ignore to an
older project also refreshes its managed rules. Entries above and below
the managed section stay in place.

A directory path can also use build placeholders. For example,
`results/@branch/run-@uuid` gives each build its own directory. On
branch `feature/foo`, the branch component is `feature_2ffoo`. A
detached checkout uses `__detached-` followed by the full commit SHA.
`@version` gives the projr version being built, such as `v1.2.3`. The
UUID is shared with child renders during a build. Outside a build it
stays stable for the R session.

A placeholder can occupy an entire component. It also expands before a
hyphen, underscore, period, space, or path separator. For example,
`@version_report.csv` expands and `@versioned` stays literal.

projr writes ignore patterns for the template itself, so another build’s
UUID is already covered. A branch placeholder spans a path component in
Git’s ignore syntax. Give it a dedicated parent, such as
`results/@branch`, when neighboring source directories should remain
trackable.

------------------------------------------------------------------------

## 8. View and manage build logs

Build history is logged to `.projr/logs/{dev,output}/history/builds.md`.
Each build also creates a detailed Quarto log file organised by date.

``` r

# View build logs
projr_log_view()

# List detailed log files
list.files(".projr/logs/dev/output",
  recursive = TRUE, pattern = "\\.qmd$"
)
```

Control logging:

``` r

# Disable detailed log files (history still maintained)
Sys.setenv(PROJR_LOG_DETAILED = "FALSE")

# Re-enable
Sys.setenv(PROJR_LOG_DETAILED = "TRUE")
```

Clear old logs:

``` r

projr_log_clear()
projr_log_clear(build_type = "dev")
projr_log_clear(before_date = "2025-01-01")
```

Logs are never cleared automatically — manage them manually.

------------------------------------------------------------------------

## 9. Query the manifest

Each final build records file hashes in the per-directory histories
under `.projr/manifests/`. Identical directory contents share one file
list.

By default, projr hashes files in C++ using one core. To use up to four
cores, add the following to `_projr.yml` or to a profile.

``` yaml
settings:
  hashing:
    approach: cpp
    cores: 4
```

Set `approach: legacy` to use the historical R implementation. Both
approaches produce the same MD5s at any core count, so changing either
setting preserves the directory version for unchanged files.

Compare two versions:

``` r

projr_manifest_changes("0.0.1", "0.0.2")
projr_manifest_changes("0.0.1", "0.0.2", label = "output")
```

Track files across a range of versions:

``` r

projr_manifest_range("0.0.1")
projr_manifest_range("0.0.1", "0.0.5", label = "raw-data")
```

See the most recent build that recorded files for each label. This can
include builds that reused earlier files.

``` r

projr_manifest_last_change()
projr_manifest_last_change("0.0.3")
```

Practical example — detect raw data changes since a release:

``` r

raw_changes <- projr_manifest_changes(
  "0.1.0", projr_version_get(), label = "raw-data"
)
if (nrow(raw_changes) > 0) {
  message("Raw data has changed - outputs may need regeneration")
}
```

Version strings accept both `"0.0.1"` and `"v0.0.1"`. All query
functions return a 0-row data.frame when there are no results.

------------------------------------------------------------------------

## 10. Configure environment variables

Create environment files at the project root:

``` r

file.create("_environment")       # Global defaults
file.create("_environment-dev")   # Profile-specific overrides
file.create("_environment.local") # Machine-specific, git-ignored
```

Add variables in `KEY=value` format:

``` bash
# _environment
PROJR_OUTPUT_LEVEL=std
PROJR_LOG_DETAILED=TRUE

# _environment.local (for secrets — git-ignored)
GITHUB_PAT=your_token_here
```

Load them:

``` r

projr_env_set()
```

Key variables:

| Variable             | Values           | Purpose                      |
|----------------------|------------------|------------------------------|
| `PROJR_OUTPUT_LEVEL` | none, std, debug | Console verbosity            |
| `PROJR_LOG_DETAILED` | TRUE, FALSE      | Create detailed log files    |
| `PROJR_CLEAR_OUTPUT` | pre, post, never | When to clear output dirs    |
| `PROJR_PROFILE`      | profile name     | Active configuration profile |

See
[`?projr_env_set`](https://satvilab.github.io/projr/reference/projr_env_set.md)
and the [Environment Variables
article](https://satvilab.github.io/projr/articles/environment.md) for
full details.
