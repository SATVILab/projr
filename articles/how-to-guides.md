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
projr_init_cite(title = "My Research Project", authors = "Jane Doe")
```

If GitHub credential prompts fail, run
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
for setup instructions. Initialisation won’t overwrite existing config
files.

------------------------------------------------------------------------

## 2. Run a development build

Use
[`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)
when iterating on code and documents before creating a versioned
release.

``` r
projr_build_dev()

# Build specific documents only
projr_build_dev("analysis.Rmd")
projr_build_dev(c("methods.Rmd", "results.Rmd"))
```

Dev builds route outputs to the cache at `_tmp/projr/v<version>/`
instead of `_output`. Always use
[`projr_path_get()`](https://satvilab.github.io/projr/reference/projr_path_get.md)
so paths resolve correctly in both dev and final builds:

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

projr supports three destination types: GitHub Releases, OSF, and local
directories. Configure them in `_projr.yml`:

``` yaml
build:
  github:
    raw-data:
      content: [raw-data]
      description: "Raw data files"
      cue: "always"
    output:
      content: [output]
      description: "Analysis outputs"

  local:
    path: "~/Dropbox/my-project-archive"
    raw-data:
      content: [raw-data]
      path: "data"
    output:
      content: [output]
      path: "outputs"
```

Or add destinations programmatically:

``` r
projr_yml_dest_add_github(
  label = "raw-data",
  content = "raw-data",
  description = "Raw data files"
)

projr_yml_dest_add_local(
  label = "raw-data",
  content = "raw-data",
  path = "~/archive/data"
)
```

Key parameters:

- `content`: directory to archive (raw-data, output, docs, cache)
- `cue`: when to upload — “always”, “new” (only if changed), “never”
- `strategy`: “archive” (versioned) or “latest” (overwrite)

GitHub has a 2 GB limit per release asset. Use local or OSF for larger
datasets.

------------------------------------------------------------------------

## 5. Restore artefacts

``` r
# Clone a repo and restore archived content
projr_restore_repo("owner/repo")
projr_restore_repo("owner/repo", path = "~/projects/my-project")

# Restore from the current working directory (already cloned)
projr_restore_repo_wd()
```

Restore specific directories:

``` r
projr_content_update(label = "raw-data")
projr_content_update(label = c("raw-data", "output"))
projr_content_update(label = "raw-data", version = "v0.1.0")
```

projr checks configured sources in order: GitHub Releases, OSF, then
local archives. Ensure credentials are set up before restoring from
remote sources.

------------------------------------------------------------------------

## 6. Define directories and labels

Default labels:

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

Labels must start with `raw`, `cache`, `output`, or `docs`. Each label
maps to a single, unique path. Directories are created automatically on
build.

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
build:
  github:
    enabled: false

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

------------------------------------------------------------------------

## 8. View and manage build logs

Build history is logged to
`cache/projr/log/{dev,output}/history/builds.md`. Each build also
creates a detailed Quarto log file organised by date.

``` r
# View build logs
projr_log_view()

# List detailed log files
list.files("cache/projr/log/dev/output",
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

The manifest (`manifest.csv`) tracks file hashes across versions. It is
updated only by final builds, not dev builds.

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

Check when directories last changed:

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
