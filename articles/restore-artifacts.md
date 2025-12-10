# Restoring Artifacts

Artifact restoration allows you to download versioned project components
from remote sources. This is useful for setting up projects on new
machines, sharing reproducible research with collaborators, and
recovering data from archived versions.

This documentation covers the three restoration workflows and their
configuration options.

## Overview

projr provides functions to restore project artifacts (raw data,
outputs, documents) from remote sources. These functions download
versioned files that were previously archived during builds.

### What Are Artifacts?

In projr, **artifacts** are the versioned components of your project:

- **raw-data** - Source data files
- **cache** - Intermediate computation results  
- **output** - Final analysis outputs (figures, tables)
- **docs** - Rendered documents (HTML, PDF)
- **code** - All Git-tracked source files

These artifacts are archived to remote destinations during production
builds and can be restored later.

### When to Restore

Common scenarios for restoration:

- **New machine setup** - Clone repository and restore data to start
  working
- **Collaboration** - Team member needs project data to reproduce
  analysis
- **Disaster recovery** - Lost local files need to be recovered from
  archives
- **Reproducibility** - Restore inputs that created published outputs
  (always gets latest remote version)

## Restoration Functions

projr provides three restoration functions for different workflows:

1.  **[`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
    Clone repository and restore artifacts (most common)
2.  **[`projr_restore_repo_wd()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
    Restore into current directory
3.  **[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
    Restore artifacts in existing local project

### projr_restore_repo()

Clone a GitHub repository and restore artifacts in one step.

**Syntax:**

``` r
library(projr)

projr_restore_repo(
  repo, # "owner/repo" or "repo"
  path = NULL, # Where to clone (default: creates subdirectory)
  label = NULL, # Which artifacts (default: all raw artifacts)
  pos = NULL, # Source position (default: both "source" and "dest")
  type = NULL, # Remote type (default: first available)
  title = NULL # Remote title (default: first available for type)
)
```

**Basic usage:**

``` r
# Clone into subdirectory and restore all raw artifacts
projr_restore_repo("owner/repo")

# Clone to specific location
projr_restore_repo("owner/repo", path = "~/projects/my-analysis")

# Clone into current directory (use with caution!)
projr_restore_repo("owner/repo", path = ".")
```

**What it does:**

1.  Clones repository into specified directory (or creates subdirectory)
2.  Reads `_projr.yml` to find configured remotes
3.  Restores artifacts (default: all `raw-*` labels like raw-data; cache
    is NOT included)
4.  Returns `TRUE` if successful, `FALSE` otherwise

### projr_restore_repo_wd()

Clone directly into current working directory, then restore artifacts.

**Syntax:**

``` r
projr_restore_repo_wd(
  repo, # "owner/repo" or "repo"
  label = NULL, # Which artifacts
  pos = NULL, # Source position
  type = NULL, # Remote type
  title = NULL # Remote title
)
```

**Usage:**

``` r
# From within target directory
setwd("~/projects/my-analysis")
projr_restore_repo_wd("owner/repo")
```

**Warning:** This creates files directly in your current directory.
Ensure you’re in the right location!

### projr_content_update()

Restore artifacts in an existing project without cloning.

**Syntax:**

``` r
projr_content_update(
  label = NULL, # Which artifacts (default: all raw artifacts)
  pos = NULL, # Source position (default: both)
  type = NULL, # Remote type (default: first available)
  title = NULL # Remote title (default: first available for type)
)
```

**Usage:**

``` r
# Navigate to project first
setwd("~/projects/my-analysis")

# Restore all raw artifacts
projr_content_update()

# Restore specific artifacts
projr_content_update(label = "raw-data")
projr_content_update(label = c("raw-data", "cache"))
```

**Requires:** Project must have `manifest.csv` file in root directory.

## Parameters

### label Parameter

Controls which artifacts are restored.

**`label = NULL`** (default) - Restore all raw-\* artifacts

``` r
# Restores: raw-data and any other raw-* directories
# Note: cache is NOT restored by default (not a raw-* label)
projr_content_update()

# To restore cache explicitly:
projr_content_update(label = c("raw-data", "cache"))
```

**`label = "raw-data"`** - Restore specific artifact

``` r
projr_content_update(label = "raw-data")
```

**`label = c("raw-data", "cache")`** - Restore multiple artifacts

``` r
projr_content_update(label = c("raw-data", "cache"))
```

**Valid labels:**

Any directory label defined in your `_projr.yml`:

- `raw-data` - Source data files
- `cache` - Cached computation results
- `output` - Analysis outputs (use cautiously - usually regenerated)
- `docs` - Rendered documents (use cautiously - usually regenerated)

**When to restore outputs/docs:**

- Comparing results across versions
- When computation is expensive and results are archived
- Generally, prefer regenerating outputs with
  [`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md)

### type Parameter

Specifies which remote type to use.

**`type = NULL`** (default) - Use first available remote

``` r
# Checks remotes in the order they appear in _projr.yml
# (not a fixed order - depends on your configuration)
projr_content_update()
```

**`type = "github"`** - Use GitHub releases only

``` r
projr_content_update(type = "github")
```

**`type = "local"`** - Use local directory only

``` r
projr_content_update(type = "local")
```

**`type = "osf"`** - Use OSF only

``` r
projr_content_update(type = "osf")
```

**When to specify:**

- Multiple remotes configured and you want a specific one
- Testing specific remote sources
- One remote is faster or more reliable

### title Parameter

Selects a specific remote configuration when multiple exist.

**`title = NULL`** (default) - Use first available title for selected
type

``` r
projr_content_update()
```

**`title = "network-backup"`** - Use specific remote

``` r
projr_content_update(type = "local", title = "network-backup")
```

**Example with multiple local remotes:**

``` yaml
# _projr.yml
build:
  local:
    local-backup:
      title: "local-backup"
      content: [raw-data]
      path: "~/backup/raw-data"
    network-backup:
      title: "network-backup"
      content: [raw-data]
      path: "/mnt/shared/raw-data"
```

``` r
# Restore from local backup
projr_content_update(type = "local", title = "local-backup")

# Restore from network backup
projr_content_update(type = "local", title = "network-backup")
```

### pos Parameter

Controls whether to restore from source directories or build
destinations.

**`pos = NULL`** (default) - Check both in order

``` r
# Checks "source" first, then "dest"
projr_content_update()
```

**`pos = "source"`** - Source directories only

``` r
projr_content_update(pos = "source")
```

**`pos = "dest"`** - Build destinations only

``` r
projr_content_update(pos = "dest")
```

**`pos = c("source", "dest")`** - Both (explicit)

``` r
projr_content_update(pos = c("source", "dest"))
```

**When to use:**

- Usually the default is appropriate
- Use `pos = "dest"` if you specifically archived outputs and want to
  restore them
- Use `pos = "source"` if you only want original source artifacts

## Authentication

### GitHub Authentication

Restoring from GitHub requires authentication.

**Required:** GitHub Personal Access Token (PAT)

**Setup:**

``` r
# Get detailed instructions
projr_instr_auth_github()
```

**Set environment variable:**

``` bash
# In _environment.local (never commit!)
GITHUB_PAT=ghp_your_token_here
```

**Verify:**

``` r
# Check token is set
Sys.getenv("GITHUB_PAT") # Should show your token
```

### OSF Authentication

Restoring from OSF requires authentication.

**Required:** OSF Personal Access Token

**Setup:**

``` r
# Get detailed instructions
projr_instr_auth_osf()
```

**Set environment variable:**

``` bash
# In _environment.local
OSF_PAT=your_osf_token_here
```

### Local Directories

No authentication needed, but ensure:

- Directory paths are accessible
- You have read permissions
- Network drives are mounted (if applicable)

## Complete Examples

### Example 1: New Collaborator Setup

Set up a project on a new machine:

``` r
# Step 1: Clone and restore
projr_restore_repo("satvilab/my-study")

# Step 2: Navigate into project
setwd("my-study")

# Step 3: Install R dependencies
renv::restore()

# Step 4: Run analysis
projr_build_dev() # Test build
projr_build_patch() # Production build when ready
```

### Example 2: Selective Restoration

Restore only specific artifacts, not all:

``` r
# Step 1: Clone repository
projr_restore_repo("owner/repo")
setwd("repo")

# Step 2: Restore only raw data (not cache or other artifacts)
projr_content_update(label = "raw-data")

# Step 3: Regenerate other artifacts by running analysis
projr_build_dev()
```

### Example 3: Selective Restoration

Restore only raw data, not cached results:

``` r
# Restore only raw data
projr_content_update(label = "raw-data")

# Regenerate cache by running analysis
projr_build_dev()
```

### Example 4: Fallback to Different Remote

Try multiple remote sources:

``` r
# Try GitHub first
result <- tryCatch(
  projr_content_update(type = "github"),
  error = function(e) FALSE
)

# Fallback to local if GitHub fails
if (!result) {
  projr_content_update(type = "local")
}
```

### Example 5: Network Drive Restoration

Restore from shared network drive:

``` yaml
# _projr.yml configuration
build:
  local:
    network-data:
      title: "network-data"
      content: [raw-data]
      path: "/mnt/shared/project-data"
```

``` r
# Restore from network drive
projr_content_update(type = "local", title = "network-data")
```

## How Restoration Works

### The Manifest System

projr uses `manifest.csv` to track file versions and hashes.

**Manifest structure:**

    label      | fn                | version | hash
    -----------|-------------------|---------|----------------------------------
    raw-data   | data/survey.csv   | v0.1.0  | 5d41402abc4b2a76b9719d911017c592
    raw-data   | data/survey.csv   | v0.2.0  | 5d41402abc4b2a76b9719d911017c592
    output     | figures/plot.png  | v0.1.0  | 098f6bcd4621d373cade4e832627b4f6

**How it’s used:**

1.  projr reads `manifest.csv` from your project to identify available
    artifacts
2.  Determines which remote version to restore (latest for archive
    remotes)
3.  Downloads files from configured remotes

### Version Resolution

projr determines which version to restore from remotes:

**For archive remotes** (most common): Always restores the latest
available version from the remote, regardless of your current Git
checkout or project version.

**For latest remotes**: Restores the current snapshot.

**Example:**

    Remote has archived versions: v0.1.0, v0.2.0, v0.3.0
    Your Git checkout: v0.1.0
    Result: Still restores v0.3.0 (latest available on remote)

**Note:**
[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md)
always fetches the newest remote version for archive remotes. To work
with older artifact versions, manually download them from the remote
source (e.g., GitHub Releases).

### Restoration Process

Step-by-step restoration:

1.  **Read configuration** - Parse `_projr.yml` to find remote sources
2.  **Read manifest** - Load `manifest.csv` to identify available
    artifacts
3.  **Select sources** - Choose appropriate remotes for each label
4.  **Determine version** - Identify which remote version to restore
    (latest for archives)
5.  **Download files** - Retrieve files from remotes and place in local
    directories

### Remote Source Priority

projr checks remote sources in the order they appear in your
`_projr.yml` configuration. The order depends on how you’ve configured
your remotes and is not fixed.

When `type = NULL` (default), projr:

1.  Looks for sources or destinations in the order listed under `build:`
    in `_projr.yml`
2.  Uses the first remote that has the requested artifact configured

The specific order depends on your project’s configuration.

## Best Practices

### For Project Setup

**Archive raw data:**

``` r
# Configure raw data archiving (one-time setup)
projr_yml_dest_add_github(
  title = "raw-data-@version",
  content = "raw-data",
  send_cue = "if-change"
)
```

**Document restoration:**

Add to your project `README.md`:

``` markdown
## Setup

1. Restore project: `projr::projr_restore_repo("owner/repo")`
2. Install dependencies: `renv::restore()`
3. Build project: `projr::projr_build_dev()`
```

### For Collaborators

**Share restoration instructions:**

``` r
# Option 1: Automated setup
projr::projr_restore_repo("owner/repo")
setwd("repo")
renv::restore()

# Option 2: Manual clone, then restore
# git clone https://github.com/owner/repo
# cd repo
# R -e "projr::projr_content_update()"
```

**Test restoration:**

``` r
# Test in clean environment before sharing
tempdir <- tempdir()
projr::projr_restore_repo("owner/repo", path = tempdir)
```

### For Reproducibility

**Archive everything needed:**

``` r
# Archive code (all Git-tracked files)
projr_yml_dest_add_github(
  title = "code-@version",
  content = "code"
)

# Archive raw data
projr_yml_dest_add_github(
  title = "raw-data-@version",
  content = "raw-data"
)

# Archive outputs for comparison
projr_yml_dest_add_github(
  title = "output-@version",
  content = "output"
)
```

**Document dependencies:**

Use `renv` for R package versions:

``` r
renv::snapshot()
```

Document system dependencies in `README.md`:

``` markdown
## System Requirements

- R version 4.3.0 or higher
- System libraries: libcurl, libxml2
- pandoc (for rendering documents)
```

### For Version Control

**Note on version history:**

[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md)
always fetches the latest remote version for archive remotes. To work
with artifacts from older versions, you’ll need to manually download
them from the remote source or implement custom restoration logic.

**Document version history:**

Keep notes in `NEWS.md` or `CHANGELOG.md`:

``` markdown
## Version 0.3.0

- Updated analysis with 2024 data
- Added sensitivity analyses

## Version 0.2.0

- Revised methods based on reviewer feedback
```

## Common Pitfalls

### No Manifest File

**Problem:** `"No manifest.csv found"`

**Solutions:**

- The project needs to have been built at least once with projr
- Clone a project that has manifest in its repository
- If manifest is missing, manually download files from GitHub releases

### No Remote Sources

**Problem:** `"No remote sources configured"`

**Solutions:**

- Check `_projr.yml` has remote destinations under `build:`
- Verify remote configurations are valid with
  [`projr_yml_check()`](https://satvilab.github.io/projr/reference/projr_yml_check.md)
- Manually download files from remote sources

### Authentication Errors

**Problem:** `"Authentication required for GitHub"`

**Solutions:**

- Set up `GITHUB_PAT` environment variable
- Run
  [`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)
  for instructions
- Verify token with `Sys.getenv("GITHUB_PAT")`
- Ensure token has `repo` scope

### Download Failures

**Problem:** Failed to download from remote

**Solutions:**

- Check internet connection
- Verify remote still exists (GitHub release, local path)
- Check authentication if required
- Try different remote type: `projr_content_update(type = "local")`

### Hash Mismatches

**Problem:** `"File hash mismatch"`

**Solutions:**

- File on remote may be corrupted
- File may have been modified outside projr
- Try restoring from different remote
- Check if remote was updated manually

### Partial Restoration

**Problem:** Some artifacts restore, others fail

**Behavior:** projr continues with available artifacts

**Recovery:**

``` r
# Try different remote type
projr_content_update(type = "local")

# Try specific artifact
projr_content_update(label = "raw-data")

# Manual download as last resort
# Download from GitHub releases manually
```

## Troubleshooting

### Check Configuration

View remote configuration:

``` r
# View entire configuration
projr_yml_get()

# View specific sections
projr_yml_get()$build$github
projr_yml_get()$build$local

# Validate configuration
projr_yml_check()
```

### Check Manifest

Inspect manifest contents:

``` r
# Read manifest
manifest <- read.csv("manifest.csv")

# View entries for specific label
manifest[manifest$label == "raw-data", ]

# View available versions
unique(manifest$version)
```

### Verbose Output

Enable detailed output:

``` r
# Set environment variable
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")

# Run restoration
projr_content_update()

# Check detailed logs
# Located in _tmp/projr/log/
```

### Test Restoration

Test in isolated environment:

``` r
# Create test directory
test_dir <- tempfile()
dir.create(test_dir)

# Test restoration
result <- tryCatch(
  projr_restore_repo("owner/repo", path = test_dir),
  error = function(e) {
    message("Restoration failed: ", e$message)
    FALSE
  }
)

# Clean up
if (result) {
  message("Restoration successful!")
  unlink(test_dir, recursive = TRUE)
}
```

## Summary

### Quick Start

``` r
# Most common: Clone and restore
projr_restore_repo("owner/repo")

# In existing project: Restore artifacts
projr_content_update()

# Specific artifact: Restore raw data only
projr_content_update(label = "raw-data")
```

### Key Functions

- **[`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
  Clone repository and restore artifacts
- **[`projr_restore_repo_wd()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
  Restore into current directory
- **[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md)** -
  Restore in existing project

### Key Parameters

- **label** - Which artifacts to restore (default: all raw-\* artifacts)
- **type** - Which remote type (github, local, osf; default: first
  available)
- **title** - Which remote configuration (default: first available for
  type)
- **pos** - Source position (source, dest; default: both)

### Key Concepts

- **Artifacts** - Versioned project components (raw-data, output, docs,
  code)
- **Manifest** - CSV file tracking file versions and hashes
- **Remotes** - Sources from which artifacts can be restored
- **Version resolution** - Determining which version to restore

### See Also

- [`?projr_restore`](https://satvilab.github.io/projr/reference/projr_restore.md) -
  Full restoration documentation
- [`?projr_restore_repo`](https://satvilab.github.io/projr/reference/projr_restore.md) -
  Repository restoration documentation
- [`vignette("send-to-remotes")`](https://satvilab.github.io/projr/articles/send-to-remotes.md) -
  Configuring remotes for archiving
- [`vignette("environment")`](https://satvilab.github.io/projr/articles/environment.md) -
  Environment variables including `GITHUB_PAT`
