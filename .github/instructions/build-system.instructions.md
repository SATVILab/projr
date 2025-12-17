---
applyTo: "R/{build,manifest,hash,change,log,changelog,buildlog,dest}*.R"
---

# Build System Guidelines for projr

## Purpose & Scope

Guidelines for working with the projr build system, including build processes, logging, manifest tracking, and change detection.

---

## Build Process Overview

### Production Builds
- `projr_build_patch()` - Increment patch version (0.0.X)
- `projr_build_minor()` - Increment minor version (0.X.0)
- `projr_build_major()` - Increment major version (X.0.0)
- Can run from either release or dev versions

### Development Builds
- `projr_build_dev()` - Development build without version increment
- Automatically bumps to dev version if not already on one (e.g., 0.0.1 → 0.0.1-1)

### Build Phases

1. **Pre-build**
   - Clear output directories (based on `PROJR_CLEAR_OUTPUT` setting)
   - Run pre-build hooks
   - Hash input files (raw-data, cache) for manifest
   - Bump version (production builds only)

2. **Build**
   - Execute build scripts or auto-detected documents
   - Render Quarto/RMarkdown/Bookdown files

3. **Post-build**
   - Hash output files (output, docs) for manifest
   - Commit changes to git (if enabled)
   - Run post-build hooks
   - Distribute to remote destinations (see `remote-system.instructions.md` for details)
   - Bump to dev version (production builds only): e.g., 0.0.2 → 0.0.2-1
   - Commit dev version with message "Begin v{version}"

---

## Build Logging System

### Log Directory Structure

```
cache/projr/log/
├── output/                    # Production build logs
│   ├── history/builds.md      # All build records, newest first
│   └── output/YYYY-MMM-DD/    # Daily log folders
│       └── HH-MM-SS.qmd       # Detailed log for each build
└── dev/                       # Development build logs
    ├── history/builds.md
    └── output/YYYY-MMM-DD/
        └── HH-MM-SS.qmd
```

### Logging Control

Use environment variables to control logging:
- `PROJR_OUTPUT_LEVEL` - Console verbosity: `"none"`, `"std"`, `"debug"`
- `PROJR_LOG_DETAILED` - Create detailed log files: `TRUE`/`FALSE`

History tracking (`builds.md`) is always maintained regardless of settings.

### Debug Output

When `PROJR_OUTPUT_LEVEL="debug"`, detailed messages include:
- Remote operations (type, ID, file counts)
- File changes (added, modified, removed)
- Upload plans and strategies
- Build change summaries

---

## Manifest System

### Purpose

Tracks file hashes across versions in `manifest.csv` at project root.

### Manifest Structure

| Column | Description |
|--------|-------------|
| `label` | Directory label (e.g., "output", "raw-data", "cache", "docs") |
| `fn` | File path relative to directory |
| `version` | Project version when file was hashed (e.g., "v0.0.1") |
| `hash` | MD5 hash of file content |

### How Manifests Are Built

1. **Pre-build phase** (`.build_manifest_pre()`):
   - Hashes files in input directories (raw-data, cache)
   - Stores in temporary manifest file in cache

2. **Post-build phase** (`.build_manifest_post()`):
   - Hashes files in output directories (output, docs)
   - Merges with pre-build manifest
   - Appends to previous manifest versions
   - Writes to `manifest.csv`

### User-Facing Query Functions

```r
# Changes between two versions
projr_manifest_changes("0.0.1", "0.0.2")
projr_manifest_changes("0.0.1", "0.0.2", label = "output")

# Changes across version range
projr_manifest_range("0.0.1", "0.0.5")

# Last changes for current version
projr_manifest_last_change()
```

---

## Build Directory Clearing

### Clear Modes

Controlled by `PROJR_CLEAR_OUTPUT` environment variable:

| Mode | When Cleared | Safe Directories | Unsafe Directories |
|------|--------------|------------------|-------------------|
| `"pre"` (default) | Before build starts | ✓ Cleared | ✓ Cleared |
| `"post"` | After build completes | ✓ Cleared | ✗ Not cleared |
| `"never"` | Never | ✗ Not cleared | ✗ Not cleared |

**Safe directories**: Temporary build locations in cache (e.g., `_tmp/projr/v0.0.1/output`)
**Unsafe directories**: Final output locations (e.g., `_output`, `docs`)

### Pre-Clear Behavior

The `.build_clear_pre()` function:
1. Clears output directories (always clears safe, conditionally clears unsafe)
2. Clears versioned cache directory (preserves "old" subdirectory)
3. Does NOT clear docs directories (handled separately in post-build)

---

## Change Detection and Hashing

### Hash Functions

```r
# Hash one or more files
.hash_file(fn)  # Returns named character vector

# Hash all files in a directory
.hash_dir(path_dir, version = NULL, dir_exc = NULL)  # Returns data frame

# Compare two hash tables
.change_get_hash(hash_pre, hash_post)  # Returns list of changes

# Compare two directories
.change_get_dir(path_dir_pre, path_dir_post)  # Returns list of changes
```

### Change Detection Results

Functions return list with:
- `fn_dest_extra` - Files removed (in pre but not in post)
- `fn_same` - Files unchanged (in both with same hash)
- `fn_diff` - Files modified (in both with different hash)
- `fn_source_extra` - Files added (in post but not in pre)

### Key Principles

1. Hash tables always store relative paths
2. Versions are always prefixed with "v"
3. Empty directories return well-structured 0-row tables
4. Directory exclusions must be filtered before converting paths to absolute
5. `.hash_file()` returns named vectors - use `unname()` for comparisons

---

## Build Change Summary

Automatically tracks and reports file changes between builds.

### BUILDLOG.md Integration

- Change summaries added to `BUILDLOG.md` for each production build
- Compares current build with previous version using manifest hashes
- Tracks changes in input directories (raw-data, cache) and output directories (output, docs)
- Shows added, removed, modified, and unchanged file counts

### Format

- If total changes < 10: Shows individual file names
- If total changes ≥ 10: Shows only counts
- Organized by section: "Inputs Changes" and "Outputs Changes"
- Displays version comparison (e.g., "v0.0.1 → v0.0.2")

---

## Code Examples

```r
# Correct: Hash directory with exclusions
hash_tbl <- .hash_dir("_tmp", dir_exc = "projr")

# Correct: Compare two versions
hash_pre <- .hash_dir(projr_path_get_dir("output", safe = TRUE))
hash_post <- .hash_dir("_output")
changes <- .change_get_hash(hash_pre, hash_post)

# Correct: Check for changes
if (length(changes$fn_diff) > 0) {
  message("Modified files: ", paste(changes$fn_diff, collapse = ", "))
}

# Incorrect: Forgetting to exclude directories before making absolute
fn <- list.files(path_dir, recursive = TRUE)
fn_abs <- file.path(path_dir, fn)
fn_filtered <- .path_filter_spec(fn_abs, exc = "old")  # Too late!
```

---

## Implementation Files

- `R/build.R` - Main build process
- `R/build-pre.R` - Pre-build operations
- `R/build-post.R` - Post-build operations
- `R/build-manifest.R` - Manifest creation during builds
- `R/build-change-summary.R` - Change summary generation
- `R/build-pre-clear.R` - Directory clearing
- `R/build-hooks.R` - Hook execution
- `R/dest-send.R` - Post-build remote destination dispatch
- `R/dest-send-label.R` - Remote sending coordination (see remote-system.instructions.md)
- `R/dest-send-prepare.R` - Pre-send preparation
- `R/log.R` - Core logging functions
- `R/cli-output.R` - CLI output with integrated logging
- `R/manifest.R` - Core manifest operations
- `R/manifest-query.R` - User-facing query functions
- `R/hash.R` - Hashing functions
- `R/change.R` - Change detection functions
- `R/changelog.R` - Changelog generation
- `R/buildlog.R` - Build log generation

## When Modifying Build Code

- Ensure log messages are passed through `log_file` parameter
- Maintain logging functionality
- Test with different `PROJR_OUTPUT_LEVEL` settings
- Verify manifest updates work correctly
- Test both safe and unsafe directory modes
