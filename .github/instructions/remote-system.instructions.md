---
applyTo: "R/remote*.R"
excludeAgent: copilot_code_review
---

# Remote System Guidelines for projr

## Purpose & Scope

Guidelines for working with the projr remote destination system, including GitHub releases, OSF nodes, and local remotes. This covers remote creation, file operations, version tracking, and the httr-based GitHub implementation.

---

## Core Concepts

### Remote Types

projr supports three remote destination types:

- **local** - Local filesystem directories
- **github** - GitHub releases (using httr API, not piggyback)
- **osf** - Open Science Framework nodes

### Remote Hierarchy

The remote system uses a two-level hierarchy:

1. **remote_pre** - The "parent" remote (e.g., GitHub release tag, OSF node, or local directory)
2. **remote_final** - The actual destination within the parent (e.g., specific asset file in a release, subdirectory in local filesystem)

#### Examples

**GitHub:**
- `remote_pre`: `c("tag" = "v0.0.1")`
- `remote_final`: `c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")`

**Local:**
- `remote_pre`: `/path/to/destination`
- `remote_final`: `/path/to/destination/output/v0.0.1`

**OSF:**
- `remote_pre`: `osf_tbl_node` object
- `remote_final`: `osf_tbl_file` object within the node

### Structure Types

Remotes support two structure types (set in `_projr.yml`):

- **latest** - Overwrites files at destination (no versioning)
  - Local: `/dest/output/` (files overwritten)
  - GitHub: `output.zip` in release (asset overwritten)
  
- **archive** - Creates versioned subdirectories/files
  - Local: `/dest/output/v0.0.1/`, `/dest/output/v0.0.2/`, etc.
  - GitHub: `output-v0.0.1.zip`, `output-v0.0.2.zip` in release

---

## File Organization

### Remote Files by Purpose

- **R/remote.R** - Core dispatcher functions that delegate to type-specific implementations
- **R/remote-github.R** - GitHub-specific logic (delegates to httr implementations)
- **R/remote-github-httr.R** - Direct httr API implementations for GitHub releases
- **R/remote-local.R** - Local filesystem remote operations
- **R/remote-osf.R** - OSF remote operations
- **R/remote-misc.R** - Utility functions (tag handling, etc.)
- **R/remote-versions.R** - Version tracking and manifest operations for remotes

### Function Naming Conventions

**Dispatcher pattern** (in R/remote.R):
```r
.remote_check_exists(type, ...)  # Dispatcher
.remote_ls_final(type, ...)      # Dispatcher
```

**Type-specific implementations:**
```r
.remote_check_exists_github(tag, ...)        # In R/remote-github.R
.remote_check_exists_github_httr(repo, tag, ...) # In R/remote-github-httr.R
.remote_ls_final_github(remote_pre)          # In R/remote-github.R
.remote_ls_final_github_httr(repo, tag)      # In R/remote-github-httr.R
```

**Key patterns:**

- Functions in `R/remote.R` take `type` as first argument and dispatch via `switch()`
- Type-specific functions in `R/remote-{type}.R` don't take `type` argument
- `*_httr` suffix indicates direct httr implementation (GitHub only)
- Order matters: `remote_final_*` vs `.remote_*_final` - use existing patterns consistently

---

## GitHub Implementation Details

### httr vs piggyback

The package previously used the `piggyback` package for GitHub release operations but has migrated to direct `httr` API calls for better performance and control.

**Current approach:**
- All GitHub release operations use `httr` package directly
- Functions in `R/remote-github-httr.R` contain the actual API implementations
- Functions in `R/remote-github.R` wrap httr functions with retry logic and validation

**Do NOT:**
- Add new dependencies on `piggyback` 
- Use `piggyback::pb_*()` functions in new code
- Mix piggyback and httr approaches

### GitHub API Authentication

All GitHub API calls require authentication:

```r
# Before any GitHub API operation
.auth_check_github("operation description")

# Get token with fallback
token <- .auth_get_github_pat_find(api_url = api_url)
```

Token precedence (implemented in `.auth_get_github_pat_find()`):
1. `GITHUB_PAT` environment variable (highest priority)
2. `gh::gh_token()` if available
3. `gitcreds::gitcreds_get()` if available
4. `GITHUB_TOKEN` environment variable (lowest priority)

### GitHub API Base URL

```r
.github_api_base(api_url = NULL)
```

Returns the GitHub API base URL, resolving in order:
1. Explicit `api_url` argument
2. `GITHUB_API_URL` environment variable  
3. Default: `"https://api.github.com"`

Always removes trailing slashes for consistent URL construction.

### Retry Mechanism

GitHub operations use retry with exponential backoff:

```r
.retry_with_backoff(
  fn = function() { /* operation */ },
  max_attempts = 3,
  max_delay = 300,
  initial_delay = 2,
  operation_name = "operation description",
  output_level = "std",
  log_file = NULL,
  check_success = function(x) TRUE
)
```

Applied to:
- Release creation
- Release existence checks
- Asset uploads/downloads
- Asset listing

---

## Common Operations

### Check Remote Existence

```r
# Check if a remote exists
.remote_check_exists("github", id = "v0.0.1")
.remote_check_exists("local", id = "/path/to/dir")
.remote_check_exists("osf", id = "abc123")

# Check if a specific final remote exists
.remote_final_check_exists(
  "github",
  id = "v0.0.1",
  label = "output",
  structure = "archive",
  path = NULL,
  path_append_label = TRUE,
  version = "0.0.1"
)
```

### Create Remote

```r
# Create a GitHub release
.remote_create(
  "github",
  id = "v0.0.1",
  name = "Version 0.0.1",
  output_level = "std",
  log_file = NULL
)

# Create OSF node
.remote_create(
  "osf",
  id = NULL,  # Auto-generated
  name = "My Project"
)

# Create local directory
.remote_create("local", id = "/path/to/dir")
```

### Get Remote Objects

```r
# Get remote_pre object
remote_pre <- .remote_get("github", id = "v0.0.1")
# Returns: c("tag" = "v0.0.1")

# Get remote_final object
remote_final <- .remote_final_get(
  "github",
  id = "v0.0.1",
  label = "output",
  structure = "archive",
  version = "0.0.1"
)
# Returns: c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")
```

### List Files in Remote

```r
# List all final remotes in a remote_pre
.remote_ls_final("github", remote_pre = c("tag" = "v0.0.1"))
# Returns vector of asset filenames

# List files within a specific final remote
.remote_file_ls("github", remote = c("tag" = "v0.0.1", "fn" = "output.zip"))
# Returns vector of file paths inside the zip
```

### File Operations

```r
# Add files to remote
.remote_file_add(
  "github",
  fn = c("file1.txt", "file2.txt"),
  path_dir_local = "/local/path",
  remote = c("tag" = "v0.0.1", "fn" = "output.zip")
)

# Download all files from remote
.remote_file_get_all(
  "github",
  remote = c("tag" = "v0.0.1", "fn" = "output.zip"),
  path_dir_save_local = "/save/path"
)

# Remove specific files
.remote_file_rm(
  "github",
  fn = c("file1.txt", "file2.txt"),
  remote = c("tag" = "v0.0.1", "fn" = "output.zip")
)

# Empty a remote (remove all content)
.remote_final_empty(
  "github",
  remote = c("tag" = "v0.0.1", "fn" = "output.zip")
)
```

---

## Remote Configuration in YAML

Remotes are configured in `_projr.yml` under `build.{type}` keys:

```yaml
build:
  github:
    id: "my-release"
    send:
      - label: output
        structure: archive
        send_cue: if-change
        send_strategy: sync-diff
        send_inspect: manifest
  
  local:
    id: "/path/to/destination"
    send:
      - label: output
        structure: latest
        send_cue: always
```

### Send Configuration Options

**send_cue** - When to send to remote:
- `always` - Creates new remote version every build
- `if-change` - Only sends when content changes
- `never` - Never sends (useful for dev builds)

**send_strategy** - How to update remote:
- `sync-diff` - Upload only changed files (efficient)
- `sync-purge` - Remove all, then upload all
- `upload-all` - Upload all files (may overwrite)
- `upload-missing` - Only upload files not present on remote

**send_inspect** - How to detect existing versions:
- `manifest` - Use manifest.csv to track versions
- `file` - Inspect actual files on remote
- `none` - Treat remote as empty

---

## Testing Guidelines

### Test Structure

- **test-remote-github.R** - The ONLY file that creates/deletes GitHub releases
- Uses fixed test releases (`projr-test-release-a`, `projr-test-release-b`)
- Tests are idempotent - can be re-run safely
- All tests skip in CRAN and LITE modes

### Test Requirements

```r
test_that("GitHub operation", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()
  
  # Test implementation
})
```

### Test Helper Pattern

```r
# Create test project (reuse across tests)
dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)

usethis::with_project(
  path = dir_test,
  code = {
    # Test operations within project context
  }
)
```

### GitHub Test Caveats

- GitHub API operations are asynchronous - may need polling/waiting
- Use retry logic when checking for uploaded assets
- Clean up test artifacts but reuse releases across tests
- Test files may temporarily reference `piggyback::pb_list()` for verification during migration

---

## Common Patterns

### Error Handling

```r
# Check existence before operations
if (!.remote_check_exists("github", tag)) {
  .remote_create("github", tag)
}

# Use tryCatch for optional operations
tryCatch(
  .remote_file_get_all("github", remote, path_save),
  error = function(e) {
    .cli_debug("Download failed: {e$message}")
    FALSE
  }
)
```

### Polling for Async Operations

```r
# Poll for GitHub asset availability
start_time <- proc.time()[3]
max_wait <- 120
asset_exists <- FALSE

while (!asset_exists && (proc.time()[3] - start_time < max_wait)) {
  Sys.sleep(5)
  asset_exists <- .remote_final_check_exists("github", ...)
}
```

### Logging

All remote operations support logging:

```r
.remote_create(
  "github",
  id = tag,
  output_level = "debug",  # "none", "std", "debug"
  log_file = "/path/to/log.txt"
)
```

---

## When Adding New Remote Operations

1. Add dispatcher function to `R/remote.R` if needed
2. Implement type-specific functions in `R/remote-{type}.R`
3. For GitHub, implement httr API calls in `R/remote-github-httr.R`
4. Add authentication checks using `.auth_check_github()` or `.auth_check_osf()`
5. Add retry logic with `.retry_with_backoff()` for network operations
6. Update tests in appropriate test files
7. Document in roxygen2 with `@keywords internal`

---

## Version Tracking

Remote versions are tracked in `R/remote-versions.R`:

```r
# Get latest version from remote
.remote_get_version_label(remote_pre, type, label, structure)

# Get version file from remote
.remote_get_version_file(remote_pre, type, structure)

# Functions handle both archive and latest structures
```

---

## Migration Notes

### From piggyback to httr

The codebase is transitioning from piggyback to httr for GitHub operations:

**Completed:**
- Release existence checks
- Release creation
- Asset listing
- Asset uploads/downloads
- Asset deletion

**In Progress:**
- Test file cleanup to remove piggyback references

**When updating code:**
- Use functions in `R/remote-github-httr.R`
- Remove `piggyback::` calls
- Use `.retry_with_backoff()` for reliability
- Ensure proper authentication checks

---

## Common Pitfalls

- **Don't** use `piggyback` for new GitHub operations
- **Don't** forget authentication checks before API calls
- **Don't** assume GitHub operations are synchronous - use polling
- **Do** use retry logic for network operations
- **Do** maintain consistent naming: type-specific functions don't take `type` argument
- **Do** test with skip conditions: CRAN, LITE, offline, auth checks
- **Do** use `remote_pre` and `remote_final` terminology consistently
