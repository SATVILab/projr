---
applyTo: "R/remote*.R"
excludeAgent: copilot_code_review
---

# Remote System Guidelines for projr

## Purpose & Scope

Guidelines for working with projr remote destinations: GitHub releases, OSF nodes, and local filesystem. Covers remote creation, file operations, version tracking, and httr-based GitHub implementation.

---

## Core Concepts

### Remote Types

- **local** - Local filesystem directories
- **github** - GitHub releases (httr API, not piggyback)
- **osf** - Open Science Framework nodes

### Remote Hierarchy

Two-level hierarchy:

1. **remote_pre** - Parent remote (GitHub tag, OSF node, or local directory)
2. **remote_final** - Destination within parent (specific asset/file/subdirectory)

Examples:
- GitHub: `remote_pre = c("tag" = "v0.0.1")`, `remote_final = c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")`
- Local: `remote_pre = "/path/to/dest"`, `remote_final = "/path/to/dest/output/v0.0.1"`
- OSF: `remote_pre = osf_tbl_node`, `remote_final = osf_tbl_file`

### Structure Types

- **latest** - Overwrites files (no versioning)
- **archive** - Creates versioned subdirectories/files

---

## File Organization

**Key files:**
- `R/remote.R` - Dispatcher functions (take `type` first, use `switch()`)
- `R/remote-github.R` - GitHub logic (wraps httr with retry)
- `R/remote-github-httr.R` - Direct httr API implementations
- `R/remote-local.R` - Local filesystem operations
- `R/remote-osf.R` - OSF operations
- `R/remote-misc.R` - Utilities (tag handling)
- `R/remote-versions.R` - Version tracking, manifest operations

**Naming conventions:**
- Dispatchers: `.remote_check_exists(type, ...)`
- Type-specific: `.remote_check_exists_github(tag, ...)`
- httr implementations: `.remote_check_exists_github_httr(repo, tag, ...)`
- Type-specific functions don't take `type` argument
- Use existing patterns consistently

---

## GitHub Implementation

**Use httr, not piggyback:**
- All GitHub operations use `httr` directly
- `R/remote-github-httr.R` has API implementations
- `R/remote-github.R` wraps with retry logic
- DO NOT use `piggyback::pb_*()` functions

**Authentication:**
```r
.auth_check_github("operation description")  # Required before any gh:: call
token <- .auth_get_github_pat_find(api_url = api_url)
```

Token precedence: `GITHUB_PAT` > `gh::gh_token()` > `gitcreds::gitcreds_get()` > `GITHUB_TOKEN`

**API Base URL:**
`.github_api_base()` resolves: `api_url` arg > `GITHUB_API_URL` env > `"https://api.github.com"`

**Retry mechanism:**
Use `.retry_with_backoff()` for release creation, existence checks, asset operations

---

## Common Operations

```r
# Check existence
.remote_check_exists("github", id = "v0.0.1")
.remote_final_check_exists("github", id = "v0.0.1", label = "output", structure = "archive", version = "0.0.1")

# Create
.remote_create("github", id = "v0.0.1", name = "Version 0.0.1")
.remote_create("local", id = "/path/to/dir")

# Get objects
remote_pre <- .remote_get("github", id = "v0.0.1")  # Returns: c("tag" = "v0.0.1")
remote_final <- .remote_final_get("github", id = "v0.0.1", label = "output", structure = "archive", version = "0.0.1")

# List files
.remote_ls_final("github", remote_pre = c("tag" = "v0.0.1"))  # Lists assets
.remote_file_ls("github", remote = c("tag" = "v0.0.1", "fn" = "output.zip"))  # Lists files in zip

# File operations
.remote_file_add("github", fn = c("file1.txt"), path_dir_local = "/local", remote = c("tag" = "v0.0.1", "fn" = "output.zip"))
.remote_file_get_all("github", remote = c("tag" = "v0.0.1", "fn" = "output.zip"), path_dir_save_local = "/save")
.remote_file_rm("github", fn = c("file1.txt"), remote = c("tag" = "v0.0.1", "fn" = "output.zip"))
.remote_final_empty("github", remote = c("tag" = "v0.0.1", "fn" = "output.zip"))
```

---

## YAML Configuration

Configure remotes in `_projr.yml`:

```yaml
build:
  github:
    id: "my-release"
    send:
      - label: output
        structure: archive          # archive or latest
        send_cue: if-change        # always, if-change, never
        send_strategy: sync-diff   # sync-diff, sync-purge, upload-all, upload-missing
        send_inspect: manifest     # manifest, file, none
```

**send_cue:** When to send (`always`, `if-change`, `never`)
**send_strategy:** How to update (`sync-diff`, `sync-purge`, `upload-all`, `upload-missing`)
**send_inspect:** How to detect versions (`manifest`, `file`, `none`)

---

## Testing

**test-remote-github.R** - Only file that creates/deletes GitHub releases
- Uses fixed test releases (`projr-test-release-a`, `projr-test-release-b`)
- Idempotent, skips in CRAN/LITE modes
- GitHub operations are async - use polling/retry logic

**test-remote-local.R** - Local remote operations
- Uses temporary directories, runs on CRAN/CI
- Comprehensive tests in test-local-remote-comprehensive.R

**Test patterns:**
```r
# GitHub tests
test_that("GitHub operation", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  .test_skip_if_cannot_modify_github()
  # Test implementation
})

# Local tests
test_that("local operation", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(dir_test, { /* tests */ })
})
```

---

## Common Patterns

**Error handling:**
```r
if (!.remote_check_exists("github", tag)) .remote_create("github", tag)
tryCatch(.remote_file_get_all("github", remote, path), error = function(e) FALSE)
```

**Polling async operations:**
```r
start_time <- proc.time()[3]
while (!asset_exists && (proc.time()[3] - start_time < 120)) {
  Sys.sleep(5)
  asset_exists <- .remote_final_check_exists("github", ...)
}
```

**Logging:**
All operations support `output_level` ("none", "std", "debug") and `log_file` parameters.

---

## Adding New Operations

1. Add dispatcher to `R/remote.R` (takes `type`, uses `switch()`)
2. Implement type-specific in `R/remote-{type}.R`
3. For GitHub: implement httr in `R/remote-github-httr.R`
4. Add auth checks: `.auth_check_github()` or `.auth_check_osf()`
5. Add retry logic: `.retry_with_backoff()`
6. Update tests with proper skip conditions
7. Document with `@keywords internal`

---

## Version Tracking

Use `R/remote-versions.R` functions:
- `.remote_get_version_latest_label(remote_pre, type, label, structure)`
- `.remote_get_version_file(remote_pre, type, structure)`

---

## Post-Build Remote Sending Workflow

The remote sending process occurs after builds via `.dest_send()` → `.dest_send_type()` → `.dest_send_title()` → `.dest_send_label()`.

### Overview

1. **Destination dispatch** (`.dest_send()`)
   - Determines which remote types to process (local, github, osf)
   - Loops through each type, forwarding to `.dest_send_type()`

2. **Title processing** (`.dest_send_type()` → `.dest_send_title()`)
   - Gets destination titles from `_projr.yml` (e.g., "archive", "my-release")
   - Each title represents a logical destination group
   - Loops through labels (output, docs, etc.) for each title

3. **Label coordination** (`.dest_send_label()`)
   - Main orchestrator for a single label-to-destination upload
   - Resolves remote metadata, builds upload plan, executes plan
   - Emits detailed debug output when `output_level = "debug"`

### Label Workflow (`.dest_send_label()`)

The function coordinates these phases:

**Phase 1: Remote Resolution** (`.dsl_get_remotes()`)
- `remote_pre` - Parent remote (tag, node, or directory)
- `remote_dest_full` - Non-empty destination (if exists)
- `remote_dest_empty` - Empty placeholder destination (if exists)
- `remote_comp` - Comparison target for change detection
- `version_comp` - Trusted version for comparisons (NULL if no trusted version)

**Phase 2: Plan Generation** (`.dsl_get_plan()`)

Determines files to add/remove based on:
- `send_strategy` - How to update (sync-diff, sync-purge, upload-all, upload-missing)
- `send_inspect` - How to detect changes (manifest, file, none)
- `send_cue` - When to send (always, if-change, never)

Returns plan with:
- `fn_add` - Files to upload
- `fn_rm` - Files to remove
- `create` - Whether to create destination
- `purge` - Whether to purge before uploading
- `version` - Updated VERSION file content
- `manifest` - Updated manifest.csv content
- `changelog` - Whether to write changelog

**Phase 3: Plan Execution** (`.dsl_implement_plan()`)

Applies the plan:
1. Purge remote if requested (`.dsl_ip_purge()`)
2. Add files (`.dsl_ip_add()` → `.remote_file_add()`)
3. Remove files (`.dsl_ip_rm()` → `.remote_file_rm()`)
4. Manage empty remotes (create/remove placeholders)
5. Upload metadata (`.dsl_ip_log()` → `.remote_write_manifest()`, `.remote_write_version_file()`)

### Upload Strategies

**upload-all**
- Uploads all tracked files
- Ignores remote state
- Updates manifest/VERSION

**upload-missing**
- Only uploads files not present remotely
- Preserves existing remote files
- Updates manifest with new files

**sync-diff**
- Uploads new/modified files
- Removes deleted files
- Most efficient for incremental updates

**sync-purge**
- Removes all existing files
- Uploads all current files
- Guarantees exact mirror of local state

### Inspection Modes

**manifest**
- Uses manifest.csv to track versions
- Fast, no remote hashing required
- Requires trusted previous upload

**file**
- Hashes remote files directly
- Always trustworthy
- Slower for large remotes

**none**
- No change detection
- Treats remote as empty
- Forces full upload

---

## Remote Interface Requirements

Each remote type (local, github, osf) must implement these dispatcher functions from `R/remote.R`:

### Required Core Functions

**Existence checks:**
- `.remote_check_exists_{type}(...)` - Check if remote exists
- `.remote_final_check_exists_{type}(...)` - Check if final destination exists
- `.remote_final_check_exists_direct_{type}(...)` - Direct check with handle

**Creation/deletion:**
- `.remote_create_{type}(...)` - Create new remote container
- `.remote_rm_{type}(...)` - Delete remote container
- `.remote_final_rm_{type}(...)` - Delete final destination
- `.remote_final_rm_if_empty_{type}(...)` - Remove if empty
- `.remote_final_empty_{type}(...)` - Remove all contents
- `.remote_final_empty_get_{type}(...)` - Create empty placeholder

**Remote handles:**
- `.remote_get_{type}(id)` - Get remote handle from id
- `.remote_final_get_{type}(...)` - Compose final destination handle
- `.remote_final_get_if_exists_{type}(...)` - Get handle only if exists (optional)

**Listing:**
- `.remote_ls_final_{type}(remote_pre)` - List final destinations under parent
- `.remote_file_ls_{type}(remote)` - List files in destination

**File operations:**
- `.remote_file_add_{type}(fn, path_dir_local, remote, ...)` - Upload files
- `.remote_file_get_{type}(remote, fn, path_dir_save_local, ...)` - Download single file
- `.remote_file_get_all_{type}(remote, path_dir_save_local, ...)` - Download all files
- `.remote_file_rm_{type}(fn, remote, ...)` - Remove files

**Information retrieval:**
- `.remote_final_get_info_{type}(remote_final)` - Get metadata (optional, can return NULL)

### Remote Handle Structure

Each type uses a specific handle format:

**local**
- Handles are file paths (character strings)
- Example: `"/path/to/dest/output/v0.0.1"`

**github**
- Handles are named character vectors: `c("tag" = "...", "fn" = "...")`
- `remote_pre`: `c("tag" = "v0.0.1")`
- `remote_final`: `c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")`

**osf**
- Handles are `osf_tbl` objects from the `osfr` package
- `remote_pre`: `osf_tbl_node` object
- `remote_final`: `osf_tbl_file` object

### Dispatcher Pattern

Dispatchers in `R/remote.R` use this pattern:

```r
.remote_{operation} <- function(type, ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_{operation}_local(...),
    "github" = .remote_{operation}_github(...),
    "osf" = .remote_{operation}_osf(...)
  )
}
```

Type-specific implementations:
- Live in `R/remote-{type}.R`
- Don't take `type` argument
- Return consistent data structures
- Follow naming: `.remote_{operation}_{type}(...)`

### Adding a New Remote Type

1. Create `R/remote-{type}.R` with all required functions
2. Add dispatcher cases to all functions in `R/remote.R`
3. Add type to `.opt_remote_get_type()` helper
4. Implement remote handle composition in `.remote_get_path_rel()` if needed
5. Add authentication checks if required
6. Write tests in `tests/testthat/test-remote-{type}.R`
7. Document in this instructions file

---

## Key Rules

- Use httr, not piggyback for GitHub operations (migration complete)
- Always add auth checks before API calls
- Use polling for async GitHub operations
- Add retry logic for network operations
- Type-specific functions don't take `type` argument
- Test with proper skip conditions
- Use `remote_pre` and `remote_final` terminology consistently
- All dispatcher functions must exist for each remote type
