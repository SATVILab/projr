---
applyTo: "R/{git,yml-git}*.R"
---

# Git and Version Control Guidelines for projr

## Purpose & Scope

Guidelines for Git integration in projr, including repository operations, configuration, and YAML settings.

---

## Git System Selection

The package automatically selects between Git CLI and `gert` R package:

- `.git_system_get()` - Returns `"git"` if Git CLI available, otherwise `"gert"`
- `.git_system_check_git()` - Checks if Git CLI is available
- `.git_system_check_gert()` - Checks if `gert` package is available
- `.git_system_setup()` - Installs `gert` if Git CLI not available

---

## Repository Operations

### Initialization and Status

```r
.git_init()                      # Initialize Git repository
.git_repo_check_exists()         # Check if .git exists
.git_repo_is_worktree()          # Check if repo is a Git worktree
.git_repo_rm()                   # Remove .git directory
```

### File Operations

```r
.git_commit_file(file, msg)      # Commit specific file(s)
.git_commit_all(msg, add_untracked)  # Commit all modified/untracked files
.git_add_file_git(file)          # Stage files using Git CLI
```

### Status Queries

```r
.git_modified_get()              # Get modified files
.git_new_get()                   # Get new/untracked files
.git_untracked_not_ignored_get() # Get untracked files not ignored
.git_changed_filter(path)        # Filter paths to changed files only
```

### Branch and Commit Info

```r
.git_branch_get()                # Get current branch name
.git_last_commit_get()           # Get last commit info (sha and message)
.git_get_commit_hash_local()     # Get local commit hashes
.git_get_commit_hash_remote()    # Get remote commit hashes
```

### Remote Operations

```r
.git_remote_check_exists()       # Check if remote exists
.git_remote_check_upstream()     # Check if upstream configured
.git_push()                      # Push to remote
.git_fetch()                     # Fetch from remote
.git_check_behind()              # Check if local is behind remote
.git_clone(repo, path)           # Clone a repository
```

### Configuration

```r
.git_config_get_name()           # Get user.name (local, global, system)
.git_config_get_email()          # Get user.email (local, global, system)
```

---

## YAML Git Configuration

Functions in `R/yml-git.R` manage Git settings in `_projr.yml`.

### Settings

- `build.git.commit` - Auto-commit changes during builds (default: TRUE)
- `build.git.add-untracked` - Add untracked files during commits (default: TRUE)
- `build.git.push` - Auto-push after commits (default: TRUE)

### Exported Functions

```r
# Set Git options
projr_yml_git_set(all, commit, add_untracked, push, ...)

# Set all to TRUE (default)
projr_yml_git_set_default(profile)
```

### Internal Functions

```r
.yml_git_get(profile)                  # Get Git configuration
.yml_git_get_commit(profile)           # Get commit setting
.yml_git_get_push(profile)             # Get push setting
.yml_git_get_add_untracked(profile)    # Get add_untracked setting
.yml_git_set(yml_git, profile)         # Write Git configuration
```

### Simplification

- If all settings are identical: Can simplify to `git: TRUE` or `git: FALSE`
- If all settings are default (TRUE): Entire section is omitted
- `simplify_identical` parameter controls simplification when all match
- `simplify_default` parameter controls removal when all are default

---

## Code Examples

```r
# Correct: Initialize git with config
.git_init()
.test_setup_project_git_config()  # In tests only
writeLines("test", "test.txt")
.git_commit_file("test.txt", "commit message")

# Correct: Set Git options via YAML
projr_yml_git_set(commit = TRUE, push = TRUE, add_untracked = TRUE)

# Correct: Check if remote exists before operations
if (.git_remote_check_exists()) {
  .git_push()
}

# Incorrect: Not checking for warnings in tests
.git_commit_file(deleted_files, "message")  # May produce warnings

# Correct: Suppress expected warnings in tests
suppressWarnings(.git_commit_file(deleted_files, "message"))
```

---

## Testing Patterns

```r
test_that("git function works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git
      .git_init()
      .test_setup_project_git_config()
      
      # Test operations
      writeLines("test", "test.txt")
      .git_commit_file("test.txt", "commit message")
      
      # Verify
      expect_true(.git_repo_check_exists())
    }
  )
})
```

---

## Important Testing Notes

- Use `.test_setup_project_git_config()` to set test user credentials
- Git operations using `_git` variants may produce warnings for deleted files - use `suppressWarnings()`
- Remote operations require GitHub authentication - use `skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))`
- `.git_changed_filter()` returns `fs_path` class, not plain character - use `as.character()` or `expect_length()`

---

## Version Management Integration

The package includes comprehensive version management:

### Version Format

- Defined in `_projr.yml` under `metadata.version-format`
- Default: `"major.minor.patch-dev"`
- Valid formats: `major.minor.patch-dev`, `major.minor.patch.dev`, etc.
- Can use numeric suffixes like `9000` or `1` instead of `dev`

### Key Functions

```r
projr_version_get()                    # Get current project version
projr_version_set(version, only_if_exists)  # Set project version
.version_check(version)                # Validate version format
.version_concat(version_vec, split_vec)  # Concatenate version components
.version_get_earliest(x)               # Find earliest version
.version_get_latest(x)                 # Find latest version
```

### Input Validation

- All version helper functions validate inputs using `.assert_*()` functions
- `.version_v_rm()` and `.version_v_add()` require non-empty single strings
- `.version_get_earliest()` and `.version_get_latest()` require non-empty character vectors
- `.version_concat()` accepts numeric or character vectors
- `.version_current_vec_get_init_file()` validates VERSION file exists and has valid content

---

## Git Clone and Authentication

The `.git_clone()` function requires GitHub authentication when inferring username:

```r
# Before cloning with username inference
.auth_check_github("cloning repository")
user <- gh::gh_whoami()$login
.git_clone(repo, path)
```

See `authentication.instructions.md` for authentication guidelines.
