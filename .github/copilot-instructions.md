# Copilot Instructions for projr

This is an R package that facilitates reproducible and archived projects. The package helps users manage project structure, build processes, versioning, and deployment.

## Code Standards

### Required Before Each Commit
- Run `devtools::document()` to update roxygen2 documentation in `man/`
- Run `devtools::test()` to ensure all tests pass
- Run `devtools::check()` to ensure package passes R CMD check
- Ensure code follows the existing style conventions (see below)
- **Update copilot instructions** (`.github/copilot-instructions.md`) when making changes that affect development workflow, code standards, or best practices
- **Update pkgdown website** (rebuild with `pkgdown::build_site()`) when making changes to package documentation, vignettes, or exported functions

### Development Flow
- **Install dependencies**: `renv::restore()`
- **Load package**: `devtools::load_all()`
- **Document**: `devtools::document()` (updates `man/` and `NAMESPACE`)
- **Test**: `devtools::test()` - **Use LITE mode by default for faster iteration** (see Testing Guidelines below)
- **Check**: `devtools::check()`
- **Build**: `devtools::build()`

### Testing Guidelines for Development

**When running tests during development, use LITE mode unless otherwise specified:**

```r
# Enable LITE test mode for faster testing (recommended default)
devtools::load_all()
.test_set_lite()
devtools::test()
```

**Use FULL mode (all tests) only when:**
- Preparing for a release or major version bump
- Explicitly requested by the issue or PR
- Working on comprehensive parameter combination testing

**When working on specific functionality, run all tests for that functionality:**
```r
# Example: Testing specific file with all its tests (including comprehensive)
devtools::load_all()
# Don't set .test_set_lite() to run all tests
devtools::test()  # or testthat::test_file("tests/testthat/test-manifest.R")
```

**Test mode selection:**
- **LITE mode** (default): Skips comprehensive tests, faster for development iteration (~364 tests, ~2.5 minutes)
- **FULL mode**: Runs all tests including comprehensive parameter combinations (~452 tests, ~5+ minutes)

### Testing with GitHub Actions
The repository uses GitHub Actions for CI/CD:
- R CMD check runs on push/PR
- Test coverage is tracked with codecov
- The workflow is defined in `.github/workflows/R-CMD-check.yaml`

## Repository Structure

### Core Directories
- `R/`: R source code - all package functions (70+ files)
- `tests/testthat/`: Unit tests using testthat 3e
  - Test files named `test-*.R` corresponding to source files
  - Test helper files: `helper-setup.R`, `helper-github.R`, `helper-osf.R`, `helper-debug.R`
  - Manual tests in `tests/testthat/manual/`
  - Project structure templates in `tests/testthat/project_structure/`
- `man/`: Auto-generated documentation (DO NOT edit directly - use roxygen2)
- `vignettes/`: Package vignettes (R Markdown format)
- `inst/`: Package installation files
  - `inst/CITATION`: Citation information
  - `inst/project_structure/`: Project structure templates (used by initialization)
- `renv/`: renv package management
- `.github/workflows/`: GitHub Actions workflows

### Configuration Files
- `DESCRIPTION`: Package metadata, dependencies, and configuration
- `NAMESPACE`: Auto-generated exports (managed by roxygen2)
- `_pkgdown.yml`: pkgdown website configuration
- `codecov.yml`: Code coverage configuration
- `renv.lock`: Locked package versions
- `.Rbuildignore`: Files to exclude from R package build

## Key Guidelines

### 1. Coding Style
- Use `.` prefix for internal (non-exported) functions (e.g., `.build_manifest_pre()`)
- Use `snake_case` for function and variable names
- Exported functions use the pattern `projr_*` (e.g., `projr_init_prompt()`, `projr_path_get_dir()`)
- Use the native pipe `|>` for function composition
- Keep functions focused and modular
- Use descriptive variable names (e.g., `label_vec`, `path_manifest`, `dir_test`)

### 2. Documentation (roxygen2)
All exported functions **must** include:
- `#' @title`: Short one-line title
- `#' @description`: Detailed description
- `#' @param`: Document all parameters with type and description
- `#' @return`: Describe what the function returns (use `invisible(...)` when appropriate)
- `#' @export`: For exported functions only
- `#' @examples`: Provide working examples (wrap in `\dontrun{}` if needed)
- `#' @seealso`: Link to related functions when appropriate

**IMPORTANT**: After updating roxygen2 documentation comments in any `R/` file, you **must** run `devtools::document()` to regenerate the corresponding `.Rd` files in `man/`. The `man/` directory is auto-generated and should never be edited directly.

Example from the codebase:
```r
#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param yml_path_from character.
#' Path to YAML file to use as `_projr.yml`.
#' If not supplied, then default `_projr.yml` file is used.
#'
#' @param renv_force Logical.
#' Passed to `renv::init()`.
#' Default is \code{FALSE}.
#'
#' @export
projr_init_prompt <- function(yml_path_from = NULL, 
                               renv_force = FALSE, 
                               renv_bioconductor = TRUE,
                               public = FALSE) {
  # implementation
}
```

Internal functions (starting with `.`) should NOT have `@export` tags.

### 3. Testing with testthat

The package has a tiered test suite to accommodate different testing needs:

#### Test Suite Levels

**CRAN Mode** (`R_PKG_TEST_CRAN=TRUE` or `NOT_CRAN=false`):
- Runs only fast, essential tests (~364 tests)
- Skips comprehensive tests (exhaustive parameter combinations)
- Skips integration tests
- Skips remote-dependent tests (GitHub/OSF)
- Target: Complete in <2 minutes for CRAN submission
- Auto-activates when `NOT_CRAN` is false/unset
- Use `skip_if(.is_test_cran())` to skip tests in CRAN mode

**Lite Mode** (`R_PKG_TEST_LITE=TRUE`) - **RECOMMENDED FOR DEVELOPMENT**:
- Runs core functionality tests (~364 tests, ~2.5 minutes)
- Skips comprehensive tests (exhaustive parameter combinations)
- Includes integration tests
- Includes remote tests if credentials available
- **Use this mode by default when running tests during development** unless:
  - Preparing for a release (use Full mode)
  - Explicitly working on comprehensive parameter testing (use Full mode for that specific file)
  - Requested otherwise in the issue/PR
- Enable with: `devtools::load_all(); .test_set_lite(); devtools::test()`
- Use `skip_if(.is_test_lite())` to skip tests in lite mode
- Note: `R_PKG_TEST_DEBUG` is deprecated but still works for backward compatibility

**Full Mode** (default when no test mode is set):
- Runs all tests (452 tests, ~5+ minutes)
- Includes comprehensive tests
- Includes integration tests
- Includes remote tests if credentials available
- **Use for:**
  - Pre-release validation
  - When working on comprehensive parameter combination testing
  - When explicitly requested in the issue/PR
- Enable by running tests without calling `.test_set_lite()` or `.test_set_cran()`

#### Test Guidelines

- Use testthat 3e (Config/testthat/edition: 3)
- Test file naming: `test-{feature}.R` (e.g., `test-manifest.R` for manifest-related functions)
- Use `test_that()` for each test case with descriptive names
- Use `skip_if(.is_test_select())` for tests that should be skipped in certain conditions
- Use `skip_if(.is_test_cran())` for comprehensive/integration/remote tests
- Use `skip_if(.is_test_lite())` for comprehensive tests (use `.is_test_debug()` for backward compatibility)
- Use `usethis::with_project()` for tests that need a temporary project environment
- **Test helpers** (located in `tests/testthat/helper-setup.R`):
  - `.test_setup_project()` - Creates a complete test project with git, files, and configuration
  - `.init()` - Minimal initialization for tests (creates directories, VERSION, _projr.yml)
  - `.init_full()` - Full initialization for tests (same as `.init()` currently)
  - `.test_setup_content()` - Creates test content in project directories
  - `.test_setup_project_lit_docs()` - Sets up document engine files (quarto, rmarkdown)
- **IMPORTANT**: Test helper functions should be added to `tests/testthat/helper-*.R` files, NOT to `R/` files
- Common expect functions:
  - `expect_identical()` for exact matches
  - `expect_true()` / `expect_false()` for logical values
  - `expect_error()` for error conditions
  - Test edge cases: empty directories, missing files, NULL values

#### When Adding New Tests

- **Comprehensive tests** (exhaustive parameter combinations):
  - Add to files named `test-*-comprehensive.R`
  - Add `skip_if(.is_test_cran())` AND `skip_if(.is_test_lite())`
  - These test all combinations of YML parameters
  
- **Integration tests** (multi-component workflows):
  - Add to files named `test-*-integration.R`
  - Add `skip_if(.is_test_cran())` only
  - Run in lite mode to catch integration issues
  
- **Regular tests** (core functionality):
  - No special skip conditions needed
  - Should run in all modes
  - Focus on essential functionality

- **Remote-dependent tests** (GitHub/OSF):
  - Add `skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))` or equivalent
  - Add `skip_if(.is_test_cran())` for CRAN compatibility
  - May run in lite mode if credentials available

#### Testing Local Remotes Comprehensively

When testing local remote functionality (see `test-local-remote-comprehensive.R`), test all combinations of YML parameters:

**Structure Options**:
- `structure = "latest"` - Overwrites files at destination
- `structure = "archive"` - Creates versioned subdirectories (v0.0.1, v0.0.2, etc.)

**Send Cue Options**:
- `send_cue = "always"` - Always creates new remote version, even without changes
- `send_cue = "if-change"` - Only creates new version when content changes
- `send_cue = "never"` - Never sends to remote (NOTE: implementation may need updates)

**Send Strategy Options**:
- `send_strategy = "sync-diff"` - Syncs only changed files (adds new, removes deleted, updates modified)
- `send_strategy = "sync-purge"` - Removes all files then uploads all local files
- `send_strategy = "upload-all"` - Uploads all files (may overwrite)
- `send_strategy = "upload-missing"` - Only uploads files not present on remote

**Send Inspect Options**:
- `send_inspect = "manifest"` - Uses manifest.csv to track versions (recommended)
- `send_inspect = "file"` - Inspects actual files on remote
- `send_inspect = "none"` - Treats remote as empty (NOTE: may have implementation issues)

**Content Types**:
Test with different content types: `raw-data`, `output`, `cache`, `docs`, `code`

**Test Pattern Example**:
```r
test_that("local remote with archive + sync-diff + if-change", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create test content
      .create_test_content("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")
      
      # Add local destination with specific parameters
      projr_yml_dest_add_local(
        title = "test-dest",
        content = "raw-data",
        path = "_archive",
        structure = "archive",
        send_strategy = "sync-diff",
        send_cue = "if-change",
        send_inspect = "manifest"
      )
      
      # Test builds and verify behavior
      projr::projr_build_patch()
      expect_true(dir.exists("_archive/raw-data/v0.0.1"))
    }
  )
})
```

Example test pattern from the codebase:
```r
test_that(".build_manifest_* works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # test setup
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 0L)
      
      # test with content
      label_vec <- c("cache", "raw-data")
      invisible(.test_setup_content(label_vec, safe = TRUE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 3L)
    }
  )
})
```

### 4. Dependencies
- **Core imports** (always available): renv, jsonlite, yaml, rprojroot, desc, fs, digest
- **Suggests** (optional): testthat, devtools, usethis, gert, gh, quarto, knitr, etc.
- When adding dependencies:
  - Add to `DESCRIPTION` under `Imports:` or `Suggests:`
  - Use `package::function()` notation for suggested packages
  - Update `renv.lock` with `renv::snapshot()`

### 5. Common Patterns in the Codebase

#### Conditional execution with output_run
Many functions have an `output_run` parameter:
```r
.build_manifest_pre <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  # actual implementation
}
```

#### Path handling
- Use `.path_get()` for project-relative paths
- Use `.dir_get_cache_auto_version()` for cache directories
- Use `projr_path_get_dir()` for directory paths

**Input Validation for Path Functions**:
Following the patterns established for version, profile, and renv functions, all path manipulation functions include comprehensive input validation:

- **Type validation**: Use `.assert_chr()` or `.assert_chr_min()` for character inputs, `.assert_string()` for single strings, `.assert_flag()` for logical flags
- **Empty vector handling**: Most filter and transformation functions accept empty character vectors (using `.assert_chr_min()`) and return appropriate empty results
- **NULL handling**: Functions that accept NULL parameters (like `path_dir` in `.path_force_rel()`) validate only when not NULL
- **Required parameters**: Use `required = TRUE` in assertion functions for mandatory parameters
- **Early validation**: Validate inputs at the start of functions before any processing

**Common validation patterns**:
```r
# Filter functions - allow empty vectors
.path_filter_spec <- function(fn, exc = NULL) {
  .assert_chr_min(fn, required = TRUE)  # Allows empty vectors
  if (is.null(exc)) return(fn)
  .assert_chr(exc, required = TRUE)     # Non-empty when provided
  # ... processing
}

# Path transformation - handle empty vectors
.path_force_rel <- function(path, path_dir = NULL) {
  .assert_chr_min(path, required = TRUE)
  .assert_string(path_dir)  # Optional, validates if not NULL
  if (length(path) == 0) return(character(0))
  # ... processing
}

# Directory operations - strict validation
.dir_ls <- function(path_dir, recursive = TRUE, full.names = FALSE) {
  .assert_string(path_dir, TRUE)        # Must be single non-empty string
  .assert_dir_exists(path_dir, TRUE)    # Must exist
  .assert_flag(recursive, TRUE)         # Must be TRUE/FALSE
  .assert_flag(full.names, TRUE)
  # ... processing
}
```

**Test coverage**: Path validation tests are in `tests/testthat/test-path-validation.R` covering:
- Edge cases (empty vectors, NULL, NA)
- Invalid input types (numeric, list, wrong length)
- Logical flag validation
- Special characters in paths
- Return type consistency

#### Data structures
- Use tibbles/data.frames for tabular data (manifests, etc.)
- Return empty tables with `.zero_tbl_get_manifest()` when appropriate
- Use lists for configurations (e.g., `nm_list`)

#### Error handling
- Use `stop()` for errors in internal functions
- Provide informative error messages
- Check for file/directory existence before operations

### 6. YAML Configuration
The package heavily uses YAML configuration (`_projr.yml`):
- Configuration is read and managed through `yml-*.R` files
- Functions like `.yml_dir_get_label_*()` retrieve configuration values
- Use existing YAML helper functions rather than reading files directly

#### YAML Validation with projr_yml_check

The `projr_yml_check()` function validates the entire `_projr.yml` configuration. It checks:

**Core Structure** (always checked):
- **Directories** (`.yml_dir_check`): Validates directory labels, paths, ignore settings
- **Build settings** (`.yml_build_check`): Validates git, dest, and label configurations
- **Dev settings** (`.yml_dev_check`): Validates dev.scripts and dev.hooks keys

**Additional Validations** (added for comprehensive checking):
- **Metadata** (`.yml_metadata_check`): Validates metadata.version-format
- **Scripts** (`.yml_scripts_check`): Validates build.scripts and dev.scripts are character vectors
- **Hooks** (`.yml_hooks_check_config`): Validates build.hooks and dev.hooks structure (pre/post/both stages)
- **Cite** (`.yml_cite_check_config`): Validates build.cite is either logical or list with valid keys

**Valid build.* keys**:
- "dev-output", "script", "hooks", "scripts", "git", "github", "package", "local", "osf", "cite"

**Testing validation**:
When adding new yml configuration options:
1. Add validation logic to appropriate check function in `R/yml-check.R`
2. Add the check function to `projr_yml_check()` if it's a new top-level category
3. Write tests in `tests/testthat/test-yml-check.R` covering valid and invalid configurations

### 7. Version Control and Git

The package includes comprehensive Git integration (see `R/git.R` and `R/yml-git.R`) that works with both Git CLI and the `gert` R package.

#### Git System Selection

The package automatically selects between Git CLI and `gert`:
- `.git_system_get()` - Returns `"git"` if Git CLI is available, otherwise `"gert"`
- `.git_system_check_git()` - Checks if Git CLI is available
- `.git_system_check_gert()` - Checks if `gert` package is available
- `.git_system_setup()` - Installs `gert` if Git CLI is not available

#### Repository Operations

**Initialization and Status**:
- `.git_init()` - Initialize a Git repository (uses Git CLI or gert)
- `.git_repo_check_exists()` - Check if `.git` exists (file or directory)
- `.git_repo_is_worktree()` - Check if the repo is a Git worktree
- `.git_repo_rm()` - Remove `.git` directory

**File Operations**:
- `.git_commit_file(file, msg)` - Commit specific file(s)
- `.git_commit_file_git(file, msg)` - Commit using Git CLI
- `.git_commit_file_gert(file, msg)` - Commit using gert
- `.git_add_file_git(file)` - Stage files using Git CLI
- `.git_commit_all(msg, add_untracked)` - Commit all modified and optionally untracked files

**Status Queries**:
- `.git_modified_get()` - Get modified files
- `.git_new_get()` - Get new/untracked files
- `.git_untracked_not_ignored_get()` - Get untracked files that aren't ignored
- `.git_changed_filter(path)` - Filter paths to only those that are changed (modified or new)

**Branch and Commit Info**:
- `.git_branch_get()` - Get current branch name
- `.git_last_commit_get()` - Get last commit info (sha and message)
- `.git_get_commit_hash_local()` - Get local commit hashes
- `.git_get_commit_hash_remote()` - Get remote commit hashes

**Remote Operations**:
- `.git_remote_check_exists()` - Check if remote exists
- `.git_remote_check_upstream()` - Check if upstream remote is configured
- `.git_push()` - Push to remote
- `.git_fetch()` - Fetch from remote
- `.git_check_behind()` - Check if local is behind remote
- `.git_clone(repo, path)` - Clone a repository (requires GitHub auth if inferring username)

**Configuration**:
- `.git_config_get_name()` - Get user.name (checks local, global, system)
- `.git_config_get_email()` - Get user.email (checks local, global, system)
- Each has `_git` and `_gert` variants, plus `_local`, `_global`, `_system` variants

#### YAML Git Configuration

Functions in `R/yml-git.R` manage Git settings in `_projr.yml`:

**Exported Functions**:
- `projr_yml_git_set(all, commit, add_untracked, push, ...)` - Set Git options
- `projr_yml_git_set_default(profile)` - Set all options to TRUE (default)

**Internal Functions**:
- `.yml_git_get(profile)` - Get Git configuration from YAML
- `.yml_git_get_commit(profile)` - Get commit setting (default: TRUE)
- `.yml_git_get_push(profile)` - Get push setting (default: TRUE)
- `.yml_git_get_add_untracked(profile)` - Get add_untracked setting (default: TRUE)
- `.yml_git_set(yml_git, profile)` - Write Git configuration to YAML

**Settings**:
- `build.git.commit` - Automatically commit changes during builds (default: TRUE)
- `build.git.add-untracked` - Add untracked files during commits (default: TRUE)
- `build.git.push` - Automatically push after commits (default: TRUE)

**Simplification**:
- If all settings are identical, can be simplified to `git: TRUE` or `git: FALSE`
- If all settings are default (TRUE), the entire section is omitted
- `simplify_identical` parameter controls whether to simplify when all match
- `simplify_default` parameter controls whether to remove when all are default

#### Testing Patterns

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

**Important Notes**:
- Use `.test_setup_project_git_config()` to set test user credentials
- Git operations using `_git` variants may produce warnings for deleted files - use `suppressWarnings()` where appropriate
- Remote operations require GitHub authentication - use `skip_if_not(nzchar(Sys.getenv("GITHUB_PAT")))`
- `.git_changed_filter()` returns `fs_path` class, not plain character - use `as.character()` or `expect_length()` in tests

### 8. Version Functions

The package includes comprehensive version management functions in `R/version.R` and `R/yml-version.R`.

#### Version Function Guidelines

**Input Validation**:
- All version helper functions validate their inputs using `.assert_*()` functions
- `.version_v_rm()` and `.version_v_add()` require non-empty single strings
- `.version_get_earliest()` and `.version_get_latest()` require non-empty character vectors
- `.version_concat()` accepts numeric or character vectors, automatically converts numeric to character
- `.version_current_vec_get_init_file()` validates VERSION file exists, is not empty, and contains valid content (trims whitespace)

**Version Format**:
- Version format is defined in `_projr.yml` under `metadata.version-format`
- Default format: `"major.minor.patch-dev"`
- Valid formats include: `major.minor.patch-dev`, `major.minor.patch.dev`, `major.minor-dev`, `major.minor.dev`, `major-dev`, `major.dev`
- Format can also use numeric suffixes like `9000` or `1` instead of `dev`

**Key Functions**:
- `projr_version_get()`: Returns current project version (exported)
- `projr_version_set(version, only_if_exists)`: Sets project version (exported)
- `.version_check(version)`: Validates version format against `_projr.yml` configuration
- `.version_check_error_free(version)`: Returns TRUE if valid, FALSE if invalid (safe validation)
- `.version_concat(version_vec, split_vec)`: Concatenates version components with separators
- `.version_get_earliest(x)` / `.version_get_latest(x)`: Find earliest/latest version from vector

**Testing Edge Cases**:
- Test empty/NULL inputs
- Test whitespace-only VERSION files
- Test numeric vs. character input handling
- Test version format validation
- Test version bumping logic
- See `tests/testthat/test-version-validation.R` for comprehensive examples

### 9. Build Scripts and Hooks Configuration

The package supports explicit specification of which files to build and hooks that run before/after the build process.

#### Build Scripts (`build.scripts` and `dev.scripts`)

**Purpose**: Explicitly specify which documents/scripts to build instead of relying on automatic detection or `_quarto.yml`/`_bookdown.yml`.

**YAML Structure**:
```yaml
# Production build scripts
build:
  scripts:
    - analysis.qmd
    - report.Rmd
    - data-processing.R

# Development build scripts (exclusive override)
dev:
  scripts:
    - quick-test.qmd
    - debug.R
```

**Key Points**:
- `build.scripts` only accepts plain character vectors (no sub-keys or named lists)
- `dev.scripts` provides exclusive control for dev builds (no fallback to `build.scripts`)
- Scripts specified in `_projr.yml` override `_quarto.yml` and `_bookdown.yml` configurations
- Priority order for dev builds: file param → dev.scripts → _quarto/_bookdown → auto-detect
- Priority order for production builds: file param → build.scripts → _quarto/_bookdown → auto-detect

**API Functions**:
- `.yml_scripts_get_build(profile)` - Get build scripts
- `.yml_scripts_get_dev(profile)` - Get dev scripts (exclusive, no fallback)
- `.yml_build_get_scripts(profile)` - Get build.scripts configuration

#### Build Hooks (`build.hooks` and `dev.hooks`)

**Purpose**: Run custom scripts before (pre) or after (post) the build process.

**YAML Structure**:
```yaml
build:
  hooks:
    pre:
      - setup.R
      - download-data.R
    post:
      - cleanup.R
      - send-notifications.R
    both:
      - log-timestamp.R

dev:
  hooks:
    pre: dev-setup.R
    both: dev-logger.R
```

**Key Points**:
- Hooks are stored as **simple character vectors** (no titles, no "path" keys)
- Three stage keys: `pre` (before build), `post` (after build), `both` (both stages)
- `build.hooks` are **always ignored** in dev runs
- `dev.hooks` are **always ignored** in production runs
- Hooks run in the order specified in `_projr.yml`
- Hooks are NOT run in the same environment as the build process

**Hook Execution Timing**:
- **Pre-build hooks**: Run after version bump, before Git commit
- **Post-build hooks**: Run after Git commit, before distributing artifacts

**API Structure**:
- **Exported functions**: `projr_yml_hooks_add()`, `projr_yml_hooks_add_pre()`, `projr_yml_hooks_add_post()`, `projr_yml_hooks_rm_all()`
- **Internal functions**: `.yml_hooks_add()`, `.yml_hooks_get()`, `.yml_hooks_get_stage()`, `.yml_hooks_set()`, `.yml_hooks_rm_all()`
- **Build execution**: `.build_hooks_run()` in `R/build-hooks.R`

**API Usage**:
```r
# Add hooks (simplified API - no title or cue parameters)
projr_yml_hooks_add(path = "setup.R", stage = "pre")
projr_yml_hooks_add(path = c("log1.R", "log2.R"), stage = "both")

# Convenience wrappers
projr_yml_hooks_add_pre(path = "pre-hook.R")
projr_yml_hooks_add_post(path = "post-hook.R")
```

#### File Existence Validation

Before any build starts, all scripts and hooks are validated:
- Build scripts from `build.scripts`
- Dev scripts from `dev.scripts`
- Build hooks from `build.hooks` (pre, post, both)
- Dev hooks from `dev.hooks` (pre, post, both)

Function: `.yml_scripts_hooks_check_exist(is_dev_build)` in `R/yml-check.R`

#### Testing Pattern
```r
test_that("build.scripts works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Write scripts to YAML
      yaml::write_yaml(
        list(build = list(scripts = c("file1.qmd", "file2.R"))),
        "_projr.yml"
      )
      
      # Read and verify
      scripts <- .yml_scripts_get_build("default")
      expect_identical(scripts, c("file1.qmd", "file2.R"))
    }
  )
})

test_that("build.hooks works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Write hooks to YAML (simple vectors)
      yaml::write_yaml(
        list(build = list(hooks = list(
          pre = c("pre1.R", "pre2.R"),
          post = "post.R",
          both = "both.R"
        ))),
        "_projr.yml"
      )
      
      # Read and verify
      pre_hooks <- .yml_hooks_get_stage("pre", "default")
      expect_true("pre1.R" %in% pre_hooks)
      expect_true("both.R" %in% pre_hooks) # both runs in pre too
    }
  )
})
```

#### Common Pitfalls
- Don't add sub-keys under `build.scripts` - it only accepts plain vectors
- Don't create nested structures for hooks - they're simple character vectors
- Remember `dev.scripts` is exclusive (no fallback to `build.scripts`)
- Remember `build.hooks` are ignored in dev runs; use `dev.hooks` for dev-specific hooks
- File paths in scripts/hooks are relative to project root

## Initialization Functions

The package has two main initialization paths:

### Production Initialization (R/init-std.R)
- `projr_init()` - Main initialization function with granular control
- `projr_init_all()` - Convenience wrapper that enables all features
- `.init_*_std()` - Internal functions for each initialization step
- Used by end users to set up new projects

### Test Initialization (tests/testthat/helper-setup.R)
- `.test_setup_project()` - Creates a complete test project (copies from `tests/testthat/project_structure/`)
- `.init()` - Minimal initialization for tests (directories + VERSION + _projr.yml)
- `.init_full()` - Full initialization for tests
- `.test_setup_project_lit_docs()` - Sets up document engine files
- **IMPORTANT**: Test helper functions belong in `tests/testthat/helper-*.R`, NOT in `R/` files

### Document Engine Initialization
Each engine type has initialization functions:
- **Bookdown**: Creates `_bookdown.yml`, `_output.yml`, and `index.Rmd`
- **Quarto Project**: Creates `_quarto.yml` and `index.qmd`
- **Quarto Document**: Creates a standalone `.qmd` file
- **RMarkdown**: Creates a standalone `.Rmd` file

Templates are stored in `inst/project_structure/` for production and `tests/testthat/project_structure/` for tests.

## File Organization Patterns

Functions are organized by feature/domain:
- `build*.R`: Build process functions
- `init*.R`: Initialization functions
- `yml*.R`: YAML configuration handling
- `dest*.R`: Destination/deployment functions
- `git*.R`: Git integration
- `manifest*.R`: Manifest management
- `auth.R`: Authentication helpers

## When Making Changes

1. **New functions**: 
   - Add to appropriate `R/*.R` file or create new file
   - Use `.` prefix for internal functions
   - Use `projr_` prefix for exported functions
   - Add roxygen2 documentation for exports
   - Add corresponding tests in `tests/testthat/test-*.R`

2. **Modifying existing functions**:
   - Maintain backward compatibility when possible
   - Update tests to cover new behavior
   - Update roxygen2 documentation if parameters/return values change
   - Run `devtools::document()` after roxygen2 changes

3. **Bug fixes**:
   - Add a test that reproduces the bug first
   - Fix the bug
   - Verify the test now passes
   - Ensure existing tests still pass

4. **Documentation changes**:
   - Vignettes: Edit `.Rmd` files in `vignettes/`
   - Function docs: Edit roxygen2 comments in `R/` files
   - README: Edit `README.md` directly
   - Run `devtools::document()` after roxygen2 changes
   - Update copilot instructions (`.github/copilot-instructions.md`) when changes affect development practices or workflow
   - Rebuild pkgdown website with `pkgdown::build_site()` when documentation changes are significant

## Build and Test Commands

```r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run tests for specific file
devtools::test_file("tests/testthat/test-manifest.R")

# Update documentation
devtools::document()

# Check package (comprehensive)
devtools::check()

# Install package locally
devtools::install()

# Build package
devtools::build()

# Update renv lockfile after adding dependencies
renv::snapshot()
```

## Common Issues and Solutions

1. **Test failures**: 
   - Check if tests need project setup: `usethis::with_project()`
   - Some tests may be skipped with `skip_if(.is_test_select())`

2. **Documentation not updating**:
   - Run `devtools::document()` to regenerate `man/` files
   - Check roxygen2 comments syntax

3. **Dependency issues**:
   - Run `renv::restore()` to sync with `renv.lock`
   - Check `DESCRIPTION` for package requirements

4. **Check failures**:
   - Review output of `devtools::check()`
   - Common issues: missing documentation, unused imports, test failures

## Additional Notes

- The package uses `renv` for reproducible dependency management
- The package has a pkgdown website (configured in `_pkgdown.yml`)
- Code coverage is tracked and should be maintained/improved
- The package integrates with multiple services: GitHub, OSF (Open Science Framework)
- Some functionality requires authentication (GitHub PAT, OSF token)
- The package supports multiple document engines (R Markdown, Quarto, Bookdown)

## Authentication System

The package includes comprehensive authentication checks for GitHub and OSF operations to ensure API calls fail gracefully with helpful error messages when credentials are missing.

### Authentication Functions

**Core Functions** (`R/auth.R`):
- `.auth_get_github_pat()` - Retrieves GitHub PAT from environment or gitcreds
- `.auth_get_osf_pat()` - Retrieves OSF PAT from environment
- `.auth_check_github(context)` - **Checks and throws error if GitHub auth missing**
- `.auth_check_osf(context)` - **Checks and throws error if OSF auth missing**

**Exported Functions**:
- `projr_instr_auth_github()` - Prints GitHub authentication instructions
- `projr_instr_auth_osf()` - Prints OSF authentication instructions

### Where Auth Checks Are Required

**GitHub Operations** - All functions that call `gh::` or `gitcreds::` must have `.auth_check_github()`:
- `.git_clone()` - When inferring username from `gh::gh_whoami()`
- `.init_github_impl()` - Before creating GitHub repository
- `.remote_host_rm_github()` - Before deleting GitHub repository
- `.pb_guess_repo()` - When using `gh::gh_tree_remote()`
- `.build_github_setup_user()` - Already protected via `.build_github_setup_check_pat()`

**OSF Operations** - All OSF wrapper functions have `.auth_check_osf()`:
- `.remote_create_osf()` - Creating OSF nodes
- `.remote_get_osf()` - Retrieving OSF nodes
- `.remote_host_rm_osf()` - Deleting OSF nodes
- `.osf_upload()`, `.osf_download()`, `.osf_ls_files()`, etc. - All OSF wrappers in `R/remote-osf.R`

### Authentication Check Pattern

When adding new functions that call GitHub or OSF APIs:

```r
# GitHub operations
.my_github_function <- function(...) {
  .auth_check_github("operation description")
  # Now safe to call gh:: functions
  user <- gh::gh_whoami()$login
  # ...
}

# OSF operations
.my_osf_function <- function(...) {
  .auth_check_osf("operation description")
  # Now safe to call osfr:: functions
  node <- osfr::osf_retrieve_node(id)
  # ...
}
```

### Testing Authentication

Tests should handle both authenticated and unauthenticated scenarios:

```r
test_that("function works with auth", {
  skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))
  # Test with credentials available
  expect_true(.my_github_function())
})

test_that("function fails gracefully without auth", {
  pat_old <- Sys.getenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_PAT")
  
  expect_error(.auth_check_github(), "GitHub authentication is required")
  
  if (nzchar(pat_old)) Sys.setenv("GITHUB_PAT" = pat_old)
})
```

### Build-Time Auth Checks

During builds, `.build_check_auth_remote()` is called via `.build_env_check()` to verify that required credentials are available for configured remote destinations.

## Build Logging System

The package includes a comprehensive build logging system that captures detailed information about each build:

### Log Directory Structure

Logs are stored in `cache/projr/log/` which is **never automatically cleared** by projr:

```
cache/projr/log/
├── output/                    # Production build logs
│   ├── history/
│   │   └── builds.md         # All build records, newest first
│   └── output/
│       └── YYYY-MMM-DD/      # Daily log folders
│           └── HH-MM-SS.qmd  # Detailed log for each build
└── dev/                       # Development build logs
    ├── history/
    │   └── builds.md
    └── output/
        └── YYYY-MMM-DD/
            └── HH-MM-SS.qmd
```

### Key Features

- **Automatic Logging**: All builds create log files automatically
- **Separate Dev/Output Logs**: Development and production builds logged separately
- **History Tracking**: `builds.md` tracks all builds (always maintained)
- **Detailed Logs**: Quarto-formatted `.qmd` files with full build output
- **Controllable**: Use `PROJR_OUTPUT_LEVEL` env var for console output level
- **Detailed Logging Control**: Use `PROJR_LOG_DETAILED` env var to disable detailed log files (history always maintained)

### Usage

```r
# Control console output level
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")  # or "std", "none"
projr_build_dev()

# Disable detailed log file creation (history still maintained)
Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
projr_build_dev()

# Clear logs
projr_log_clear()                          # Clear all logs
projr_log_clear(build_type = "dev")        # Clear dev logs only
projr_log_clear(history = FALSE)           # Keep history, clear output logs
projr_log_clear(before_date = "2025-01-01") # Clear logs before date
```

### Debug Output for Remote Operations

The build system includes detailed debug logging for remote operations (local, OSF, GitHub) that can be enabled for troubleshooting:

**Output Levels**:
- `"none"` - No additional messages (default for dev builds)
- `"std"` - Standard messaging (default for output builds)
- `"debug"` - Verbose messaging including remote operations details and change summaries

**Debug Messages for Remotes**:
When `PROJR_OUTPUT_LEVEL="debug"`, the following remote operations are logged:
- Remote creation: Type and ID
- File addition: Type, number of files, source path
- File removal: Type, number of files being removed
- File listing: Type, number of files found
- Destination processing: Remote configuration details, upload plans, file counts

**Example Debug Session**:
```r
# Enable debug output
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")

# Add local destination
projr_yml_dest_add_local(
  title = "archive",
  content = "raw-data",
  path = "_archive",
  structure = "archive",
  send_cue = "if-change",
  send_strategy = "sync-diff",
  send_inspect = "manifest"
)

# Build - will show detailed remote operations
projr_build_patch()

# Output will include:
# - "Content 'raw-data': Starting processing for destination 'archive' (type: local)"
# - "Content 'raw-data': Remote configuration - id: _archive, structure: archive, strategy: sync-diff, inspect: manifest"
# - "Content 'raw-data': Upload plan - X file(s) to add, Y file(s) to remove, create: TRUE/FALSE, purge: TRUE/FALSE"
# - "Remote file add: type=local, adding X file(s) from /path/to/source"
```

### Build Change Summary

The build system automatically tracks and reports changes in input and output files between builds:

**BUILDLOG.md Integration**:
- Change summaries are automatically added to `BUILDLOG.md` for each production build
- Compares current build with previous version using manifest hashes
- Tracks changes in both input directories (raw-data, cache) and output directories (output, docs)
- Shows added, removed, modified, and unchanged file counts

**Debug Console Output**:
When `PROJR_OUTPUT_LEVEL="debug"`, change summaries are displayed in the console during the build

**Change Summary Format**:
- If total changes < 10: Shows individual file names for added, removed, and modified files
- If total changes ≥ 10: Shows only counts to avoid cluttering the log
- Organized by section: "Inputs Changes" and "Outputs Changes"
- Displays version comparison (e.g., "v0.0.1 → v0.0.2")

**Example Change Summary in BUILDLOG.md**:
```markdown
**Inputs Changes (v0.0.1 → v0.0.2)**

- No changes detected in inputs

**Outputs Changes (v0.0.1 → v0.0.2)**

- `output`:
  - 1 added, 1 modified, 2 unchanged
  - Added: newfile.txt
  - Modified: report.html
```

**Implementation**:
- Core functions in `R/build-change-summary.R`
- `.build_change_summary_get()` - Generates change summary by comparing manifests
- `.build_change_summary_display()` - Displays changes at debug level
- `.buildlog_get_change_summary()` - Integrates into BUILDLOG.md
- Tests in `tests/testthat/test-build-change-summary.R`

## Build Directory Clearing System

The package includes a sophisticated directory clearing system that manages when and how output directories are cleared during builds.

### Clear Output Modes

The `clear_output` parameter (or `PROJR_CLEAR_OUTPUT` environment variable) controls when directories are cleared:

| Mode | When Cleared | Safe Directories | Unsafe Directories | Use Case |
|------|--------------|------------------|-------------------|----------|
| `"pre"` | Before build starts | ✓ Cleared | ✓ Cleared | Default: Ensures clean slate, allows saving directly to final locations |
| `"post"` | After build completes | ✓ Cleared | ✗ Not cleared | Conservative: Preserves final outputs until after successful build |
| `"never"` | Never | ✗ Not cleared | ✗ Not cleared | Manual control: User manages clearing |

**Safe vs Unsafe Directories**:
- **Safe directories**: Temporary build locations in cache (e.g., `_tmp/projr/v0.0.1/output`)
- **Unsafe directories**: Final output locations (e.g., `_output`, `docs`)

### Build Pre-Clear Behavior

The `.build_clear_pre()` function is called at the start of builds and performs these operations:

1. **Clears output directories** (via `.build_clear_pre_output()`):
   - Gets all output labels from `_projr.yml` (excluding docs)
   - For each output label:
     - **Always** clears safe (cache) directory
     - **Only** clears unsafe (final) directory if `clear_output = "pre"`
   
2. **Clears cache version directory** (via `.dir_clear_pre_cache_version()`):
   - Clears versioned cache directory (e.g., `_tmp/projr/v0.0.1/`)
   - **Preserves** "old" subdirectory for archival purposes
   - Removes all other subdirectories and files

3. **Does NOT clear docs directories**:
   - Docs clearing is handled separately in post-build phase
   - Allows different rendering engines to control their own output

### Implementation Details

**Key Functions** (in `R/build-pre-clear.R`):
- `.build_clear_pre(output_run, clear_output)` - Main pre-clear function
- `.build_clear_pre_output(clear_output)` - Clears output directories
- `.build_clear_pre_output_label(label, clear_output)` - Clears specific output label
- `.dir_clear_pre_cache_version()` - Clears cache with "old" preservation

**Directory Exclusion** (in `R/path.R`):
- `.dir_clear_dir(path, dir_exc)` - Clears directories, excluding specified subdirectories
- `.dir_clear_file(path, dir_exc)` - Clears files, excluding those in specified subdirectories
- The `dir_exc` parameter uses **relative paths** (e.g., `"old"`, not `/full/path/to/old`)

### Testing Patterns

Comprehensive tests are in `tests/testthat/test-build-pre-clear-comprehensive.R`:

```r
test_that(".build_clear_pre clears based on clear_output parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)
      
      # Test with clear_output = "pre" (clears both)
      .build_clear_pre(output_run = TRUE, clear_output = "pre")
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_true(.check_dir_cleared("output", safe = FALSE))
      
      # Test with clear_output = "post" (clears only safe)
      .create_clear_test_content("output", safe = TRUE)
      .create_clear_test_content("output", safe = FALSE)
      .build_clear_pre(output_run = TRUE, clear_output = "post")
      expect_true(.check_dir_cleared("output", safe = TRUE))
      expect_false(.check_dir_cleared("output", safe = FALSE))
      
      # Test with clear_output = "never"
      .create_clear_test_content("output", safe = FALSE)
      .build_clear_pre(output_run = TRUE, clear_output = "never")
      expect_false(.check_dir_cleared("output", safe = FALSE))
    }
  )
})
```

### Common Issues and Solutions

**Issue**: Files remain after build with `clear_output = "pre"`
- **Cause**: Files created after the pre-clear phase
- **Solution**: Use `clear_output = "post"` to clear after build completion

**Issue**: Need to preserve specific subdirectories during clear
- **Cause**: Default behavior clears all subdirectories
- **Solution**: Use `dir_exc` parameter in custom clear functions (relative paths only)

**Issue**: Cache fills up with old build artifacts
- **Cause**: "old" directory preserved indefinitely
- **Solution**: Manually clear `_tmp/projr/*/old/` or use `projr_log_clear()`

### Related Environment Variables

See [PROJR_CLEAR_OUTPUT](#projr_clear_output) for configuration details.


**Example Debug Session**:
```r
# Enable debug output
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")

# Add local destination
projr_yml_dest_add_local(
  title = "archive",
  content = "raw-data",
  path = "_archive",
  structure = "archive",
  send_cue = "if-change",
  send_strategy = "sync-diff",
  send_inspect = "manifest"
)

# Build - will show detailed remote operations
projr_build_patch()

# Output will include:
# - "Content 'raw-data': Starting processing for destination 'archive' (type: local)"
# - "Content 'raw-data': Remote configuration - id: _archive, structure: archive, strategy: sync-diff, inspect: manifest"
# - "Content 'raw-data': Upload plan - X file(s) to add, Y file(s) to remove, create: TRUE/FALSE, purge: TRUE/FALSE"
# - "Remote file add: type=local, adding X file(s) from /path/to/source"
```

### Implementation Files

- `R/log.R`: Core logging functions (.log_* internal functions)
- `R/cli-output.R`: CLI output with integrated logging
- `R/build.R`: Build process with log initialization and finalization

**Important**: When modifying build-related code, ensure log messages are properly passed through the `log_file` parameter to maintain logging functionality.

## Manifest System

The package includes a manifest system that tracks file hashes across different project versions. This allows users to query which files changed between versions, understand the history of their project data, and ensure reproducibility.

### Manifest Structure

The manifest is stored in `manifest.csv` at the project root. Each row represents a file at a specific version:

| Column | Description |
|--------|-------------|
| `label` | Directory label (e.g., "output", "raw-data", "cache", "docs") |
| `fn` | File path relative to directory |
| `version` | Project version when file was hashed (e.g., "v0.0.1") |
| `hash` | MD5 hash of file content |

### How Manifests Are Built

Manifests are automatically updated during builds:

1. **Pre-build phase** (`.build_manifest_pre()`):
   - Hashes files in input directories (raw-data, cache if configured)
   - Stores hashes in a temporary manifest file in cache

2. **Post-build phase** (`.build_manifest_post()`):
   - Hashes files in output directories (output, docs)
   - Merges with pre-build manifest
   - Appends to previous manifest versions
   - Writes to `manifest.csv` at project root

### User-Facing Query Functions

Three exported functions allow users to query the manifest (in `R/manifest-query.R`):

#### `projr_manifest_changes(version_from, version_to, label)`
Query which files changed between two versions.

```r
# Changes between v0.0.1 and v0.0.2
projr_manifest_changes("0.0.1", "0.0.2")

# Changes in output directory only
projr_manifest_changes("0.0.1", "0.0.2", label = "output")
```

Returns a data.frame with:
- `label`: Directory label
- `fn`: File path
- `change_type`: "added", "modified", or "removed"
- `hash_from`: Hash in version_from (NA for added files)
- `hash_to`: Hash in version_to (NA for removed files)

#### `projr_manifest_range(version_start, version_end, label)`
Query which files changed across a version range.

```r
# All changes from v0.0.1 to current
projr_manifest_range("0.0.1")

# Changes in specific range
projr_manifest_range("0.0.1", "0.0.5")
```

Returns a data.frame with:
- `label`: Directory label
- `fn`: File path
- `version_first`: First version where file appeared
- `version_last_change`: Last version where file was modified
- `hash`: Current file hash

#### `projr_manifest_last_change(version)`
Query when files in each directory last changed.

```r
# Last changes for current version
projr_manifest_last_change()

# Last changes as of v0.0.5
projr_manifest_last_change("0.0.5")
```

Returns a data.frame with:
- `label`: Directory label
- `version_last_change`: Most recent version with changes
- `n_files`: Number of files in directory at this version

### Internal Functions

- `R/manifest.R`: Core manifest operations
  - `.manifest_hash_label()`: Hash files in a directory
  - `.manifest_read()`: Read manifest from file
  - `.manifest_write()`: Write manifest to file
  - `.manifest_filter_*()`: Filter manifest by label/version
  
- `R/build-manifest.R`: Build-time manifest operations
  - `.build_manifest_pre()`: Pre-build manifest creation
  - `.build_manifest_post()`: Post-build manifest creation

- `R/manifest-query.R`: User-facing query functions
  - Helper functions for version normalization and comparison

### Testing

Tests for manifest functionality are in:
- `tests/testthat/test-manifest.R`: Core manifest operations
- `tests/testthat/test-manifest-query.R`: Query functions (34 tests)

Tests verify:
- Empty directory handling (returns 0-row tables)
- Multi-version tracking across builds
- File change detection (added, modified, removed)
- Version range queries
- Edge cases (empty manifests, same version comparisons)

### Common Patterns

```r
# Read project manifest
manifest <- .manifest_read_project()

# Filter by label
output_files <- .manifest_filter_label(manifest, "output")

# Filter by version
v1_files <- .manifest_filter_version(manifest, "0.0.1")

# Get empty tables
.zero_tbl_get_manifest()  # 0-row manifest table
.zero_tbl_get_manifest_changes()  # 0-row changes table
```

## Restore Functions

The package includes functions to restore project artefact directories from remote sources. These functions allow users to download and restore data that was previously sent to remote destinations.

### Exported Functions

#### `projr_restore(label, pos, type, title)`
Restores artefacts in an existing local project from configured remote sources.

**Parameters:**
- `label`: NULL or character vector of directory labels to restore (default: all "raw" directories)
- `pos`: NULL or character vector specifying source preference ("source" or "dest", default: both)
- `type`: NULL or single character specifying remote type ("local", "osf", "github")
- `title`: NULL or single character specifying remote title from `_projr.yml`

**Returns:** Invisibly returns `TRUE` if all restorations succeed, `FALSE` otherwise

**Requirements:**
- Must have a `manifest.csv` file in the project root
- Configured remote sources in `_projr.yml`

#### `projr_restore_repo(repo, path, label, pos, type, title)`
Clones a GitHub repository and restores its artefacts from remote sources.

**Parameters:**
- `repo`: Single character string for GitHub repository ("owner/repo")
- `path`: NULL or single character for clone destination (default: subdirectory named after repo)
- `label`, `pos`, `type`, `title`: Same as `projr_restore()`

**Returns:** Invisibly returns `TRUE` if clone and restoration succeed, `FALSE` otherwise

#### `projr_restore_repo_wd(repo, label, pos, type, title)`
Convenience wrapper that clones repository into current working directory and restores artefacts.

**Parameters:**
- `repo`: Single character string for GitHub repository
- `label`, `pos`, `type`, `title`: Same as `projr_restore()`

**Returns:** Same as `projr_restore_repo()`

### Input Validation

All restore functions perform comprehensive input validation:

**Parameter Type Checks:**
- All parameters must be either NULL or the expected type (character)
- No numeric, logical, or list types accepted (except where specified)

**Parameter Length Checks:**
- `label`: Can be vector with multiple values or NULL
- `pos`: Can be vector with multiple values ("source" and/or "dest") or NULL
- `type`: Must be single value or NULL
- `title`: Must be single value or NULL
- `repo`: Must be single value (cannot be NULL)
- `path`: Must be single value or NULL

**Parameter Value Checks:**
- `pos`: Must only contain "source" and/or "dest"
- `type`: Must be one of "local", "osf", or "github"
- `label`: Must be valid directory labels defined in `_projr.yml`
- `repo`, `path`, `title`: Cannot be empty strings

**Example validation errors:**
```r
# Invalid type
projr_restore(label = 123)  # Error: 'label' must be NULL or a character vector

# Invalid pos value
projr_restore(pos = "invalid")  # Error: 'pos' must be 'source' or 'dest'

# Invalid type value
projr_restore(type = "invalid")  # Error: 'type' must be one of: local, osf, github

# Multiple values where single expected
projr_restore(type = c("local", "osf"))  # Error: 'type' must be a single character value

# Empty vector
projr_restore(label = character(0))  # Error: 'label' must have at least one element
```

### Error Handling and Edge Cases

The restore functions handle errors gracefully:

**Missing Files:**
- Missing `manifest.csv`: Stops with informative error
- No labels to restore: Returns `FALSE` with message

**Invalid Sources:**
- No restore source found for label: Skips with message, continues with other labels
- Invalid remote configuration: Caught and reported per label

**Git Clone Failures:**
- Wrapped in tryCatch to prevent crashes
- Returns `FALSE` with error message

**Partial Success:**
- Errors in one label don't prevent restoration of others
- Overall success is `FALSE` if any label fails
- Each failure is logged with descriptive message

**Example error handling:**
```r
# Missing manifest
projr_restore()  # Error: No manifest.csv file found

# No labels to restore
projr_restore(label = NULL)  # If no raw directories exist
# Message: "No labels to restore"
# Returns: FALSE

# Label with no files
projr_restore(label = "empty-dir")
# Message: "No files kept in empty-dir"
# Message: "Skipping restore for empty-dir"
# Returns: FALSE

# Git clone failure
projr_restore_repo("nonexistent/repo")
# Message: "Error in projr_restore_repo: ..."
# Returns: FALSE
```

### Internal Functions

Key internal functions (not exported):

- `.restore_get_label()`: Gets labels to restore (defaults to raw directories)
- `.restore_label()`: Restores a single label from remote source
- `.restore_label_check_non_empty()`: Checks if label has files to restore
- `.restore_label_get_source()`: Finds appropriate remote source for label
- `.restore_repo_labels()`: Helper for repository restoration functions

### Testing Pattern

When testing restore functions:

```r
test_that("projr_restore validates parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Create minimal manifest
      writeLines("label,fn,version,hash\nraw-data,test.txt,v0.0.1,abc", "manifest.csv")
      
      # Test valid inputs
      expect_error(projr_restore(label = NULL), NA)
      expect_error(projr_restore(label = "raw-data"), NA)
      
      # Test invalid inputs
      expect_error(projr_restore(label = 123), "'label' must be NULL or a character vector")
      expect_error(projr_restore(pos = "invalid"), "'pos' must be 'source' or 'dest'")
    },
    force = TRUE,
    quiet = TRUE
  )
})
```

### Common Pitfalls

- Don't forget to create `manifest.csv` before calling `projr_restore()`
- Validate all parameters before calling git operations
- Use tryCatch to handle errors gracefully per label
- Return meaningful success/failure values (TRUE/FALSE)
- Provide informative error messages for debugging

## renv Package Management Functions


The package includes several exported functions to manage renv environments and package installations from lockfiles.
### Main Functions

#### `projr_renv_restore(github, non_github, biocmanager_install)`
Restores packages from the lockfile, attempting to install the exact versions specified.

**Parameters**:
- `github` (logical): Whether to restore GitHub packages. Default is `TRUE`.
- `non_github` (logical): Whether to restore non-GitHub packages (CRAN and Bioconductor). Default is `TRUE`.
- `biocmanager_install` (logical): If `TRUE`, Bioconductor packages are installed using `BiocManager::install()`; otherwise, uses `renv::install("bioc::<package>")`. Default is `FALSE`.

**Validation**:
- All parameters must be single logical values
- At least one of `github` or `non_github` must be `TRUE`
- Checks for `renv.lock` file existence

#### `projr_renv_update(github, non_github, biocmanager_install)`
Updates packages to their latest available versions, ignoring the lockfile versions.

**Parameters**: Same as `projr_renv_restore()`

**Validation**: Same as `projr_renv_restore()`

#### `projr_renv_restore_and_update(github, non_github, biocmanager_install)`
First restores packages from the lockfile, then updates them to the latest versions.

**Parameters**: Same as `projr_renv_restore()`

**Validation**: Same as `projr_renv_restore()`

#### `projr_renv_test(files_to_copy, delete_lib)`
Tests `renv::restore()` in a clean, isolated temporary environment without using the cache.

**Parameters**:
- `files_to_copy` (character vector): Paths to files to copy into the temporary directory. `renv.lock` is always copied.
- `delete_lib` (logical): If `TRUE`, the restored library path is deleted after the test. Default is `TRUE`.

**Returns**: `TRUE` if `renv::restore()` succeeds, `FALSE` otherwise.

### Internal Validation Functions

- `.check_renv()`: Checks if renv package is installed
- `.check_renv_params()`: Validates all parameters for renv functions
- `.check_renv_lockfile()`: Checks for renv.lock file existence

### Error Messages

The functions provide informative error messages for:
- Invalid parameter types (non-logical values, vectors with length > 1)
- Both package sources disabled (`github = FALSE` and `non_github = FALSE`)
- Missing renv.lock file
- renv package not installed

### Testing

Tests for renv functions are in `tests/testthat/test-renv.R`:
- Parameter validation tests for all three main functions
- Edge case tests (missing lockfile, invalid parameters)
- Integration tests (restore and update workflows)
- Tests use `.renv_rest_init()` and `.renv_test_test_lockfile_create()` helper functions

**Note**: Some tests are skipped in offline mode using `skip_if_offline()`.

## Environment Variables

The package uses several environment variables to control behavior. All environment variables are optional and have sensible defaults.

### Project Configuration

#### PROJR_PROFILE
- **Purpose**: Specifies which profile-specific YAML configurations to load
- **Format**: Comma or semicolon-separated list of profile names
- **Default**: None (uses `_projr.yml` only)
- **Example**: `PROJR_PROFILE="test,dev"` or `PROJR_PROFILE="test;dev"`
- **Behavior**:
  - Supports both comma (`,`) and semicolon (`;`) separators
  - Whitespace around values is automatically trimmed
  - Earlier profiles take precedence over later ones
  - The values `"default"` and `"local"` are filtered out (handled separately)
  - Profiles are loaded in this order:
    1. `_environment.local` (highest precedence, git-ignored)
    2. `_environment-<QUARTO_PROFILE>` (if set)
    3. `_environment-<PROJR_PROFILE>` (if set)
    4. `_environment` (lowest precedence)

#### QUARTO_PROFILE
- **Purpose**: Quarto profile that also affects environment file loading
- **Format**: Comma-separated list of profile names
- **Default**: None
- **Behavior**: Takes precedence over `PROJR_PROFILE` for environment file loading

### Build Control

#### PROJR_OUTPUT_LEVEL
- **Purpose**: Controls verbosity of console output during builds
- **Values**: `"none"`, `"std"`, `"debug"`
- **Default**: `"none"` for dev builds, `"std"` for output builds
- **Behavior**:
  - `"none"`: No additional console output
  - `"std"`: Standard informational messages
  - `"debug"`: Verbose output including debug messages
  - Case-sensitive (must be lowercase)
  - Can be overridden by explicit `output_level` parameter in build functions

#### PROJR_LOG_DETAILED
- **Purpose**: Controls whether detailed build logs are written to files
- **Values**: TRUE/FALSE representations
- **Default**: `"TRUE"`
- **Behavior**:
  - TRUE values: `"TRUE"`, `"true"`, `"1"`, `"YES"`, `"yes"`, `"Y"`, `"y"`
  - FALSE values: `"FALSE"`, `"false"`, `"0"`, `"NO"`, `"no"`, `"N"`, `"n"`
  - Case-insensitive for boolean values
  - When `TRUE`: Creates detailed `.qmd` log files in `cache/projr/log/`
  - When `FALSE`: Still maintains build history but skips detailed logs
  - History tracking (`builds.md`) is always maintained regardless of this setting

#### PROJR_CLEAR_OUTPUT
- **Purpose**: Controls when to clear output directories during builds
- **Values**: `"pre"`, `"post"`, `"never"`
- **Default**: `"pre"`
- **Behavior**:
  - `"pre"`: Clear output before build starts
  - `"post"`: Clear output after build completes
  - `"never"`: Never automatically clear output
  - Case-sensitive (must be lowercase)
  - Can be overridden by explicit `clear_output` parameter in build functions

### Authentication

#### GITHUB_PAT
- **Purpose**: GitHub Personal Access Token for GitHub API operations
- **Default**: None
- **Behavior**:
  - Checked before `GITHUB_TOKEN`
  - Falls back to `gitcreds` package if not set
  - Required for creating GitHub repositories and releases
  - Should have appropriate scopes (repo, workflow, etc.)

#### GITHUB_TOKEN
- **Purpose**: Alternative GitHub token (lower precedence than GITHUB_PAT)
- **Default**: None
- **Behavior**: Used only if `GITHUB_PAT` is not set

#### OSF_PAT
- **Purpose**: Open Science Framework Personal Access Token
- **Default**: None
- **Behavior**: Required for OSF remote destinations

### Testing Control

#### R_PKG_TEST_IN_PROGRESS
- **Purpose**: Indicates tests are running
- **Values**: `"TRUE"` or unset
- **Usage**: Internal, set by test runner

#### R_PKG_TEST_FAST
- **Purpose**: Skip slow tests
- **Values**: `"TRUE"` or unset
- **Usage**: Set to skip integration and slow tests

#### R_PKG_TEST_SELECT
- **Purpose**: Skip most tests (for targeted testing)
- **Values**: `"TRUE"` or unset
- **Usage**: Set to run only specific tests

### Environment Variable Files

The package supports loading environment variables from files:

1. **`_environment.local`**: Machine-specific overrides (git-ignored, highest precedence)
2. **`_environment-<profile>`**: Profile-specific variables
3. **`_environment`**: Global defaults (lowest precedence)

**Behavior**:
- Variables are only set if not already defined (existing values are preserved)
- Comments are supported (lines starting with `#` or inline after `#`)
- Format: `VARIABLE_NAME=value` (one per line)
- Empty values and invalid formats are silently ignored
- Special characters in values are preserved (spaces, URLs, paths)
- The `_environment.local` file is automatically added to `.gitignore`

**Example `_environment` file**:
```bash
# Project configuration
PROJR_OUTPUT_LEVEL=debug
PROJR_LOG_DETAILED=TRUE

# Authentication (use _environment.local for actual tokens)
# GITHUB_PAT=your_token_here

# URLs and paths work too
API_URL=https://api.example.com?param=value
DATA_PATH=/path/to/data
```


## Hash Functions

The package includes comprehensive file and directory hashing functionality used throughout the manifest and change detection systems. Hash functions compute MD5 hashes of file contents to track changes over time.

### Core Hash Functions (R/hash.R)

#### `.hash_file(fn)`
Hash one or more files and return their MD5 hashes.

**Parameters**:
- `fn` (character vector): File path(s) to hash

**Returns**: Named character vector with hashes (names are file paths)

**Examples**:
```r
# Hash a single file
hash <- .hash_file("path/to/file.txt")

# Hash multiple files
hashes <- .hash_file(c("file1.txt", "file2.txt", "file3.txt"))

# Files with identical content have identical hashes
all(hashes[c(1, 3)] == hashes[1])  # TRUE if file1 and file3 have same content
```

**Implementation Notes**:
- Uses `digest::digest()` with `serialize = FALSE` and `file = TRUE`
- Vectorized via `vapply()` over input files
- Returns character vector with file paths as names
- Use `unname()` for comparison if names interfere

#### `.hash_dir(path_dir, version = NULL, dir_exc = NULL)`
Hash all files in a directory recursively and return a data frame with file paths, versions, and hashes.

**Parameters**:
- `path_dir` (character): Directory path to hash
- `version` (character, optional): Version to assign (defaults to current project version)
- `dir_exc` (character vector, optional): Directory names to exclude from hashing

**Returns**: Data frame with columns `fn` (relative path), `version`, `hash`

**Examples**:
```r
# Hash all files in a directory
hash_tbl <- .hash_dir("_output")

# Hash with specific version
hash_tbl <- .hash_dir("_output", version = "1.0.0")

# Hash with exclusions (excludes "projr" subdirectory from cache)
hash_tbl <- .hash_dir("_tmp", dir_exc = "projr")

# Empty directory returns 0-row data frame
hash_tbl <- .hash_dir("empty_dir")  # nrow(hash_tbl) == 0
```

**Implementation Notes**:
- Uses `.file_ls()` to recursively list files
- Filters excluded directories using `.path_filter_spec()` **before** converting to absolute paths
- Returns relative paths in `fn` column
- Includes hidden files (starting with `.`)
- Version is prefixed with "v" automatically via `.version_v_add()`

**Bug Fix History**:
- **Fixed in this PR**: `dir_exc` filtering was applied after `file.path()` call, preventing exclusions from working. Now filters relative paths before making them absolute.

#### `.zero_tbl_get_hash()`
Return an empty hash table with the correct structure.

**Returns**: 0-row data frame with columns `fn`, `version`, `hash` (all character)

### Change Detection Functions (R/change.R)

#### `.change_get_hash(hash_pre, hash_post)`
Compare two hash tables and identify added, removed, unchanged, and modified files.

**Parameters**:
- `hash_pre` (data frame): Hash table for "before" state (must have `fn` and `hash` columns)
- `hash_post` (data frame): Hash table for "after" state (must have `fn` and `hash` columns)

**Returns**: List with 4 character vectors:
- `fn_dest_extra`: Files removed (in pre but not in post)
- `fn_same`: Files unchanged (in both with same hash)
- `fn_diff`: Files modified (in both with different hash)
- `fn_source_extra`: Files added (in post but not in pre)

**Examples**:
```r
# Create hash tables
hash_pre <- data.frame(
  fn = c("file1.txt", "file2.txt"),
  version = c("v1.0.0", "v1.0.0"),
  hash = c("hash1", "hash2"),
  stringsAsFactors = FALSE
)

hash_post <- data.frame(
  fn = c("file1.txt", "file2.txt", "file3.txt"),
  version = c("v1.0.0", "v1.0.0", "v1.0.0"),
  hash = c("hash1", "hash2_modified", "hash3"),
  stringsAsFactors = FALSE
)

# Compare
result <- .change_get_hash(hash_pre, hash_post)

# result$fn_same: "file1.txt" (unchanged)
# result$fn_diff: "file2.txt" (modified)
# result$fn_source_extra: "file3.txt" (added)
# result$fn_dest_extra: character(0) (none removed)
```

**Implementation Notes**:
- Uses `%in%` operator for set operations
- Handles empty hash tables correctly (returns appropriate empty vectors)
- Applies `.filter_filter_non_na()` to clean up NA values

#### `.change_get_dir(path_dir_pre, path_dir_post)`
Compare two directories by hashing their contents and detecting changes.

**Parameters**:
- `path_dir_pre` (character): "Before" directory path
- `path_dir_post` (character): "After" directory path

**Returns**: Same structure as `.change_get_hash()` (list with 4 character vectors)

**Examples**:
```r
# Compare two directories
result <- .change_get_dir("_output_v1", "_output_v2")

# Files in result$fn_same have not changed
# Files in result$fn_diff have been modified
# Files in result$fn_source_extra are new
# Files in result$fn_dest_extra were removed
```

**Implementation Notes**:
- Hashes both directories using `.hash_dir()`
- Calls `.change_get_hash()` to compare the hash tables

**Bug Fix History**:
- **Fixed in this PR**: Function had `stop("this should not happen like this")` instead of actually hashing the pre directory

### Testing Hash Functions

Comprehensive tests are in `tests/testthat/test-hash.R` (84 assertions across 13 test cases):

**`.hash_file()` tests**:
- Single file hashing
- Multiple file hashing (with name handling)
- Different file types (empty, binary, large text)
- Identical content produces identical hashes
- Different content produces different hashes

**`.hash_dir()` tests**:
- Empty directories (returns 0-row table)
- Nested directory structures
- Directory exclusion (`dir_exc` parameter)
- Hidden files (included in hash)
- Version assignment
- Relative paths in output

**`.change_get_hash()` tests**:
- Detecting added files (empty pre, populated post)
- Detecting removed files (populated pre, empty post)
- Detecting unchanged files (identical hashes)
- Detecting modified files (same filename, different hash)
- Mixed changes (combination of all types)

**`.change_get_dir()` tests**:
- Directory comparison end-to-end
- Correctly identifies all change types

**Test Patterns**:
```r
test_that(".hash_file works with single file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      test_file <- file.path(tempdir(), "test.txt")
      writeLines("content", test_file)
      
      hash_result <- .hash_file(test_file)
      
      expect_true(is.character(hash_result))
      expect_identical(length(hash_result), 1L)
      
      # Verify same content = same hash
      hash_result2 <- .hash_file(test_file)
      expect_identical(hash_result, hash_result2)
      
      unlink(test_file)
    }
  )
})
```

### Common Patterns

```r
# Hash a directory for tracking changes
hash_tbl <- .hash_dir(projr_path_get_dir("output"))

# Compare two versions
hash_pre <- .hash_dir(projr_path_get_dir("output", safe = TRUE))
hash_post <- .hash_dir("_output")
changes <- .change_get_hash(hash_pre, hash_post)

# Check if files changed
if (length(changes$fn_diff) > 0) {
  message("Modified files: ", paste(changes$fn_diff, collapse = ", "))
}

# Exclude specific directories from hashing
hash_tbl <- .hash_dir("_tmp", dir_exc = c("projr", "cache_old"))
```

### Key Design Principles

1. **Relative Paths**: Hash tables always store relative paths (relative to hashed directory)
2. **Version Prefixing**: Versions are always prefixed with "v" in hash tables
3. **Empty Handling**: Empty directories and comparisons return well-structured 0-row tables
4. **Filter Before Absolute**: Directory exclusions must be filtered before converting paths to absolute
5. **Named Vectors**: `.hash_file()` returns named vectors (file paths as names) - use `unname()` for comparisons


## Directory Functions

The package includes a comprehensive directory management system that handles project directory paths with support for safe/unsafe modes, creation, and various directory labels.

### Core Exported Functions

#### `projr_path_get_dir(label, ..., create, relative, absolute, safe)`

Returns path to a profile-specific directory. The primary function for getting directory paths.

**Parameters**:
- `label` (character): Directory label - one of "raw-data", "cache", "output", "docs", "project", "code", or "data"
- `...`: Optional subdirectory path components (passed to `file.path`)
- `create` (logical): If TRUE, creates the directory if it doesn't exist. Default: TRUE
- `relative` (logical): If TRUE, returns path relative to project root. Default: FALSE
- `absolute` (logical): If TRUE, forces absolute path. Default: FALSE
- `safe` (logical): If TRUE, uses safe cache directory for output/docs. Default: TRUE

**Examples**:
```r
# Get cache directory (created if doesn't exist)
projr_path_get_dir("cache")

# Get cache subdirectory
projr_path_get_dir("cache", "figures", "plot1")

# Get output directory (safe mode - in cache/projr/v0.0.0-1/output)
projr_path_get_dir("output", safe = TRUE)

# Get output directory (unsafe mode - actual _output directory)
projr_path_get_dir("output", safe = FALSE)

# Get relative path
projr_path_get_dir("cache", relative = TRUE)

# Get without creating
projr_path_get_dir("cache", create = FALSE)
```

**Important Notes**:
- Cannot specify both `relative = TRUE` and `absolute = TRUE` (will error)
- The `safe` parameter affects `output`, `docs`, and `data` directories
  - `safe = TRUE`: Returns cache build directory (e.g., `_tmp/projr/v0.0.0-1/output`)
  - `safe = FALSE`: Returns actual directory (e.g., `_output`)
- The `create` parameter adds directory to `.gitignore` and `.Rbuildignore` as specified in `_projr.yml`

#### `projr_path_get(label, ..., create, relative, absolute, safe)`

Returns path to a file (or directory). Similar to `projr_path_get_dir` but for file paths.

**Difference from `projr_path_get_dir`**:
- When `create = TRUE`, creates the **parent directory**, not the full path
- Last argument in `...` is treated as the filename
- If no additional arguments, behaves identically to `projr_path_get_dir`

**Examples**:
```r
# Get path to a file in cache (creates parent directory)
projr_path_get("cache", "data", "results.csv", create = TRUE)

# Equivalent to projr_path_get_dir when no filename
projr_path_get("cache")
```

#### `projr_path_get_cache_build_dir(..., create, profile)`

Get the cache directory for `projr` builds. This is a sub-directory of the cache directory.

**Parameters**:
- `...`: Optional subdirectory path components
- `create` (logical): If TRUE, creates directory if it doesn't exist
- `profile` (character): Profile name. Default: NULL (uses current profile)

**Usage**:
- For development builds: This is the final directory for output and docs items
- For production builds: This is the staging directory

**Examples**:
```r
# Get cache build directory
projr_path_get_cache_build_dir(profile = NULL)

# Get cache build output subdirectory
projr_path_get_cache_build_dir("output", profile = NULL)
```

#### `projr_path_get_cache_build(..., create, profile)`

Similar to `projr_path_get_cache_build_dir` but for file paths.

### Directory Labels

Valid directory labels and their meanings:

| Label | Description | Default Path |
|-------|-------------|--------------|
| `"raw-data"` | Raw data directory | `_raw_data` |
| `"cache"` | Cache directory | `_tmp` |
| `"output"` | Output directory | `_output` (unsafe) or `_tmp/projr/vX.Y.Z/output` (safe) |
| `"docs"` | Documentation output | Depends on engine (`_book`, `_site`, or `docs`) |
| `"project"` | Project root | `.` |
| `"code"` | Code directory (temporary) | Random temp directory |
| `"data"` | Data directory | `data` (unsafe) or `_tmp/projr/vX.Y.Z/data` (safe) |

**Label Validation**:
- Labels are stripped of hyphens, underscores, and case for matching
- Example: "raw-data", "raw_data", "rawdata", and "RAW-DATA" all resolve to the same directory
- Invalid labels will error with a helpful message

### Safe vs Unsafe Modes

The `safe` parameter controls where output-related directories are located:

**Safe Mode (`safe = TRUE`)** - Default:
- Used during builds to avoid overwriting production files
- Paths go to versioned cache directory: `_tmp/projr/v0.0.0-1/<label>`
- Applies to: `output`, `docs`, `data` directories

**Unsafe Mode (`safe = FALSE`)**:
- Points to actual production directories
- Use when you want to work with final output locations
- Paths go to configured directory: `_output`, `_book`, `data`, etc.

### Internal Directory Functions

#### Core Path Resolution Functions (R/dir-get.R)

- `.dir_get(label, ..., safe)` - Get directory path for a label
- `.dir_get_label(label, safe)` - Get label path with safe/unsafe handling
- `.dir_get_label_safe(label)` - Get safe (cache build) path for label
- `.dir_get_label_unsafe(label)` - Get unsafe (actual) path for label
- `.dir_get_code()` - Get temporary code directory
- `.dir_get_tmp_random(...)` - Get random temporary directory

#### Document Engine Specific Functions

- `.dir_get_docs_quarto_project()` - Get Quarto project docs directory
- `.dir_get_docs_bookdown()` - Get Bookdown docs directory
- `.dir_get_docs_md()` - Get Rmd/qmd docs directory
- `.dir_set_docs_quarto_project(path)` - Set Quarto output directory in config
- `.dir_set_docs_bookdown(path)` - Set Bookdown output directory in config

#### Cache Build Directory Functions

- `.dir_get_cache_auto_version(..., create, profile)` - Get versioned cache build directory
- `.path_get_cache_auto_version(..., create, profile)` - Get versioned cache build file path
- `.dir_get_cache_auto_path(profile)` - Get base cache path
- `.dir_get_cache_auto_check(profile)` - Validate cache directory exists
- `.dir_get_cache_auto_ind(profile)` - Get index of cache directory in yml

#### Validation Functions (R/dir.R)

- `.dir_get_check(label, dots_list, relative, absolute, safe)` - Validate parameters
- `.dir_check_label(label, profile)` - Validate label is a string
- `.dir_check_label_found(label, profile)` - Validate label exists in yml
- `.dir_check_label_strip(label)` - Validate stripped label format
- `.dir_label_strip(x)` - Strip hyphens/underscores and lowercase

#### Path Transformation Functions

- `.dir_get_create(path, create)` - Create directory if requested
- `.dir_get_rel(path, relative)` - Convert to relative path if requested
- `.dir_get_abs(path, absolute)` - Convert to absolute path if requested

#### Directory Creation Functions

**Note**: There are TWO different `.dir_create` functions in different files:

1. **R/path.R** - `.dir_create(path_dir)`:
   - Takes actual directory paths as input
   - Creates directories recursively
   - Used by most internal functions
   - Loaded later, so this is the active version

2. **R/dir.R** - `.dir_create(label, ..., safe = TRUE)`:
   - Takes directory labels as input
   - Calls `projr_path_get_dir` internally
   - Intended for creating directories by label
   - Overridden by path.R version in current codebase

**Best Practice**: Use `projr_path_get_dir(label, create = TRUE)` to create directories by label.

### Testing Directory Functions

Comprehensive tests are in `tests/testthat/test-dir-comprehensive.R` covering:

1. **Basic functionality**: All directory labels work correctly
2. **Safe/unsafe modes**: Correct paths returned for each mode
3. **Create parameter**: Directories created when `create = TRUE`, not created when `FALSE`
4. **Relative/absolute paths**: Path transformation works correctly
5. **Subdirectories**: Multiple path components handled correctly
6. **File paths**: `projr_path_get` works for file paths
7. **Cache build functions**: Both dir and file variants work with profile parameter
8. **Label validation**: Invalid labels error appropriately
9. **Edge cases**: Special labels (code, project, data) behave correctly
10. **Path normalization**: Paths don't contain double slashes or trailing slashes

Example test pattern:
```r
test_that("projr_path_get_dir works with safe parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Test safe vs unsafe paths for output
      path_safe <- projr_path_get_dir("output", safe = TRUE, create = FALSE)
      path_unsafe <- projr_path_get_dir("output", safe = FALSE, create = FALSE)
      
      expect_false(identical(path_safe, path_unsafe))
      expect_true(grepl("projr", path_safe))
      expect_false(grepl("projr", path_unsafe))
    },
    force = TRUE,
    quiet = TRUE
  )
})
```

### Common Patterns

```r
# Get cache directory for saving temporary files
cache_dir <- projr_path_get_dir("cache", "temp")

# Get output directory during builds (safe mode)
output_dir <- projr_path_get_dir("output", safe = TRUE)

# Get actual output directory for final deployment
output_final <- projr_path_get_dir("output", safe = FALSE)

# Get path to a specific file
cache_file <- projr_path_get("cache", "results", "data.csv")

# Check if directory exists without creating
if (dir.exists(projr_path_get_dir("cache", create = FALSE))) {
  # Do something
}

# Get cache build directory for current version
build_dir <- projr_path_get_cache_build_dir(profile = NULL)
```


## Metadata System

The package includes a metadata system that collects information about the author, host system, and timestamps for use in changelogs and build tracking.

### Metadata Functions

Core metadata functions are in `R/metadata.R`:

- `.metadata_get_author_host()` - Get author/username (primary function)
- `.metadata_get_host()` - Get hostname/machine name
- `.metadata_get_date()` - Get current date in UTC (YYYY-MM-DD format)
- `.metadata_get_time()` - Get current time in UTC (HH:MM:SS format)
- `.metadata_get_os()` - Get operating system name

### Author Detection Hierarchy

The `.metadata_get_author_host()` function uses a sophisticated fallback mechanism to detect the author:

1. **Git config** (if git repo exists): Check git user.name from config
2. **Environment variable**: Check `USERNAME` (Windows) or `USER` (Linux/Darwin)
3. **System info**: Check `Sys.info()[["user"]]`
4. **System login**: Check `Sys.info()[["login"]]` (excluding "unknown")
5. **Hostname fallback**: Use `HOSTNAME` environment variable or "anonymous-user"

**Implementation detail**: The `.metadata_get_author_sys_info()` function checks both `Sys.info()[["user"]]` and `Sys.info()[["login"]]` as separate fallback steps. This is intentional - the first returns early if valid, the second has additional validation (excludes "unknown").

### Metadata Configuration (YAML)

Metadata can be stored in `_projr.yml` under the `metadata` key. Functions in `R/yml-metadata.R` handle reading/writing:

- `.yml_metadata_get(profile)` - Get entire metadata section
- `.yml_metadata_set(yml_metadata, profile)` - Set entire metadata section
- `.yml_metadata_get_nm(nm, profile)` - Get specific metadata field by name
- `.yml_metadata_set_nm(yml, nm, profile)` - Set specific metadata field

Common metadata fields:
- `version-format` - Version string format (e.g., "major.minor.patch-dev")

### Testing Metadata Functions

Test pattern for metadata functions (see `tests/testthat/test-metadata.R`):

```r
test_that(".metadata_get_author_host works without git", {
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = FALSE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Should fall back to non-git method
      author <- .metadata_get_author_host()
      expect_true(.is_chr(author))
      expect_true(length(author) == 1)
    }
  )
})

test_that(".metadata_get_date works", {
  skip_if(.is_test_select())

  date_str <- .metadata_get_date()
  expect_true(.is_chr(date_str))
  
  # Verify YYYY-MM-DD format
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str))
})
```

Key testing considerations:
- Test both git and non-git scenarios for author detection
- Verify format patterns for date/time strings (regex matching)
- Test fallback logic thoroughly
- Don't assume specific values - check types and formats instead

### Common Usage

Metadata functions are primarily used in:
- **Changelog generation** (`R/changelog.R`): Author, date, and time stamping
- **Build process** (`R/build-pre.R`): Tracking who initiated builds
- **Version management** (`R/version.R`): Version format configuration

Example from changelog:
```r
author <- .metadata_get_author_host()
date <- .metadata_get_date()
time <- .metadata_get_time()
```


### Testing Environment Variables

Comprehensive tests for environment variables are in:
- `tests/testthat/test-env.R`: Environment file loading and profile handling
- `tests/testthat/test-cli-output.R`: PROJR_OUTPUT_LEVEL behavior
- `tests/testthat/test-log.R`: PROJR_LOG_DETAILED behavior
- `tests/testthat/test-build.R`: PROJR_CLEAR_OUTPUT behavior
- `tests/testthat/test-auth.R`: Authentication token handling

## Maintaining Documentation and Instructions

### Copilot Instructions
When making changes that affect how developers should work with this codebase, update this file (`.github/copilot-instructions.md`):
- New code standards or style guidelines
- Changes to build/test/deployment processes
- New dependencies or tools
- Common patterns or best practices
- File organization changes

### pkgdown Website
The package documentation website is built with pkgdown and hosted at https://satvilab.github.io/projr/. Update the website when:
- Adding or modifying exported functions
- Changing function signatures or behavior
- Adding or updating vignettes
- Making significant changes to README or other user-facing documentation

To rebuild the website locally:
```r
pkgdown::build_site()
```

The website configuration is in `_pkgdown.yml`. GitHub Pages deployment is typically handled by GitHub Actions.
