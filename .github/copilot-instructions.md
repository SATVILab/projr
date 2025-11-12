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
- **Test**: `devtools::test()`
- **Check**: `devtools::check()`
- **Build**: `devtools::build()`

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
- Use testthat 3e (Config/testthat/edition: 3)
- Test file naming: `test-{feature}.R` (e.g., `test-manifest.R` for manifest-related functions)
- Use `test_that()` for each test case with descriptive names
- Use `skip_if(.is_test_select())` for tests that should be skipped in certain conditions
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

### 7. Version Control and Git
- The package includes Git integration (see `R/git.R`)
- Some functions interact with GitHub (using `gh` and `gert` packages)
- Test functions can create Git repos: `.test_setup_project(git = TRUE)`

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
- `"debug"` - Verbose messaging including remote operations details

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
