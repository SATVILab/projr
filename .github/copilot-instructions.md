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
  - Manual tests in `tests/testthat/manual/`
- `man/`: Auto-generated documentation (DO NOT edit directly - use roxygen2)
- `vignettes/`: Package vignettes (R Markdown format)
- `inst/`: Package installation files
  - `inst/CITATION`: Citation information
  - `inst/project_structure/`: Project structure templates
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
- Test helpers available: `.test_setup_project()`, `.test_setup_content()`
- Common expect functions:
  - `expect_identical()` for exact matches
  - `expect_true()` / `expect_false()` for logical values
  - `expect_error()` for error conditions
  - Test edge cases: empty directories, missing files, NULL values

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

### 8. Build Scripts and Hooks Configuration

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
