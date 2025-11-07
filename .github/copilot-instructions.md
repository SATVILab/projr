# Copilot Instructions for projr

This is an R package that facilitates reproducible and archived projects. The package helps users manage project structure, build processes, versioning, and deployment.

## Code Standards

### Required Before Each Commit
- Run `devtools::document()` to update roxygen2 documentation in `man/`
- Run `devtools::test()` to ensure all tests pass
- Run `devtools::check()` to ensure package passes R CMD check
- Ensure code follows the existing style conventions (see below)

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
