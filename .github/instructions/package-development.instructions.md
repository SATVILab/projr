---
applyTo: "**/*"
excludeAgent: copilot_code_review
---

# Package Development Workflow for projr

## Purpose & Scope

Development workflow guidelines for the projr R package, including build commands, commit requirements, and common development tasks.

---

## Required Before Each Commit

- Run `devtools::document()` to update roxygen2 documentation in `man/`
- Run `devtools::test()` to ensure all tests pass (use LITE mode for faster iteration)
- Run `devtools::check()` to ensure package passes R CMD check
- Ensure code follows the existing style conventions

---

## Development Flow

```r
# 1. Install dependencies
renv::restore()

# 2. Load package for interactive development
devtools::load_all()

# 3. Make changes to code

# 4. Update documentation
devtools::document()  # Updates man/ and NAMESPACE

# 5. Test changes (LITE mode for faster iteration)
devtools::load_all()
.test_set_lite()
devtools::test()

# 6. Check package
devtools::check()

# 7. Build package
devtools::build()

# 8. Update renv lockfile after adding dependencies
renv::snapshot()
```

## Testing During Development

Use LITE mode by default for faster testing:

```r
devtools::load_all()
.test_set_lite()
devtools::test()
```

Use FULL mode only when:
- Preparing for a release or major version bump
- Explicitly requested by the issue or PR
- Working on comprehensive parameter combination testing

## Common Development Commands

```r
# Run tests for specific file
devtools::test_file("tests/testthat/test-manifest.R")

# Install package locally
devtools::install()

# Rebuild pkgdown website
pkgdown::build_site()
```

---

## Repository Structure

### Core Directories

- `R/` - R source code, all package functions (70+ files)
- `tests/testthat/` - Unit tests using testthat 3e
- `man/` - Auto-generated documentation (DO NOT edit directly)
- `vignettes/` - Package vignettes (R Markdown format)
- `inst/` - Package installation files
- `renv/` - renv package management
- `.github/workflows/` - GitHub Actions workflows

### Configuration Files

- `DESCRIPTION` - Package metadata, dependencies, and configuration
- `NAMESPACE` - Auto-generated exports (managed by roxygen2)
- `_pkgdown.yml` - pkgdown website configuration
- `codecov.yml` - Code coverage configuration
- `renv.lock` - Locked package versions
- `.Rbuildignore` - Files to exclude from R package build

---

## File Organization Patterns

Functions are organized by feature/domain:

- `build*.R` - Build process functions
- `init*.R` - Initialization functions
- `yml*.R` - YAML configuration handling
- `dest*.R` - Destination/deployment functions
- `git*.R` - Git integration
- `manifest*.R` - Manifest management
- `auth.R` - Authentication helpers

---

## When Making Changes

### New Functions

- Add to appropriate `R/*.R` file or create new file
- Use `.` prefix for internal functions
- Use `projr_` prefix for exported functions
- Add roxygen2 documentation for exports
- Add corresponding tests in `tests/testthat/test-*.R`

### Modifying Existing Functions

- Maintain backward compatibility when possible
- Update tests to cover new behavior
- Update roxygen2 documentation if parameters/return values change
- Run `devtools::document()` after roxygen2 changes

### Bug Fixes

- Add a test that reproduces the bug first
- Fix the bug
- Verify the test now passes
- Ensure existing tests still pass

### Documentation Changes

- Vignettes: Edit `.Rmd` files in `vignettes/`
- Function docs: Edit roxygen2 comments in `R/` files
- README: Edit `README.md` directly
- Run `devtools::document()` after roxygen2 changes
- Update copilot instructions when changes affect development practices
- Rebuild pkgdown website with `pkgdown::build_site()` when documentation changes are significant

---

## Common Issues and Solutions

### Test Failures

- Check if tests need project setup: `usethis::with_project()`
- Some tests may be skipped with `skip_if(.is_test_select())`

### Documentation Not Updating

- Run `devtools::document()` to regenerate `man/` files
- Check roxygen2 comments syntax

### Dependency Issues

- Run `renv::restore()` to sync with `renv.lock`
- Check `DESCRIPTION` for package requirements

### Check Failures

- Review output of `devtools::check()`
- Common issues: missing documentation, unused imports, test failures

---

## Additional Notes

- The package uses `renv` for reproducible dependency management
- The package has a pkgdown website (configured in `_pkgdown.yml`)
- Code coverage is tracked and should be maintained/improved
- The package integrates with multiple services: GitHub, OSF (Open Science Framework)
- Some functionality requires authentication (GitHub PAT, OSF token)
- The package supports multiple document engines (R Markdown, Quarto, Bookdown)

## GitHub Actions

The repository uses GitHub Actions for CI/CD:
- R CMD check runs on push/PR
- Test coverage is tracked with codecov
- The workflow is defined in `.github/workflows/R-CMD-check.yaml`
