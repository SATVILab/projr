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
- **If export status changed**: Update `_pkgdown.yml` and verify with `pkgdown::check_pkgdown()`

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

### General Testing (Recommended)

Use LITE mode by default for faster testing:

```r
devtools::load_all()
.test_set_lite()
devtools::test()
```

### Debugging Specific Test Failures

When debugging a specific test issue, turn off LITE mode and use SELECT mode:

```r
devtools::load_all()
.test_unset_lite()       # Ensure relevant tests will run
.test_set_select()       # Skip most tests

# Edit test file(s): comment out skip_if(.is_test_select()) in tests you want to debug
devtools::test()         # Run only selected tests

# When done debugging:
.test_unset_select()     # Re-enable skipped tests
# Restore skip_if(.is_test_select()) lines in test files
```

### Full Mode Testing

Use FULL mode (no test mode set) only when:
- Preparing for a release or major version bump
- Explicitly requested by the issue or PR
- Working on comprehensive parameter combination testing

### Debugging Specific Test Failures

When debugging a specific test failure or issue:

1. **Turn off LITE mode** to ensure the relevant test runs:
   ```r
   devtools::load_all()
   .test_unset_lite()
   ```

2. **Use test selection** to run only specific tests:
   ```r
   .test_set_select()
   # Temporarily remove skip_if(.is_test_select()) from the test you're debugging
   devtools::test()
   # Or run specific file:
   devtools::test_file("tests/testthat/test-specific.R")
   ```

3. **After debugging**, restore skip conditions and unset select mode:
   ```r
   # Re-add skip_if(.is_test_select()) to the test
   .test_unset_select()
   ```

This approach allows you to focus on specific failing tests without running the entire suite.

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
- **Add to `_pkgdown.yml` reference index** in the appropriate section when exported

### Modifying Existing Functions

- Maintain backward compatibility when possible
- Update tests to cover new behavior
- Update roxygen2 documentation if parameters/return values change
- Run `devtools::document()` after roxygen2 changes
- **Update `_pkgdown.yml` reference index** if function export status changes (newly exported, no longer exported, or deleted)

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

### pkgdown Reference Index Maintenance

The `_pkgdown.yml` file contains the reference index for the package documentation website. **Always keep it synchronized with exported functions:**

- **When adding exported functions**: Add them to the appropriate section in `_pkgdown.yml`
- **When removing exported functions**: Remove them from `_pkgdown.yml`
- **When changing export status**: Update `_pkgdown.yml` accordingly
- Functions grouped by `@rdname` in roxygen2 should be listed together in pkgdown
- Verify with `pkgdown::check_pkgdown()` to catch missing or invalid entries
- Build the site locally with `pkgdown::build_site()` to ensure no errors

Common pkgdown reference sections in this package:
- Core workflow - Main build/init/restore functions
- Path helpers - Functions for getting project paths
- Version management - Version get/set functions
- Manifest queries - Functions for querying file changes
- YAML configuration - Functions for managing `_projr.yml`
- Profile management - Profile create/delete/get
- And others (see `_pkgdown.yml` for full list)

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
