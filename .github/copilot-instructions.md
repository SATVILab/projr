# projr Package - General Guidelines

## About projr

An R package for reproducible and archived research projects. Manages project structure, build processes, versioning, and deployment to multiple destinations (local, GitHub, OSF).

---

## Quick Reference

For detailed guidelines, see topic-specific instruction files in `.github/instructions/`:
- `r-coding-standards.instructions.md` - R code style and patterns
- `testing.instructions.md` - Test suite guidelines
- `package-development.instructions.md` - Development workflow
- `yaml-configuration.instructions.md` - YAML config guidelines

---

## Core Principles

### Code Quality
- Make minimal, surgical changes to fix issues
- Maintain backward compatibility when possible
- Follow existing patterns in the codebase
- Add tests for new functionality or bug fixes

### Before Committing
- Run `devtools::document()` to update documentation
- Run `devtools::test()` with LITE mode for faster iteration
- Run `devtools::check()` to ensure package passes R CMD check

### Package Structure
- `R/` - Source code (use `.` prefix for internal, `projr_` for exported functions)
- `tests/testthat/` - Tests (use helper functions from `helper-*.R`)
- `man/` - Auto-generated docs (DO NOT edit directly)
- `_projr.yml` - Project configuration

---

## Key Systems

### Version Management
- Versions follow format in `metadata.version-format` (default: `major.minor.patch-dev`)
- Functions: `projr_version_get()`, `projr_version_set()`

### Build System
- Production builds: `projr_build_patch()`, `projr_build_minor()`, `projr_build_major()`
- Development builds: `projr_build_dev()`
- Environment variables: `PROJR_OUTPUT_LEVEL`, `PROJR_CLEAR_OUTPUT`, `PROJR_LOG_DETAILED`

### Manifest System
- Tracks file hashes across versions in `manifest.csv`
- Query functions: `projr_manifest_changes()`, `projr_manifest_range()`, `projr_manifest_last_change()`

### Git Integration
- Auto-commits and pushes based on `build.git` settings in `_projr.yml`
- Works with both Git CLI and `gert` R package

### Remote Destinations
- Local, GitHub, OSF destinations supported
- Restore functions: `projr_restore()`, `projr_restore_repo()`

---

## Authentication

### GitHub
- Set `GITHUB_PAT` environment variable
- All `gh::` or `gitcreds::` calls must have `.auth_check_github()` before use
- Instructions: `projr_instr_auth_github()`

### OSF
- Set `OSF_PAT` environment variable  
- All `osfr::` calls must have `.auth_check_osf()` before use
- Instructions: `projr_instr_auth_osf()`

---

## Environment Variables

### Build Control
- `PROJR_OUTPUT_LEVEL` - Console verbosity: `"none"`, `"std"`, `"debug"` (default: `"none"` for dev, `"std"` for output)
- `PROJR_CLEAR_OUTPUT` - When to clear output: `"pre"`, `"post"`, `"never"` (default: `"pre"`)
- `PROJR_LOG_DETAILED` - Create detailed log files: `TRUE`/`FALSE` (default: `"TRUE"`)

### Testing Control
- `R_PKG_TEST_LITE` - Enable LITE test mode (skip comprehensive tests)
- `R_PKG_TEST_CRAN` - Enable CRAN test mode (skip slow/integration tests)
- `R_PKG_TEST_SELECT` - Skip most tests (for targeted testing)

### Environment Files
- `_environment.local` - Machine-specific (git-ignored, highest precedence)
- `_environment-<profile>` - Profile-specific variables
- `_environment` - Global defaults (lowest precedence)

---

## Common Patterns

### YAML Configuration
```r
# Get directory path
projr_path_get_dir("output", safe = TRUE)

# Set Git options
projr_yml_git_set(commit = TRUE, push = TRUE, add_untracked = TRUE)

# Add build hook
projr_yml_hooks_add(path = "setup.R", stage = "pre")
```

### Input Validation
```r
.my_function <- function(param) {
  .assert_string(param, required = TRUE)
  # implementation
}
```

### Error Handling
```r
if (!file.exists(path)) {
  stop("File not found: ", path)
}
```

---

## Key Concepts

### Safe vs Unsafe Directories
- **Safe** (`safe = TRUE`): Build directory in cache (e.g., `_tmp/projr/v0.0.1/output`)
- **Unsafe** (`safe = FALSE`): Actual directory (e.g., `_output`)
- Applies to `output`, `docs`, `data` directories

### Test Modes
- **LITE**: Fast, core tests (~364 tests, ~2.5 min) - recommended for development
- **FULL**: All tests including comprehensive (~452 tests, ~5+ min) - for releases
- **CRAN**: Minimal tests for CRAN submission (<2 min)

### Directory Labels
- `raw-data`, `cache`, `output`, `docs`, `project`, `code`, `data`
- Labels are case-insensitive and ignore hyphens/underscores

---

## Additional Resources

- Package website: https://satvilab.github.io/projr/
- pkgdown config: `_pkgdown.yml`
- CI/CD: `.github/workflows/R-CMD-check.yaml`
