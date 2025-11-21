# projr Package Development Guidelines

## Purpose & Scope

Core development standards for the projr R package. This package manages reproducible research projects with version control, automated builds, and multi-destination deployment (local, GitHub, OSF).

For language-specific and topic-specific guidelines, see `.github/instructions/` directory.

---

## Topic-Specific Instructions

See detailed guidelines in `.github/instructions/`:
- `r-coding-standards.instructions.md` - R code style and patterns (applies to `**/*.R`)
- `testing.instructions.md` - Test suite guidelines (applies to `tests/**/*`)
- `package-development.instructions.md` - Development workflow
- `yaml-configuration.instructions.md` - YAML config guidelines (applies to `**/*.{yml,yaml}`)
- `build-system.instructions.md` - Build process, logging, manifest system (applies to build/manifest/hash files)
- `git-version-control.instructions.md` - Git integration and version management (applies to git files)
- `authentication.instructions.md` - Authentication for GitHub/OSF (applies to auth files)
- `remote-system.instructions.md` - Remote destinations (GitHub, OSF, local) and file operations (applies to `R/remote*.R`)

---

## Core Principles

### Code Quality

- Make minimal, surgical changes to fix issues
- Maintain backward compatibility when possible
- Follow existing patterns in the codebase
- Add tests for new functionality or bug fixes
- **Never leave trailing whitespace** at the end of lines or on blank lines
- **Always add a blank line** between headings (ending with `**`) and bullet points

### Before Committing

- Run `devtools::document()` to update `man/` and `NAMESPACE`.
- Run `devtools::test()` (LITE mode during development).
- Update `_pkgdown.yml` when adding/removing/exporting functions:
  - Add exported function names or `@rdname`s to the appropriate `reference → sections → contents` section.
  - Verify with `pkgdown::check_pkgdown()` and `pkgdown::build_site()`.
- Ensure file formatting:
  - Files must end with a single newline (empty line at end).
  - No trailing whitespace anywhere.
- Update copilot instructions in this file or relevant topics files as needed, following maintenance guidelines at the end of this document.

### Package Structure

- `R/` - Source code (use `.` prefix for internal, `projr_` for exported functions)
- `tests/testthat/` - Tests (use helper functions from `helper-*.R`)
- `man/` - Auto-generated docs (DO NOT edit directly)
- `_projr.yml` - Project configuration

---

## Advice

### Input Validation

- Use `.assert_*()` and other functions from the `R/check.R` file for input validation to fail early with clear messages.
- Validate all user inputs, including internal function calls
- Provide clear error messages for invalid inputs

### Debugging

**Log Files for Debugging:**
- Log files are **automatically created during builds** (`projr_build_*` functions) and contain all CLI output (info, debug, success, step messages)
- Log files are **NOT automatically created** outside of builds (e.g., during manual function testing)
- To enable logging outside of builds for debugging:
  1. Create a log file using `.log_build_init()`: `log_info <- .log_build_init(build_type = "dev", msg = "Manual debugging")`
  2. Get the log file path from the returned list: `log_info$log_file`
  3. All subsequent `.cli_*()` calls will write to this log file
  4. Inspect the log with `.log_file_get_most_recent()` or `projr_log_view()`
- **All `.cli_*()` functions write to the log file** (not just `.cli_debug()`):
  - `.cli_info()` - Standard messages
  - `.cli_success()` - Success messages
  - `.cli_debug()` - Debug messages (only shown in console at debug level)
  - `.cli_step()` - Step/progress messages
  - `.cli_stage_header()` - Section headers
  - `.cli_process_start()` / `.cli_process_done()` - Process status
- Use `.cli_debug()` to add lightweight debug logging (variable values, progress). Prefer committing these when they aid future debugging, but:
  - Avoid logging secrets, large binary blobs, or excessive output that clutters CI logs.
- Use `debugonce()` for short, local function-level debugging; it does not persist across sessions.
- Use `browser()` only for interactive local debugging. Guard calls to avoid CI/test hangs:
  - e.g. `if (interactive()) browser()`.
  - Always remove or guard `browser()` calls before committing.
- Use post-mortem tools for non-interactive diagnostics:
  - `traceback()`, `rlang::last_error()`, `rlang::last_trace()`.
  - Consider `options(error = rlang::entrace)` during ad-hoc debugging.
- Avoid committing interactive debug statements in code or tests; guard or remove them.
- For tests, use `skip_on_noninteractive()` or `skip_if_not(interactive())` to avoid CI failures.

### Debugging Tests

- **General test runs**: Use LITE mode (`.test_set_lite()`) for faster iteration during development
- **Debugging specific test failures**: 
  - Turn off LITE mode (`.test_unset_lite()`) to ensure relevant tests run
  - Use SELECT mode (`.test_set_select()`) to skip most tests
  - Comment out `skip_if(.is_test_select())` in only the specific tests you need to debug
  - Run `devtools::test()` to execute only selected tests
  - When done, run `.test_unset_select()` and restore `skip_if(.is_test_select())` lines
- See `testing.instructions.md` for detailed workflow and examples

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
- Restore functions: `projr_content_update()`, `projr_restore_repo()`

### Directory Licenses
- Per-directory LICENSE files for raw data, outputs, and docs
- Two approaches: YAML configuration (automatic) or manual creation
- Templates: CC-BY, CC0, Apache-2.0, MIT, Proprietary
- Functions: `projr_yml_dir_license_set()`, `projr_license_create_manual()`, `projr_yml_dir_license_update()`
- LICENSE files are tracked in manifest for versioning

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

# Set directory license (automatic approach)
projr_yml_dir_license_set("CC-BY", "output")
projr_yml_dir_license_set("MIT", "raw-data", authors = c("Author Name"))
```

### Directory Licenses
```r
# Automatic approach - managed in YAML, regenerated during builds
projr_yml_dir_license_set("CC-BY", "output")

# Manual approach - created once, preserved across builds
projr_license_create_manual("MIT", "raw-data")

# Update all YAML-configured licenses with DESCRIPTION authors
projr_yml_dir_license_update()

# Get/remove license configuration
projr_yml_dir_license_get("output")
projr_yml_dir_license_rm("output")
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

### Directory Licensing Approaches
- **YAML Configuration**: Licenses managed in `_projr.yml`, regenerated during builds
  - Only created/updated if YAML config exists
  - Ensures consistency with project metadata
  - Good for outputs that regenerate each build
- **Manual Creation**: Licenses created with `projr_license_create_manual()`
  - NOT in YAML configuration
  - Never overwritten during builds (unless YAML config added)
  - Allows manual editing and customization
  - Good for raw data with complex licensing needs
- Both approaches can coexist; YAML takes precedence when specified
- LICENSE files are tracked in manifest for versioning

---

## Maintaining These Instructions

When updating copilot instructions, follow GitHub's best practices:

- **Keep it concise** - Files under 1000 lines (ideally under 250)
- **Structure matters** - Use headings, bullets, clear sections
- **Be direct** - Short, imperative rules over long paragraphs
- **Show examples** - Include code samples (correct and incorrect patterns)
- **No external links** - Copilot won't follow them; copy info instead
- **No vague language** - Avoid "be more accurate", "identify all issues", etc.
- **Path-specific** - Use `applyTo` frontmatter in topic files
- **Review regularly** - Update as package evolves.

See `.github/instructions/README.md` for detailed maintenance guidelines.

---

## Resources

- pkgdown config: `_pkgdown.yml`
- CI/CD: `.github/workflows/R-CMD-check.yaml`
- Package documentation: Run `pkgdown::build_site()` to build locally
