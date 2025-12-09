# projr Package Development Guidelines

## Purpose & Scope

Core development standards for the projr R package. This package manages
reproducible research projects with version control, automated builds,
and multi-destination deployment (local, GitHub, OSF).

For language-specific and topic-specific guidelines, see
`.github/instructions/` directory.

------------------------------------------------------------------------

## Topic-Specific Instructions

See detailed guidelines in `.github/instructions/`: -
`r-coding-standards.instructions.md` - R code style and patterns
(applies to `**/*.R`) - `testing.instructions.md` - Test suite
guidelines (applies to `tests/**/*`) -
`package-development.instructions.md` - Development workflow -
`yaml-configuration.instructions.md` - YAML config guidelines (applies
to `**/*.{yml,yaml}`) - `build-system.instructions.md` - Build process,
logging, manifest system (applies to build/manifest/hash files) -
`git-version-control.instructions.md` - Git integration and version
management (applies to git files) - `authentication.instructions.md` -
Authentication for GitHub/OSF (applies to auth files) -
`remote-system.instructions.md` - Remote destinations (GitHub, OSF,
local) and file operations (applies to `R/remote*.R`)

------------------------------------------------------------------------

## Core Principles

- Make minimal, surgical changes
- Maintain backward compatibility
- Follow existing patterns
- Add tests for new functionality/bug fixes
- Never leave trailing whitespace
- Always add blank line between headings and bullets

**Before committing:** - Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html),
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
(LITE mode), `styler::style_pkg()` - Update `_pkgdown.yml` when
adding/removing exported functions - Verify with
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html) -
Ensure files end with single newline, no trailing whitespace - Update
copilot instructions as needed

**Package structure:** - `R/` - Source (`.` prefix internal, `projr_`
exported) - `tests/testthat/` - Tests (use `helper-*.R` functions) -
`man/` - Auto-generated docs (DO NOT edit) - `_projr.yml` - Project
configuration

------------------------------------------------------------------------

## Development Practices

**Input validation:** - Use `.assert_*()` functions from `R/check.R` -
Validate all user inputs and internal calls - Provide clear error
messages

**Logging:** - Log files auto-created during builds (`projr_build_*`) -
Outside builds: Use
[`.log_build_init()`](https://satvilab.github.io/projr/reference/dot-log_build_init.md)
to enable logging - All `.cli_*()` functions write to log:
[`.cli_info()`](https://satvilab.github.io/projr/reference/dot-cli_info.md),
[`.cli_success()`](https://satvilab.github.io/projr/reference/dot-cli_success.md),
[`.cli_debug()`](https://satvilab.github.io/projr/reference/dot-cli_debug.md),
[`.cli_step()`](https://satvilab.github.io/projr/reference/dot-cli_step.md) -
Commit useful
[`.cli_debug()`](https://satvilab.github.io/projr/reference/dot-cli_debug.md)
calls; avoid logging secrets or large output

**Interactive debugging:** - Use
[`debugonce()`](https://rdrr.io/r/base/debug.html) for function-level
debugging - Guard [`browser()`](https://rdrr.io/r/base/browser.html)
calls: `if (interactive()) browser()` - Use
[`traceback()`](https://rdrr.io/r/base/traceback.html),
[`rlang::last_error()`](https://rlang.r-lib.org/reference/last_error.html),
[`rlang::last_trace()`](https://rlang.r-lib.org/reference/last_error.html)
for diagnostics - Remove/guard interactive statements before committing

**Test debugging:** - General runs: Use LITE mode (`.test_set_lite()`) -
Specific failures: Turn off LITE (`.test_unset_lite()`), use SELECT
(`.test_set_select()`), comment out `skip_if(.is_test_select())` in
target tests - See `testing.instructions.md` for details

------------------------------------------------------------------------

## Key Systems

**Version management:** `major.minor.patch-dev` format;
[`projr_version_get()`](https://satvilab.github.io/projr/reference/projr_version_get.md),
[`projr_version_set()`](https://satvilab.github.io/projr/reference/projr_version_set.md)

**Build system:** Production (`projr_build_patch/minor/major()`), dev
([`projr_build_dev()`](https://satvilab.github.io/projr/reference/projr_build_dev.md));
env vars: `PROJR_OUTPUT_LEVEL`, `PROJR_CLEAR_OUTPUT`,
`PROJR_LOG_DETAILED`

**Manifest:** Tracks file hashes in `manifest.csv`; query:
[`projr_manifest_changes()`](https://satvilab.github.io/projr/reference/projr_manifest_query.md),
[`projr_manifest_range()`](https://satvilab.github.io/projr/reference/projr_manifest_query.md),
[`projr_manifest_last_change()`](https://satvilab.github.io/projr/reference/projr_manifest_query.md)

**Git:** Auto-commits/pushes via `build.git` settings; uses Git CLI or
`gert`

**Remotes:** Local, GitHub, OSF destinations; restore:
[`projr_content_update()`](https://satvilab.github.io/projr/reference/projr_restore.md),
[`projr_restore_repo()`](https://satvilab.github.io/projr/reference/projr_restore.md)

**Directory licenses:** Per-directory LICENSE files; YAML
(auto-regenerated) or manual
([`projr_license_create_manual()`](https://satvilab.github.io/projr/reference/projr_license_create_manual.md));
templates: CC-BY, CC0, Apache-2.0, MIT, Proprietary

------------------------------------------------------------------------

## Authentication

**GitHub:** Set `GITHUB_PAT`; use
[`.auth_check_github()`](https://satvilab.github.io/projr/reference/dot-auth_check_github.md)
before `gh::`/`gitcreds::` calls; see
[`projr_instr_auth_github()`](https://satvilab.github.io/projr/reference/instr_auth.md)

**OSF:** Set `OSF_PAT`; use
[`.auth_check_osf()`](https://satvilab.github.io/projr/reference/dot-auth_check_osf.md)
before `osfr::` calls; see
[`projr_instr_auth_osf()`](https://satvilab.github.io/projr/reference/instr_auth.md)

------------------------------------------------------------------------

## Environment Variables

**Build:** `PROJR_OUTPUT_LEVEL` (none/std/debug), `PROJR_CLEAR_OUTPUT`
(pre/post/never), `PROJR_LOG_DETAILED` (TRUE/FALSE)

**Testing:** `R_PKG_TEST_LITE` (skip comprehensive), `R_PKG_TEST_CRAN`
(skip slow), `R_PKG_TEST_SELECT` (targeted)

**Files:** `_environment.local` (git-ignored, highest),
`_environment-<profile>` (profile), `_environment` (global, lowest)

------------------------------------------------------------------------

## Common Patterns

### YAML Configuration

``` r
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

``` r
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

``` r
.my_function <- function(param) {
  .assert_string(param, required = TRUE)
  # implementation
}
```

### Error Handling

``` r
if (!file.exists(path)) {
  stop("File not found: ", path)
}
```

------------------------------------------------------------------------

## Key Concepts

**Safe vs Unsafe directories:** - Safe (`safe = TRUE`): Cache build dir
(e.g., `_tmp/projr/v0.0.1/output`) - Unsafe (`safe = FALSE`): Actual dir
(e.g., `_output`)

**Test modes:** - LITE: Core tests (~364, ~2.5 min) - for development -
FULL: All tests (~452, ~5+ min) - for releases - CRAN: Minimal (\<2
min) - for submission

**Directory labels:** `raw-data`, `cache`, `output`, `docs`, `project`,
`code`, `data` (case-insensitive, ignore hyphens/underscores)

**Directory licensing:** - YAML: Managed in `_projr.yml`,
auto-regenerated (good for outputs) - Manual:
[`projr_license_create_manual()`](https://satvilab.github.io/projr/reference/projr_license_create_manual.md),
never overwritten (good for raw data) - Both coexist; YAML takes
precedence

------------------------------------------------------------------------

## Maintaining Instructions

Follow GitHub best practices: - Keep concise (under 250 lines ideal, max
1000) - Use clear structure: headings, bullets, sections - Be direct:
short, imperative rules - Show examples: code samples
(correct/incorrect) - No external links (copy info instead) - No vague
language (“be more accurate”, etc.) - Use `applyTo` frontmatter in topic
files - Review regularly

See `.github/instructions/README.md` for details.

------------------------------------------------------------------------

## Resources

- pkgdown config: `_pkgdown.yml`
- CI/CD: `.github/workflows/R-CMD-check.yaml`
- Package documentation: Run
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  to build locally
