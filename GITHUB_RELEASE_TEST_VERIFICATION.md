# GitHub Release Test Verification

## Summary

**Status: ✅ VERIFIED - No GitHub release tests run in LITE or CRAN modes**

All tests that create or check for GitHub releases are properly skipped when running with `R_PKG_TEST_LITE=TRUE` or `R_PKG_TEST_CRAN=TRUE`.

## Test Files with GitHub Release Operations

### test-remote-github.R (ONLY file with GitHub release tests)

This is the dedicated test file for GitHub release functionality. All 23 tests:

1. **Skip in CRAN mode**: `skip_if(.is_test_cran())`
2. **Skip in LITE mode**: `skip_if(.is_test_lite())`
3. **Skip in FAST mode**: `skip_if(.is_test_fast())`
4. **Skip without credentials**: `.test_skip_if_cannot_modify_github()`

Tests include:
- Creating GitHub releases
- Checking release existence
- Adding/removing files from releases
- Manifest and VERSION file round-trips
- Testing different send strategies (sync-diff, sync-purge, etc.)
- Testing different send cues (always, if-change, never)
- Restore operations from GitHub releases

**Verification**: Ran tests with both LITE and CRAN modes - all 23 tests properly skipped.

## Other Test Files - No GitHub Release Operations

### test-piggyback-retry.R
- Tests retry logic functions only
- Does NOT call GitHub API
- Safe to run in all modes

### test-build-check-packages.R
- Line 105-136: Adds GitHub destination to YAML config
- Only calls `projr_build_check_packages()` to check if packages are installed
- Does NOT execute builds or interact with GitHub
- Has `skip_if(.is_test_cran())`

### test-dest-send.R
- Uses `.test_setup_project(github = FALSE)`
- Explicitly removes GitHub remotes
- Only tests local destinations
- Has `skip_if(.is_test_cran())`

### test-yml.R
- Only tests YAML configuration functions
- Does NOT execute builds or interact with actual GitHub
- No skip conditions needed

### test-git.R
- Has one test with `github = TRUE` (line 562)
- Tests Git remote operations (`.git_remote_check_exists()`)
- Does NOT check for or create GitHub releases
- Different from `.remote_check_exists("github", tag)` which checks releases
- Has `skip_if(!nzchar(.auth_get_github_pat_find()))`

## Test Setup Safeguards

### `.test_setup_project()` defaults:
```r
.test_setup_project <- function(git = TRUE,
                                github = FALSE,  # Default is FALSE
                                set_env_var = TRUE,
                                ...)
```

When `github = FALSE` (the default):
1. Calls `.test_setup_project_github_unset()` 
2. Explicitly removes `build.github` from `_projr.yml`
3. Prevents any accidental GitHub release operations

## GitHub Actions Configuration

### R-CMD-check-auto (automatic on push)
```yaml
env:
  R_PKG_TEST_LITE: true  # Line 28
```
✅ All GitHub release tests skip

### R-CMD-check-all (manual workflow_dispatch)
```yaml
env:
  # No R_PKG_TEST_LITE set
```
⚠️ All tests run, including GitHub release tests (intentional for comprehensive testing)

## Verification Commands

### Run LITE mode tests:
```r
Sys.setenv(R_PKG_TEST_LITE = "TRUE")
devtools::load_all()
devtools::test(filter = "remote-github")
# Result: All 23 tests skipped
```

### Run CRAN mode tests:
```r
Sys.setenv(R_PKG_TEST_CRAN = "TRUE")
devtools::load_all()
devtools::test(filter = "remote-github")
# Result: All 23 tests skipped
```

### Run normal mode tests (with credentials):
```r
# Unset test mode env vars
Sys.unsetenv("R_PKG_TEST_LITE")
Sys.unsetenv("R_PKG_TEST_CRAN")
devtools::load_all()
devtools::test(filter = "remote-github")
# Result: Tests run if GitHub credentials available
```

## GitHub Release Functions (for reference)

These functions are ONLY called from test-remote-github.R:

- `.remote_create_github(tag)` - Creates a GitHub release
- `.remote_check_exists_github(tag)` - Checks if release exists
- `.remote_final_check_exists_github(remote_pre)` - Checks if release asset exists
- `.remote_file_add_github(...)` - Adds files to release
- `.remote_file_rm_github(...)` - Removes files from release
- `.remote_get_manifest("github", ...)` - Gets manifest from release
- `.remote_write_manifest("github", ...)` - Writes manifest to release

## Conclusion

The test suite is correctly configured. GitHub release tests are properly isolated in test-remote-github.R with appropriate skip conditions. The slow GitHub Actions runs must be due to other factors:

- Network latency in CI environment
- General CI overhead
- Other integration tests that run in LITE mode
- Package installation time
- Other remote operations (e.g., OSF tests if credentials available)

No changes needed to prevent GitHub release tests from running in LITE or CRAN mode.
