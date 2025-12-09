---
applyTo: "tests/**/*"
---

# Testing Guidelines for projr

## Purpose & Scope

Testing standards and patterns for the projr package test suite, including test modes, helper functions, and common patterns.

---

## Test Suite Levels

### CRAN Mode
- Runs only fast, essential tests (~364 tests, <2 minutes)
- Skips comprehensive, integration, and remote-dependent tests
- Use `skip_if(.is_test_cran())` to skip tests in CRAN mode

### Lite Mode (Recommended for Development)
- Runs core functionality tests (~364 tests, ~2.5 minutes)
- Skips comprehensive tests (exhaustive parameter combinations)
- Includes integration and remote tests if credentials available
- Enable with: `devtools::load_all(); .test_set_lite(); devtools::test()`
- Use `skip_if(.is_test_lite())` to skip comprehensive tests

### Full Mode
- Runs all tests (452 tests, ~5+ minutes)
- Use for pre-release validation or comprehensive parameter testing
- Enable by running tests without calling `.test_set_lite()` or `.test_set_cran()`

### Select Mode (For Debugging Specific Issues)
- Skips all tests that have `skip_if(.is_test_select())` guard
- Used to run only specific tests when debugging a particular issue
- Enable with: `devtools::load_all(); .test_set_select(); devtools::test()`
- Disable with: `.test_unset_select()`
- Most tests include `skip_if(.is_test_select())` to allow selective debugging
- When debugging, comment out `skip_if(.is_test_select())` in only the tests you want to run

---

## Debugging Specific Tests

When debugging a specific issue, use the test selection approach:

### Debugging Workflow

1. **Turn off LITE mode** to ensure all relevant tests run:
   ```r
   devtools::load_all()
   .test_unset_lite()  # Disable LITE mode
   ```

2. **Enable test selection** to skip most tests:
   ```r
   .test_set_select()
   ```

3. **Temporarily remove** `skip_if(.is_test_select())` from the specific test(s) you want to debug

4. **Run tests** - only tests without `skip_if(.is_test_select())` will run:
   ```r
   devtools::test()
   # Or run specific test file:
   devtools::test_file("tests/testthat/test-manifest.R")
   ```

5. **After debugging**, restore the skip condition and unset select mode:
   ```r
   # Re-add skip_if(.is_test_select()) to the test
   .test_unset_select()
   ```

### When to Use Each Mode

- **LITE mode (default)**: General development, quick validation of changes
- **FULL mode**: Pre-release testing, comprehensive parameter testing
- **SELECT mode**: Debugging specific failing tests, investigating issues in particular test cases

---

## Test Guidelines

- Use testthat 3e (Config/testthat/edition: 3)
- Test file naming: `test-{feature}.R` (e.g., `test-manifest.R`)
- Use `test_that()` for each test case with descriptive names
- Use `usethis::with_project()` for tests that need a temporary project environment
- Test edge cases: empty directories, missing files, NULL values

## Debugging with Selective Test Running

When debugging specific test failures or issues:

1. **Turn off LITE mode** - Run `.test_unset_lite()` to ensure the relevant tests will run
2. **Enable SELECT mode** - Run `.test_set_select()` to skip most tests
3. **Select tests to run** - In the test files, comment out `skip_if(.is_test_select())` for ONLY the tests you want to debug
4. **Run tests** - Run `devtools::test()` to execute only your selected tests
5. **Clean up** - When done debugging, uncomment the `skip_if(.is_test_select())` lines and run `.test_unset_select()`

### Workflow Example

```r
# 1. Load package and disable LITE mode
devtools::load_all()
.test_unset_lite()  # Ensure comprehensive tests can run if needed

# 2. Enable SELECT mode
.test_set_select()

# 3. Edit test file - comment out skip_if(.is_test_select()) in specific tests
# In tests/testthat/test-manifest.R:
#   test_that("manifest tracking works", {
#     skip_if(.is_test_select())  <- Comment this out
#     # ... test code ...
#   })

# 4. Run tests - only selected tests will run
devtools::test()

# 5. Clean up when done
.test_unset_select()
# Uncomment skip_if(.is_test_select()) in test files
```

### When to Use Each Mode

- **General test runs during development**: Use LITE mode (`.test_set_lite()`)
- **Debugging specific failing tests**: Turn off LITE (`.test_unset_lite()`), use SELECT mode (`.test_set_select()`), and narrow down with `skip_if(.is_test_select())`
- **Pre-release validation**: Use FULL mode (no test mode set)
- **CRAN submission**: Use CRAN mode (`.test_set_cran()`)

## When Adding New Tests

### Comprehensive Tests
- Add to files named `test-*-comprehensive.R`
- Add `skip_if(.is_test_cran())` AND `skip_if(.is_test_lite())`
- Test all combinations of YML parameters

### Integration Tests
- Add to files named `test-*-integration.R`
- Add `skip_if(.is_test_cran())` only
- Run in lite mode to catch integration issues

### Regular Tests
- No special skip conditions needed
- Should run in all modes
- Focus on essential functionality

### Remote-Dependent Tests
- Add `skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))` or equivalent
- Add `skip_if(.is_test_cran())` for CRAN compatibility

---

## Test Helper Functions

Located in `tests/testthat/helper-*.R`:

- `.test_setup_project()` - Creates complete test project with git, files, configuration
- `.init()` - Minimal initialization (creates directories, VERSION, _projr.yml)
- `.init_full()` - Full initialization for tests
- `.test_content_setup_label()` - Creates test content in project directories
- `.test_setup_project_lit_docs()` - Sets up document engine files (quarto, rmarkdown)

Test helper functions belong in `tests/testthat/helper-*.R`, NOT in `R/` files.

## Common Expect Functions

- `expect_identical()` for exact matches
- `expect_true()` / `expect_false()` for logical values
- `expect_error()` for error conditions

---

## Code Examples

```r
# Correct: Standard test pattern
test_that(".build_manifest_* works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 0L)

      label_vec <- c("cache", "raw-data")
      invisible(.test_content_setup_label(label_vec, safe = TRUE))
      path_manifest <- .build_manifest_pre(TRUE)
      manifest <- .manifest_read(path_manifest)
      expect_identical(nrow(manifest), 3L)
    }
  )
})

# Correct: Comprehensive test with proper skip conditions
test_that("local remote with archive + sync-diff", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)
  # test implementation
})

# Correct: Remote-dependent test
test_that("git function works with GitHub", {
  skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))
  skip_if(.is_test_cran())
  # test implementation
})

# Incorrect: Missing skip conditions for comprehensive test
test_that("tests all parameter combinations", {
  # Missing skip_if(.is_test_lite())
  # This will slow down development testing
})
```

---

## Testing Local Remotes Comprehensively

When testing local remote functionality, test combinations of YML parameters:

### Structure Options
- `structure = "latest"` - Overwrites files at destination
- `structure = "archive"` - Creates versioned subdirectories

### Send Cue Options
- `send_cue = "always"` - Always creates new remote version
- `send_cue = "if-change"` - Only creates version when content changes
- `send_cue = "never"` - Never sends to remote

### Send Strategy Options
- `send_strategy = "sync-diff"` - Syncs only changed files
- `send_strategy = "sync-purge"` - Removes all files then uploads all
- `send_strategy = "upload-all"` - Uploads all files (may overwrite)
- `send_strategy = "upload-missing"` - Only uploads files not present on remote

### Send Inspect Options
- `send_inspect = "manifest"` - Uses manifest.csv to track versions
- `send_inspect = "file"` - Inspects actual files on remote
- `send_inspect = "none"` - Treats remote as empty

## Git Testing Notes

- Use `.test_setup_project_git_config()` to set test user credentials
- Git operations using `_git` variants may produce warnings for deleted files - use `suppressWarnings()` where appropriate
- Remote operations require GitHub authentication - use `skip_if_not(nzchar(Sys.getenv("GITHUB_PAT")))`
- `.git_changed_filter()` returns `fs_path` class, not plain character - use `as.character()` or `expect_length()` in tests
