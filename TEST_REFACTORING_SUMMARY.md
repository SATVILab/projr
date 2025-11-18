# Test Suite Refactoring Summary

## Overview

This document summarizes the refactoring of GitHub and OSF remote tests to improve test organization, reduce redundant remote operations, and make tests more maintainable.

## Changes Made

### 1. New Test Files Created

#### `tests/testthat/test-remote-github.R`

- **Purpose**: Dedicated file for GitHub release-specific tests
- **Key Features**:
  - Creates and reuses two fixed GitHub releases (`projr-test-release-a` and `projr-test-release-b`)
  - Tests are idempotent - releases may already exist from previous runs
  - Tests basic GitHub remote operations:
    - `.remote_create()`, `.remote_check_exists()`, `.remote_get()`, `.remote_get_final()`
    - File operations: `.remote_file_add()`, `.remote_file_ls()`, `.remote_file_rm()`, `.remote_file_rm_all()`
    - Manifest round-trip: `.remote_write_manifest()`, `.remote_get_manifest()`
    - VERSION file round-trip: `.remote_write_version_file()`, `.remote_get_version_file()`
    - Restore integration: `projr_restore()` with GitHub sources
- **Skip Conditions**:
  - `skip_if(.is_test_cran())`
  - `skip_if(.is_test_lite())`
  - `skip_if(.is_test_fast())`
  - `skip_if(.is_test_select())`
  - `skip_if_offline()`
  - `.test_skip_if_cannot_modify_github()`
- **Test Count**: 12 tests (all skip when GitHub credentials unavailable)

#### `tests/testthat/test-remote-osf.R`

- **Purpose**: Dedicated file for OSF node-specific tests
- **Key Features**:
  - Tests basic OSF remote operations:
    - `.remote_create()`, `.remote_check_exists()`, `.remote_get()`, `.remote_get_final()`
    - File operations: `.remote_file_add()`, `.remote_file_ls()`, `.remote_file_rm()`, `.remote_file_rm_all()`
    - Empty remote removal: `.remote_rm_final_if_empty()`
- **Skip Conditions**:
  - `skip_if(.is_test_cran())`
  - `skip_if(.is_test_lite())`
  - `skip_if(.is_test_fast())`
  - `skip_if(.is_test_select())`
  - `skip_if_offline()`
  - `skip_if(!nzchar(Sys.getenv("OSF_PAT")))`
- **Test Count**: 6 tests (all skip when OSF credentials unavailable)

### 2. Updated Test Files

#### `tests/testthat/test-remote.R`

- **Before**: Mixed local, GitHub, and OSF remote tests
- **After**: Local-only remote tests
- **Changes**:
  - Removed all GitHub-specific tests (moved to `test-remote-github.R`)
  - Removed all OSF-specific tests (moved to `test-remote-osf.R`)
  - Added documentation header explaining the new structure
- **Test Count**: 19 tests (all pass without external dependencies)

#### `tests/testthat/test-github-remote-comprehensive.R`
- **Status**: No changes needed
- **Purpose**: Tests full build cycles with different YML parameter combinations
- **Verification**: Already has proper skip conditions
- **Note**: Kept separate from `test-remote-github.R` because it tests full build workflows rather than basic remote operations

#### `tests/testthat/test-github-release-preparation.R`
- **Status**: No changes needed
- **Verification**: Already has proper skip conditions

### 3. Files Verified (No Changes Needed)

The following files were audited and confirmed to be local-only or have proper skip conditions:


- `tests/testthat/test-dest-send.R` - Local-only destination send tests
- `tests/testthat/test-build-post-comprehensive.R` - No remote creation
- `tests/testthat/test-integration-comprehensive.R` - No remote creation
- `tests/testthat/test-restore-integration.R` - Already local-only

## Benefits

### 1. Faster Test Execution


- Local tests run without waiting for GitHub/OSF API responses
- Reduced repeated creation of GitHub releases
- Fixed releases can be reused across test runs

### 2. Better Test Organization

- Clear separation of concerns:
  - Local-only tests in `test-remote.R`
  - GitHub-specific tests in `test-remote-github.R`
  - OSF-specific tests in `test-remote-osf.R`
  - Full build workflows in `test-github-remote-comprehensive.R`

### 3. Improved Reliability
- All remote-dependent tests properly skip in CRAN and LITE modes
- Consistent skip conditions across all remote tests
- No accidental execution of remote tests without credentials

### 4. Easier Maintenance
- GitHub tests centralized in one file
- OSF tests centralized in one file
- Clear documentation in each file about its purpose
- Easy to identify which tests require credentials

## Test Results

### Full Test Suite (LITE Mode)
```
[ FAIL 0 | WARN 0 | SKIP 122 | PASS 1844 ]
```

### Remote Tests Only
```
[ FAIL 0 | WARN 0 | SKIP 37 | PASS 66 ]
```
- 19 local tests (all pass)
- 12 GitHub tests (skip without credentials)
- 6 OSF tests (skip without credentials)
- 29 comprehensive tests (various skip conditions)

### Local-Only Remote Tests
```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 19 ]
```

### GitHub Remote Tests
```
[ FAIL 0 | WARN 0 | SKIP 12 | PASS 0 ]
```
(All skip without GitHub credentials)

### OSF Remote Tests
```
[ FAIL 0 | WARN 0 | SKIP 6 | PASS 0 ]
```
(All skip without OSF credentials)

## Future Considerations

### Reusable GitHub Releases
The two fixed GitHub releases (`projr-test-release-a` and `projr-test-release-b`) are designed to be reused:
- Tests check if releases exist before creating them
- Tests are idempotent (can run multiple times without issues)
- Assets may accumulate over time from different test runs
- Consider periodic cleanup of test releases if needed

### OSF Node Cleanup
OSF tests use the existing cleanup infrastructure:
- `.osf_rm_node_id_defer()` ensures nodes are deleted after tests
- Temporary OSF nodes are created for each test run
- No persistent OSF nodes are maintained

## Migration Guide

If you have custom tests that interact with GitHub or OSF:

1. **GitHub Tests**: Add to `test-remote-github.R` or `test-github-remote-comprehensive.R`
   - Use `test-remote-github.R` for basic remote operation tests
   - Use `test-github-remote-comprehensive.R` for full build workflow tests
   - Always include all required skip conditions

2. **OSF Tests**: Add to `test-remote-osf.R`
   - Always include all required skip conditions
   - Use `.osf_rm_node_id_defer()` for cleanup

3. **Local Tests**: Add to `test-remote.R`
   - No skip conditions needed
   - Should run in all test modes

## Summary

This refactoring successfully isolates GitHub and OSF remote tests into dedicated files, maintains backward compatibility, and improves test organization. All existing tests pass, and the new structure makes it easier to maintain and extend the test suite.
