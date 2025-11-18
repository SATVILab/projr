# Investigation Complete - GitHub Release Tests

## Problem Statement Analysis

You asked to investigate:
1. Why GitHub release tests don't seem to be running in CI
2. Whether they're being skipped due to auth or test mode (LITE/CRAN)
3. Why malformed GitHub URLs sometimes occur when user is missing

## Findings

### 1. Why Tests Don't Run in CI ✅

**Root Cause**: Tests are correctly skipped in LITE mode by design.

The GitHub release tests in `test-github-remote-comprehensive.R` are **comprehensive tests** that test all combinations of YML parameters:
- Structure: latest vs archive
- Send_cue: always, if-change, never  
- Send_strategy: sync-diff, sync-purge, upload-all
- Send_inspect: manifest, file, none
- Content types: raw-data, cache, output, code

According to `testing.instructions.md`, comprehensive tests should:
- Skip in LITE mode via `skip_if(.is_test_lite())`
- Skip in CRAN mode via `skip_if(.is_test_cran())`
- Run in FULL mode only

**CI Configuration**:
- Auto workflow (on every push): Sets `R_PKG_TEST_LITE: true` → Fast feedback (~2.5 min)
- Manual workflow (workflow_dispatch): No LITE flag → Full validation (~5+ min)

**This is correct behavior** - comprehensive parameter testing should not slow down every CI run.

### 2. Malformed URL Issue ✅ FIXED

**Root Cause**: `.test_skip_if_cannot_modify_github()` only checked if auth token exists, not if `gh::gh_whoami()` works.

**Scenario**: If credentials exist but `gh::gh_whoami()` fails (e.g., gh package not properly configured), tests would start running but then construct malformed URLs like `https://github.com//.git`.

**Fix**: Enhanced the skip helper to verify `gh::gh_whoami()` succeeds:

```r
# Now checks:
# 1. Token exists
# 2. Token is not GITHUB_TOKEN (prevents CI token usage)  
# 3. gh::gh_whoami() successfully retrieves username ← NEW
```

This ensures tests only run when GitHub operations will actually work.

### 3. Secondary Skip Condition

Even with LITE mode disabled, GitHub tests have additional requirements:
- `GITHUB_PAT` must be set (not just `GITHUB_TOKEN`)
- PAT must have appropriate permissions
- `gh::gh_whoami()` must work

In CI, you would need to set the `GH_TOKEN` repository secret.

## Changes Made

1. **tests/testthat/helper-github.R**: Added gh_whoami() verification
2. **.github/instructions/authentication.instructions.md**: Updated documentation
3. **GITHUB_TESTS_FINDINGS.md**: Created comprehensive analysis document
4. **.Rbuildignore**: Added findings document

## Recommendations

### For CI Testing

**Option A: Keep Current Setup (Recommended)**
- Auto CI runs LITE mode for fast feedback
- Manually trigger workflow_dispatch before releases for full tests
- Ensure `GH_TOKEN` repository secret is set

**Option B: Add Basic Integration Tests**
- Create `test-github-remote-integration.R` with 2-3 basic tests
- Don't use `skip_if(.is_test_lite())`
- Provides some GitHub coverage in every CI run
- Trade-off: Slower CI but better coverage

### For Local Development

Run full tests before committing GitHub-related changes:
```r
devtools::load_all()
# Don't call .test_set_lite() to run FULL mode
devtools::test(filter = "github")
```

Run LITE mode for quick iteration:
```r
devtools::load_all()
.test_set_lite()
devtools::test()
```

## Test Results

✅ Package builds successfully: `R CMD check` passes with 0 errors/warnings/notes
✅ Test suite passes: 1824 tests pass in LITE mode
✅ Auth tests pass: All authentication tests working
✅ Skip helper works: Correctly skips when auth unavailable or gh_whoami fails

## Next Steps

1. Review `GITHUB_TESTS_FINDINGS.md` for detailed technical analysis
2. Decide if you want to add basic integration tests (Option B above)
3. Ensure `GH_TOKEN` repository secret is set if you want manual CI to run GitHub tests
4. Consider adding reminder in CONTRIBUTING.md about running full tests before releases

---

**Summary**: The current behavior is correct. Tests are being skipped intentionally to keep CI fast. The malformed URL issue has been fixed by verifying gh_whoami() works before running tests.
