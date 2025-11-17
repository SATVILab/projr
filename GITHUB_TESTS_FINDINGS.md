# GitHub Release Tests - Investigation Findings

## Summary

This document explains why GitHub release tests don't run in CI by default and provides recommendations.

## Current Situation

### Test File: `test-github-remote-comprehensive.R`

- **Type**: Comprehensive tests (tests all combinations of YML parameters)
- **Skip Conditions**: 
  - `skip_if(.is_test_cran())` - Skips in CRAN mode
  - `skip_if(.is_test_lite())` - Skips in LITE mode ← **KEY SKIP**
  - `skip_if_offline()` - Skips if no network
  - `.test_skip_if_cannot_modify_github()` - Skips if cannot modify GitHub repos
  
### CI Workflow Configuration

The `.github/workflows/R-CMD-check.yaml` has two jobs:

1. **R-CMD-check-auto** (runs on every push)
   - Sets `R_PKG_TEST_LITE: true`
   - Only tests Ubuntu + R release
   - Fast feedback (~2.5 minutes)
   
2. **R-CMD-check-all** (manual trigger only)
   - Does NOT set `R_PKG_TEST_LITE`
   - Tests multiple OS and R versions
   - Comprehensive validation

## Why GitHub Tests Don't Run in CI

### Primary Reason: LITE Mode

The automatic CI workflow (triggered on every push) sets `R_PKG_TEST_LITE: true`, which causes all tests with `skip_if(.is_test_lite())` to be skipped. This is **by design** and follows the testing guidelines:

- **Comprehensive tests** should skip in LITE mode
- **Integration tests** should run in LITE mode
- LITE mode runs ~364 tests in ~2.5 minutes
- FULL mode runs 452 tests in ~5+ minutes

### Secondary Reason: GitHub Authentication

Even if LITE mode is disabled, GitHub tests have an additional skip condition via `.test_skip_if_cannot_modify_github()`:

1. Checks that `GITHUB_PAT` (not `GITHUB_TOKEN`) is available
2. Verifies the PAT is NOT the same as `GITHUB_TOKEN`
3. Verifies `gh::gh_whoami()` can retrieve the username

The CI workflows provide:
- `GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}` - Auto-generated, limited permissions
- `GITHUB_PAT: ${{ secrets.GH_TOKEN }}` - User token (if configured)

**If `GH_TOKEN` secret is not set**, tests will skip even in FULL mode.

## Bug Discovery Issue

The user reports: "tests get buggy locally but not here" (CI).

This happens because:
1. **Locally**: Running `devtools::test()` without `.test_set_lite()` runs FULL mode
2. **In CI**: Automatic workflow always runs LITE mode
3. **Result**: Bugs in comprehensive GitHub tests are found locally but not in CI

## Solutions

### Option 1: Manual CI Trigger for Full Tests (Current Best Practice)

Keep current setup, but ensure comprehensive tests run before releases:

1. Manually trigger `workflow_dispatch` to run full test suite
2. Set `GH_TOKEN` secret in repository settings to enable GitHub tests
3. Use this for:
   - Pre-release validation
   - When debugging GitHub-specific issues
   - Weekly/monthly comprehensive validation

**Pros**: Fast CI feedback on every push, comprehensive validation on demand
**Cons**: Requires manual trigger, may forget to run before releases

### Option 2: Create Basic GitHub Integration Tests

Create a new `test-github-remote-integration.R` file with:
- Basic smoke tests (not all parameter combinations)
- No `skip_if(.is_test_lite())`
- Runs in LITE mode to catch major regressions

**Pros**: Some GitHub testing on every push
**Cons**: Requires additional test file, duplicates some test logic, slower CI

### Option 3: Always Run Full Tests in CI

Remove `R_PKG_TEST_LITE: true` from auto workflow.

**Pros**: Catches all bugs immediately
**Cons**: Doubles CI time (~5+ minutes), burns more CI minutes

## Recommendations

### Immediate Actions

1. ✅ **DONE**: Enhanced `.test_skip_if_cannot_modify_github()` to verify `gh::gh_whoami()` works
   - Prevents malformed GitHub URLs when auth exists but gh_whoami fails
   
2. **Document**: Add workflow documentation explaining when to use manual triggers

3. **Secret**: Ensure `GH_TOKEN` secret is set in repository settings
   - Navigate to: Settings → Secrets and variables → Actions
   - Add `GH_TOKEN` with a personal access token that has `repo` scope

### Long-Term Options

**Recommended: Option 1 + Documentation**

Keep current setup but improve awareness:
- Add a reminder in CONTRIBUTING.md to run full tests before releases
- Consider adding a GitHub Action that reminds developers to run full tests
- Use pre-commit hooks locally to encourage running lite tests

**Alternative: Option 2 (Basic Integration Tests)**

If GitHub functionality breaks frequently:
- Create `test-github-remote-integration.R` 
- Include 2-3 basic tests covering:
  - Create release with structure='latest'
  - Restore from release
  - Verify @version tag works
- These run in LITE mode on every push

## Technical Details

### Test Mode Levels

| Mode | Tests Run | Time | When to Use |
|------|-----------|------|-------------|
| CRAN | ~364 essential | <2 min | CRAN submission prep |
| LITE | ~364 core + integration | ~2.5 min | **Development (recommended)** |
| FULL | 452 all tests | ~5+ min | Pre-release, debugging |

### Skip Condition Helpers

- `.is_test_cran()` - True when `R_PKG_TEST_CRAN=TRUE` or `NOT_CRAN` not set
- `.is_test_lite()` - True when `R_PKG_TEST_LITE=TRUE`
- `.test_skip_if_cannot_modify_github()` - Checks auth and gh_whoami()

### Auth Token Priority

`.auth_get_github_pat_find()` searches in order:
1. `GITHUB_PAT` environment variable (highest priority)
2. `gh::gh_token()` (may check gitcreds, etc.)
3. `gitcreds::gitcreds_get()` (fallback)
4. `GITHUB_TOKEN` environment variable (lowest priority)

This ensures user-specific tokens override system tokens.

## Conclusion

The current setup is **correct and follows best practices**:
- Comprehensive tests skip in LITE mode (as designed)
- CI runs LITE mode for fast feedback
- Full tests available via manual trigger

The "bug" is actually expected behavior. To catch GitHub test bugs in CI:
1. Set `GH_TOKEN` repository secret
2. Manually trigger workflow_dispatch before releases
3. Or create basic integration tests (Option 2)
