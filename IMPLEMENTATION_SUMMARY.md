# GitHub Release Handling Implementation Summary

## Overview

This PR implements improvements to `projr`'s GitHub release handling, authentication, and interaction with `piggyback`, focusing on predictable, fast behavior and enterprise GitHub support.

## Changes Implemented

### 1. Authentication Fixes (R/auth.R)

#### Fixed Bug in `.auth_get_github_pat_find()`
- **Issue**: The function called `.auth_get_github_pat_find_gitcreds()` but never returned its value
- **Fix**: Properly check and return the gitcreds token if non-empty
- **Impact**: gitcreds-stored tokens now work correctly

#### Added `GH_TOKEN` Fallback
- **New**: `GH_TOKEN` environment variable now serves as final fallback
- **Order**: GITHUB_PAT → gh::gh_token() → gitcreds → GITHUB_TOKEN → GH_TOKEN
- **Benefit**: More flexible authentication options, especially in CI environments

### 2. New GitHub API Helpers (R/auth.R)

#### `.github_api_base(api_url = NULL)`
- Resolves GitHub API base URL
- Priority: explicit arg → GITHUB_API_URL env var → https://api.github.com
- Removes trailing slashes
- **Benefit**: Enterprise GitHub support via GITHUB_API_URL

#### `.release_exists(repo, tag, api_url = NULL, token = NULL)`
- Fast boolean check for release existence
- Uses GET /repos/{owner}/{repo}/releases/tags/{tag}
- Returns TRUE/FALSE without downloading full release data
- **Benefit**: Much faster than piggyback's internal checks

#### `.github_release_get(repo, tag, api_url = NULL, token = NULL)`
- Fetches complete release information by tag
- Returns parsed JSON response from GitHub API
- **Use case**: Getting release ID and upload_url

#### `.github_asset_delete(repo, asset_id, api_url = NULL, token = NULL)`
- Deletes a release asset by ID
- **Use case**: Removing old assets before upload

#### `.github_asset_upload(repo, tag, file_path, asset_name = NULL, overwrite = TRUE, api_url = NULL, token = NULL)`
- Uploads file directly to GitHub release
- Handles overwrite by deleting existing asset first
- Uses release upload_url endpoint
- **Benefit**: Direct API path bypasses piggyback complexity

### 3. Integration Changes (R/remote.R)

#### Updated `.remote_check_exists_github(tag)`
- Tries fast path with `.release_exists()` first when httr available
- Falls back to piggyback's `pb_releases()` if needed
- **Benefit**: Faster existence checks in most cases

#### Enhanced `.remote_file_add_github_zip()`
- Tries direct API upload first when httr available and release exists
- Falls back to piggyback approach if:
  - httr not available
  - Direct API upload fails
  - Release doesn't exist
- Logs decision path for debugging
- **Benefit**: Faster uploads, better error handling

#### Clarified `piggyback::pb_upload()` Usage
- Now explicitly passes `overwrite = TRUE`
- Now explicitly passes `show_progress = FALSE`
- **Benefit**: Predictable behavior in non-interactive contexts

### 4. Testing (tests/testthat/test-auth.R)

Added comprehensive tests for:
- GH_TOKEN fallback mechanism
- gitcreds return value being used
- `.github_api_base()` URL resolution with various inputs
- `.release_exists()` signature and requirements
- Release existence checks against real API (when credentials available)

All tests pass: **1834 passed, 117 skipped, 0 failed**

## Benefits

### Performance
- **Faster release checks**: Direct API calls avoid piggyback's caching/processing overhead
- **Faster uploads**: Direct API path when release already exists

### Reliability
- **Better error handling**: Explicit HTTP status codes vs opaque piggyback errors
- **Fixed auth bug**: gitcreds now properly consulted

### Flexibility
- **Enterprise support**: GITHUB_API_URL environment variable for custom GitHub instances
- **More auth options**: GH_TOKEN as additional fallback
- **Backward compatible**: Falls back to piggyback if httr unavailable or direct API fails

### Clarity
- **Explicit piggyback usage**: overwrite and show_progress parameters always specified
- **Separation of concerns**: Release existence check independent of upload

## API Surface

All new functions are internal (prefixed with `.`) and can be iterated based on experience.

### Public API (No changes)
- Existing exported functions unchanged
- No breaking changes to user-facing API

### Internal API (New)
- `.github_api_base(api_url = NULL)`
- `.release_exists(repo, tag, api_url = NULL, token = NULL)`
- `.github_release_get(repo, tag, api_url = NULL, token = NULL)`
- `.github_asset_delete(repo, asset_id, api_url = NULL, token = NULL)`
- `.github_asset_upload(repo, tag, file_path, asset_name = NULL, overwrite = TRUE, api_url = NULL, token = NULL)`

### Internal API (Modified)
- `.auth_get_github_pat_find(api_url = NULL, use_gh_if_available = TRUE, use_gitcreds_if_needed = TRUE)` - Fixed bug, added GH_TOKEN fallback
- `.remote_check_exists_github(tag)` - Now tries fast path first
- `.remote_file_add_github_zip(path_zip, tag, pause_second = 3, output_level = "std", log_file = NULL)` - Now tries direct API path first

## Dependencies

### No New Hard Dependencies
- `httr` already in `Suggests` (not `Imports`)
- All new functionality gracefully degrades if httr unavailable

### Optional Enhancement
- When `httr` is available: faster release checks, direct API uploads
- When `httr` is not available: falls back to piggyback (existing behavior)

## Testing

### R CMD check
```
Status: OK
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
Duration: 5m 11.8s
```

### Test Results (LITE mode)
```
[ FAIL 0 | WARN 0 | SKIP 117 | PASS 1834 ]
```

### Coverage
- Auth token discovery: All paths tested
- API URL resolution: Tested with env vars, args, defaults
- Release existence: Tested signature and basic functionality
- Backward compatibility: All existing tests pass

## Future Enhancements

Possible future improvements (not in scope for this PR):
1. Pre-flight check function to surface missing httr early
2. Performance metrics to measure speedup of direct API path
3. Integration tests for upload functionality with dedicated test repo
4. Migration path to make httr a hard dependency if direct API path proves reliable

## Documentation

- All new functions have roxygen2 documentation
- Internal functions documented with `@keywords internal`
- README and vignettes unchanged (internal implementation detail)

## Migration Notes

No user action required. Changes are backward compatible:
- Existing workflows continue to work
- New fast paths automatically used when httr available
- Falls back gracefully when not available
- No changes to `_projr.yml` configuration needed
