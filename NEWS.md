# projr 0.0.8.9002

## Bug Fixes and Improvements

### Piggyback (GitHub Releases) Operations

* **Improved debug output**: All piggyback operations (download, upload, delete, list) now provide detailed debug messages when `PROJR_OUTPUT_LEVEL="debug"` is set
  - Shows repository and tag information
  - Reports file names and sizes
  - Displays success/failure status
  - Shows error messages from piggyback operations (previously hidden by `suppressWarnings`/`suppressMessages`)
  - Reports retry attempts with reasons
  - Logs cache clearing operations
  - Lists available vs. requested files

* **Better error handling for manifest.csv and VERSION file operations**:
  - Added `tryCatch` blocks to handle download failures gracefully
  - Added informative debug messages for download attempts, successes, and failures
  - Fixed silent failures - now properly reports when files are not found or downloads fail
  - Improved error messages to help diagnose issues

* **Enhanced reliability**:
  - All retry logic now includes debug output explaining why retries are happening
  - Piggyback operations provide better feedback about what's happening under the hood
  - Error messages from the `piggyback` package are no longer silently suppressed

### Usage

To enable detailed debug output for piggyback operations:

```r
# Set environment variable before running builds
Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")
projr_build_patch()

# Or pass directly to build functions
projr_build_patch(output_level = "debug")
```

This will show detailed information about all GitHub release operations, making it much easier to diagnose issues with manifest.csv, VERSION file downloads, or release asset uploads.

* Added a `NEWS.md` file to track changes to the package.
