# projr 0.0.8.9002

## Breaking Changes

### CRAN Compliance: Removed Automatic Package Installation

* **Removed automatic package installation from all package code** to comply with CRAN policy that prohibits writing to user filespace during checks.

* **Pre-build package validation**: Builds now check for all required packages (build engines, remote destinations) BEFORE starting the build process. If packages are missing, you get a clear error with a copy-paste installation command for all missing packages at once.

* **Improved error messages**: When running interactively and a package is needed but not installed, projr provides clear, actionable error messages with exact installation commands. No automatic installation occurs to ensure CRAN compliance.

* `.dep_install_only()` and `.renv_lockfile_read()` no longer automatically install missing packages. Instead, they throw clear, informative errors with installation instructions when required packages are missing.

* Users must now manually install required optional dependencies before using features that depend on them. The error messages provide exact installation commands.

* This change affects functions that previously auto-installed packages like:
  - Build engines (quarto, rmarkdown, bookdown)
  - Remote destinations (piggyback for GitHub, osfr for OSF)
  - GitHub operations (requires `gh` package)
  - Git operations in test mode (requires `gert` package)  
  - License creation (requires `usethis` package)
  - Documentation generation (requires `roxygen2` package)
  - And other optional features

* **For renv projects**: Use `renv::install()` to install dependencies
* **For non-renv projects**: Use `install.packages()` or `remotes::install_github()` as shown in error messages

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
