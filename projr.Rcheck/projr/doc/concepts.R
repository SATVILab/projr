## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# projr_build_dev()

## ----eval=FALSE---------------------------------------------------------------
# projr_build()        # patch bump
# projr_build_minor()  # minor bump
# projr_build_major()  # major bump

## ----eval=FALSE---------------------------------------------------------------
# # Clear before build (default)
# projr_build_patch(clear_output = "pre")
# 
# # Clear after build
# projr_build_patch(clear_output = "post")
# 
# # Never clear automatically
# projr_build_patch(clear_output = "never")

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(PROJR_CLEAR_OUTPUT = "post")
# projr_build_patch()  # Uses "post" mode

## ----eval=FALSE---------------------------------------------------------------
# # Clean slate approach
# projr_build_patch(clear_output = "pre")
# 
# # 1. Clears _output/ completely
# # 2. Builds analysis
# # 3. Saves outputs directly to _output/
# # 4. Archives to remotes (if configured)

## ----eval=FALSE---------------------------------------------------------------
# # Safety-first approach
# projr_build_patch(clear_output = "post")
# 
# # 1. Keeps existing _output/ intact
# # 2. Builds to cache (_tmp/projr/v0.0.1/)
# # 3. After successful build, clears _output/
# # 4. Copies from cache to _output/
# # 5. Archives to remotes (if configured)

## ----eval=FALSE---------------------------------------------------------------
# # Advanced: Manual clearing
# unlink("_output", recursive = TRUE)  # Clear manually
# projr_build_patch(clear_output = "never")
# 
# # 1. No automatic clearing
# # 2. Builds to specified locations
# # 3. User responsible for cleanup

## ----eval=FALSE---------------------------------------------------------------
# # Compare current hash with manifest
# tools::md5sum("_output/figure.png")
# # vs hash in manifest

## ----eval=FALSE---------------------------------------------------------------
# # See what files changed between v0.0.1 and v0.0.2
# changes <- projr_manifest_changes("0.0.1", "0.0.2")
# # Returns: label, fn, change_type (added/modified/removed), hash_from, hash_to
# 
# # Filter by directory
# output_changes <- projr_manifest_changes("0.0.1", "0.0.2", label = "output")

## ----eval=FALSE---------------------------------------------------------------
# # See all files and when they last changed from v0.0.1 to current
# file_history <- projr_manifest_range("0.0.1")
# # Returns: label, fn, version_first, version_last_change, hash

## ----eval=FALSE---------------------------------------------------------------
# # See most recent changes for each directory
# last_changes <- projr_manifest_last_change()
# # Returns: label, version_last_change, n_files

## ----eval=FALSE---------------------------------------------------------------
# # Restore from GitHub
# projr_restore_repo("owner/repo")
# 
# # Restore specific version
# projr_restore(label = "raw-data", version = "v0.1.0")
# 
# # Restore specific label
# projr_restore(label = "output")

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(PROJR_PROFILE = "dev")

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(PROJR_PROFILE = "dev")
# Sys.setenv(PROJR_OUTPUT_CLEAR = "pre")

## ----eval=FALSE---------------------------------------------------------------
# projr_env_set(
#   profile = "dev",
#   output_clear = "pre"
# )

## ----eval=FALSE---------------------------------------------------------------
# projr_init_renv()

## ----eval=FALSE---------------------------------------------------------------
# projr_renv_update()  # Wrapper for renv::snapshot()

## ----eval=FALSE---------------------------------------------------------------
# projr_renv_restore()  # Wrapper for renv::restore()

## ----eval=FALSE---------------------------------------------------------------
# # 1. Initialise
# projr_init()
# 
# # 2. Add raw data to _raw_data/
# 
# # 3. Write analysis code in .Rmd files
# 
# # 4. Iterate with dev builds
# projr_build_dev("analysis.Rmd")
# # Check outputs in _tmp/projr/v0.0.1/
# 
# # 5. When ready, create first release
# projr_build()
# # Outputs in _output/, archived to GitHub
# 
# # 6. Continue development
# # ... edit code ...
# projr_build_dev()
# 
# # 7. Create minor release with new analysis
# projr_build_minor()
# 
# # 8. Share repository
# # Collaborators run:
# projr_restore_repo("you/your-project")

