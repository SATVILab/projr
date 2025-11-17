## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(projr)
# 
# # Option 1: Interactive prompts (recommended for first-time users)
# projr_init_prompt()
# 
# # Option 2: Sensible defaults
# projr_init()
# 
# # Option 3: Full setup with extras
# projr_init_full()

## ----eval=FALSE---------------------------------------------------------------
# # Set license
# projr_init_license(license = "MIT")
# 
# # Set up citation
# projr_init_cite(
#   title = "My Research Project",
#   authors = "Jane Doe"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Build all documents, route outputs to cache
# projr_build_dev()
# 
# # Build specific documents only
# projr_build_dev("analysis.Rmd")
# projr_build_dev(c("methods.Rmd", "results.Rmd"))

## ----eval=FALSE---------------------------------------------------------------
# # Clear _output before building (useful for testing)
# projr_build_dev(clear_output = "pre")

## ----eval=FALSE---------------------------------------------------------------
# # In your R Markdown or scripts:
# fig_path <- projr_path_get("output", "figures", "plot.png")
# png(filename = fig_path)
# # ... plotting code ...
# dev.off()
# 
# # During dev build: saves to _tmp/projr/v0.1.0/output/figures/plot.png
# # During final build: saves to _output/figures/plot.png

## ----eval=FALSE---------------------------------------------------------------
# # Patch version bump (0.1.0 -> 0.1.1)
# projr_build()
# # or explicitly:
# projr_build_patch()
# 
# # Minor version bump (0.1.0 -> 0.2.0)
# projr_build_minor()
# 
# # Major version bump (0.1.0 -> 1.0.0)
# projr_build_major()

## ----eval=FALSE---------------------------------------------------------------
# # Set environment variable to control clearing behaviour
# # Options: "pre" (before build), "post" (after build), "none"
# Sys.setenv(PROJR_OUTPUT_CLEAR = "pre")
# projr_build()
# 
# # Or set in .Renviron file:
# # PROJR_OUTPUT_CLEAR=pre

## ----eval=FALSE---------------------------------------------------------------
# # Add GitHub archive
# projr_yml_dest_add_github(
#   label = "raw-data",
#   content = "raw-data",
#   description = "Raw data files"
# )
# 
# # Add OSF archive
# projr_yml_dest_add_osf(
#   label = "raw-data",
#   content = "raw-data",
#   project = "abc123",
#   component = "data"
# )
# 
# # Add local archive
# projr_yml_dest_add_local(
#   label = "raw-data",
#   content = "raw-data",
#   path = "~/archive/data"
# )

## ----eval=FALSE---------------------------------------------------------------
# # View current YAML configuration
# projr_yml_get()
# 
# # Check if files will be uploaded
# # (Run a dev build to see what would happen without actually uploading)
# projr_build_dev()

## ----eval=FALSE---------------------------------------------------------------
# # Clone repo and restore raw data
# projr_restore_repo("owner/repo")
# 
# # Clone to specific directory
# projr_restore_repo("owner/repo", path = "~/projects/my-project")
# 
# # Restore from current working directory (if already cloned)
# projr_restore_repo_wd()

## ----eval=FALSE---------------------------------------------------------------
# # Restore raw data only
# projr_restore(label = "raw-data")
# 
# # Restore multiple directories
# projr_restore(label = c("raw-data", "output"))
# 
# # Restore specific version
# projr_restore(label = "raw-data", version = "v0.1.0")

## ----eval=FALSE---------------------------------------------------------------
# # Access custom paths
# projr_path_get("raw-data-public")
# # "_raw_data_public"
# 
# projr_path_get("output-figures", "plot.png")
# # "_output/figures/plot.png"

## ----eval=FALSE---------------------------------------------------------------
# # Create a profile based on _projr.yml
# projr_profile_create("dev")
# 
# # This creates _projr-dev.yml

## ----eval=FALSE---------------------------------------------------------------
# # Set environment variable
# Sys.setenv(PROJR_PROFILE = "dev")
# 
# # Or set in .Renviron:
# # PROJR_PROFILE=dev
# 
# # Now all projr functions use _projr-dev.yml
# projr_build()

## ----eval=FALSE---------------------------------------------------------------
# projr_profile_get()
# # "dev"

## ----eval=FALSE---------------------------------------------------------------
# projr_profile_delete("dev")

## ----eval=FALSE---------------------------------------------------------------
# # View dev build history
# file.edit("cache/projr/log/dev/history/builds.md")
# 
# # View output build history
# file.edit("cache/projr/log/output/history/builds.md")

## ----eval=FALSE---------------------------------------------------------------
# # List recent dev build logs
# list.files("cache/projr/log/dev/output",
#            recursive = TRUE, pattern = "\\.qmd$")
# 
# # Render a log file to HTML
# quarto::quarto_render("cache/projr/log/dev/output/2025-Nov-11/14-07-00.qmd")

## ----eval=FALSE---------------------------------------------------------------
# # Disable detailed log file creation (history still maintained)
# Sys.setenv(PROJR_LOG_DETAILED = "FALSE")
# projr_build_dev()
# 
# # Re-enable detailed logging
# Sys.setenv(PROJR_LOG_DETAILED = "TRUE")

## ----eval=FALSE---------------------------------------------------------------
# # Clear all logs
# projr_log_clear()
# 
# # Clear only dev logs
# projr_log_clear(build_type = "dev")
# 
# # Keep history, clear detailed logs only
# projr_log_clear(history = FALSE, output = TRUE)
# 
# # Clear logs before a specific date
# projr_log_clear(before_date = "2025-01-01")

## ----eval=FALSE---------------------------------------------------------------
# # See what files changed between v0.0.1 and v0.0.2
# changes <- projr_manifest_changes("0.0.1", "0.0.2")
# 
# # View the results
# changes
# # Returns: label, fn, change_type (added/modified/removed),
# #          hash_from, hash_to
# 
# # Filter by directory
# output_changes <- projr_manifest_changes(
#   "0.0.1", "0.0.2",
#   label = "output"
# )

## ----eval=FALSE---------------------------------------------------------------
# # See all files and when they last changed from v0.0.1 to current
# file_history <- projr_manifest_range("0.0.1")
# 
# # View the results
# file_history
# # Returns: label, fn, version_first, version_last_change, hash
# 
# # Limit to specific directory and version range
# raw_data_history <- projr_manifest_range(
#   "0.0.1", "0.0.5",
#   label = "raw-data"
# )

## ----eval=FALSE---------------------------------------------------------------
# # See most recent changes for each directory
# last_changes <- projr_manifest_last_change()
# 
# # View the results
# last_changes
# # Returns: label, version_last_change, n_files
# 
# # Check status as of a specific version
# historical_status <- projr_manifest_last_change("0.0.3")

## ----eval=FALSE---------------------------------------------------------------
# # Did any raw data change since last release?
# raw_changes <- projr_manifest_changes(
#   "0.1.0",
#   projr_version_get(),
#   label = "raw-data"
# )
# if (nrow(raw_changes) > 0) {
#   message("Raw data has changed - outputs may need regeneration")
# }
# 
# # Find files that haven't changed in a while
# old_files <- projr_manifest_range("0.0.1")
# old_files[old_files$version_last_change < "v0.0.5", ]

## ----eval=FALSE---------------------------------------------------------------
# file.create("_environment")         # Global defaults
# file.create("_environment-dev")     # Profile-specific
# file.create("_environment.local")   # Machine-specific (git-ignored)

## ----eval=FALSE---------------------------------------------------------------
# projr_env_set()  # Loads all files in order of precedence

## ----eval=FALSE---------------------------------------------------------------
# # Build control
# Sys.setenv(PROJR_OUTPUT_LEVEL = "debug")    # "none", "std", "debug"
# Sys.setenv(PROJR_LOG_DETAILED = "FALSE")    # TRUE/FALSE
# Sys.setenv(PROJR_CLEAR_OUTPUT = "never")    # "pre", "post", "never"
# 
# # Profile selection
# Sys.setenv(PROJR_PROFILE = "dev")
# Sys.setenv(QUARTO_PROFILE = "production")

