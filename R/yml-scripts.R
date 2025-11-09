# Functions for managing build.scripts configuration
# This specifies which documents/scripts to build explicitly

#' Get build scripts from YAML configuration
#' 
#' @param profile Profile name
#' @return Vector of script paths or NULL
#' @keywords internal
.yml_scripts_get <- function(profile) {
  .yml_build_get_nm("scripts", profile)
}

#' Get list of scripts to build for production builds
#' 
#' Scripts go directly under build.scripts (no "build" sub-key)
#' Format: scripts: [file1.qmd, file2.qmd]
#' 
#' @param profile Profile name
#' @return Vector of script paths to build or NULL
#' @keywords internal
.yml_scripts_get_build <- function(profile) {
  yml_scripts <- .yml_scripts_get(profile)
  if (is.null(yml_scripts)) {
    return(NULL)
  }
  
  # Filter out "dev" key (dev-specific scripts)
  # All other entries are build scripts
  script_names <- names(yml_scripts)
  if (is.null(script_names)) {
    # It's a plain vector of scripts
    return(yml_scripts)
  }
  
  # Filter out dev key
  build_scripts <- yml_scripts[!script_names %in% c("dev")]
  
  # If what's left is a plain vector (unnamed), return it
  if (length(build_scripts) > 0 && is.null(names(build_scripts))) {
    return(unlist(build_scripts))
  }
  
  # If it's named, return the values
  if (length(build_scripts) > 0) {
    return(unlist(build_scripts, use.names = FALSE))
  }
  
  NULL
}

#' Get list of scripts to build for dev builds
#' 
#' Checks for dev.scripts first (top-level), then build.scripts.dev, then build.scripts
#' 
#' @param profile Profile name
#' @return Vector of script paths to build or NULL
#' @keywords internal
.yml_scripts_get_dev <- function(profile) {
  # First check top-level dev.scripts (highest priority for dev builds)
  dev_scripts <- .yml_dev_get_scripts(profile)
  if (!is.null(dev_scripts)) {
    return(dev_scripts)
  }
  
  # Fall back to build.scripts.dev
  yml_scripts <- .yml_scripts_get(profile)
  if (is.null(yml_scripts)) {
    return(NULL)
  }
  
  # If yml_scripts has a "dev" element, use that
  if ("dev" %in% names(yml_scripts)) {
    return(yml_scripts[["dev"]])
  }
  
  # Otherwise fall back to the general scripts
  .yml_scripts_get_build(profile)
}

#' Get pre-build hooks from build.scripts
#' 
#' @param profile Profile name
#' @return Hook configuration or NULL
#' @keywords internal
.yml_scripts_get_hooks_pre <- function(profile) {
  yml_scripts <- .yml_scripts_get(profile)
  if (is.null(yml_scripts)) {
    return(NULL)
  }
  yml_scripts[["pre"]]
}

#' Get post-build hooks from build.scripts
#' 
#' @param profile Profile name
#' @return Hook configuration or NULL
#' @keywords internal
.yml_scripts_get_hooks_post <- function(profile) {
  yml_scripts <- .yml_scripts_get(profile)
  if (is.null(yml_scripts)) {
    return(NULL)
  }
  yml_scripts[["post"]]
}
