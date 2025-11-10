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
#' Scripts go directly under build.scripts (no sub-keys allowed)
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
  
  # build.scripts should only contain raw elements (no named lists)
  # It's a plain vector of scripts
  yml_scripts
}

#' Get list of scripts to build for dev builds
#' 
#' Only checks dev.scripts (top-level), no fallback to build.scripts
#' 
#' @param profile Profile name
#' @return Vector of script paths to build or NULL
#' @keywords internal
.yml_scripts_get_dev <- function(profile) {
  # Only check top-level dev.scripts (no fallback)
  .yml_dev_get_scripts(profile)
}


