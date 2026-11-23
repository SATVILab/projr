# Functions for managing build.scripts configuration
# This specifies which documents/scripts to build explicitly

#' Get build scripts from YAML configuration
#'
#' @param profile Profile name
#' @return Vector of script paths or NULL
#' @keywords internal
.yml_scripts_get <- function(profile) {
  .yml_scripts_normalize(.yml_build_get_nm("scripts", profile), "build.scripts")
}

.yml_scripts_normalize <- function(scripts, field) {
  if (is.null(scripts)) {
    return(NULL)
  }
  if (!is.null(names(scripts)) || !(is.character(scripts) || is.list(scripts))) {
    stop(field, " must contain script paths or list entries such as '- exclude: scratch.R'.", call. = FALSE)
  }
  vapply(scripts, function(entry) {
    if (.is_string_min(entry)) {
      if (is.na(entry) || !nzchar(trimws(entry))) {
        stop("Empty YAML script entry in ", field, '. Quote exclusions such as "!skip.R", ',
          "use '- exclude: skip.R', or use scripts: [] to run nothing.",
          call. = FALSE
        )
      }
      return(entry)
    }
    if (is.list(entry) && identical(names(entry), "exclude") &&
      .is_string(entry$exclude) && nzchar(trimws(entry$exclude))) {
      return(paste0("!", trimws(entry$exclude)))
    }
    stop("Each entry in ", field, " must be a script path or an 'exclude' mapping with one non-empty path or glob.",
      call. = FALSE
    )
  }, character(1), USE.NAMES = FALSE)
}

#' Get list of scripts to build for production builds
#'
#' Resolve included paths from the project root and retain exclusion patterns.
#'
#' @param profile Profile name
#' @return Vector of script paths to build or NULL
#' @keywords internal
.yml_scripts_get_build <- function(profile) {
  yml_scripts <- .yml_scripts_get(profile)
  if (is.null(yml_scripts)) {
    return(NULL)
  }

  included <- !startsWith(yml_scripts, "!")
  yml_scripts[included] <- .path_get(yml_scripts[included])
  yml_scripts
}

#' Get list of scripts to build for dev builds
#'
#' Checks dev.scripts first, falls back to build.scripts if not set
#'
#' @param profile Profile name
#' @return Vector of script paths to build or NULL
#' @keywords internal
.yml_scripts_get_dev <- function(profile) {
  # Check top-level dev.scripts first, fall back to build.scripts
  dev_scripts <- .yml_dev_get_scripts(profile)
  if (!is.null(dev_scripts)) {
    return(dev_scripts)
  }
  # Fall back to build.scripts when dev.scripts is not set
  .yml_scripts_get_build(profile)
}
