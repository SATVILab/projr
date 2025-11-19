# Directory configuration setting functions
# ==============================================================================

#' @rdname yml-dir-set
#' @title Set directory configuration
#'
#' @description
#' `projr_dir_set` sets directory configuration for the project.
#'
#' The options are:
#' \itemize{
#' \item path: the directory path
#' \item ignore: whether to ignore the directory in both git and rbuild
#' \item ignore_git: whether to ignore the directory in git (.gitignore)
#' \item ignore_rbuild: whether to ignore the directory in R build (.Rbuildignore)
#' \item git_skip_adjust: whether to adjust git skip-worktree flags
#' }
#' The default is to leave all the settings unchanged.
#'
#' If `path` is not set and the label is new, an error is thrown.
#'
#' `projr_dir_set_default` removes custom settings for a directory,
#' reverting to default behavior.
#'
#' @param label character.
#' Directory label to configure (e.g., "output", "raw-data", "cache").
#' Must be a valid directory label.
#' @param path character.
#' Path to the directory.
#' If `NULL`, the setting is not changed.
#' Default is `NULL`.
#' @param ignore logical or character.
#' Whether to ignore the directory in both git and rbuild.
#' Can be `TRUE`, `FALSE`, or one of `"manual"`, `"ignore"`, `"no-ignore"`.
#' If `NULL`, the setting is not changed.
#' Default is `NULL`.
#' @param ignore_git logical or character.
#' Whether to ignore the directory in git (.gitignore).
#' Can be `TRUE`, `FALSE`, or one of `"manual"`, `"ignore"`, `"no-ignore"`.
#' If `NULL`, the setting is not changed.
#' Default is `NULL`.
#' @param ignore_rbuild logical or character.
#' Whether to ignore the directory in R build (.Rbuildignore).
#' Can be `TRUE`, `FALSE`, or one of `"manual"`, `"ignore"`, `"no-ignore"`.
#' If `NULL`, the setting is not changed.
#' Default is `NULL`.
#' @param git_skip_adjust logical.
#' Whether to adjust git skip-worktree flags for files in the directory.
#' If `NULL`, the setting is not changed.
#' Default is `NULL`.
#' @param simplify_default logical.
#' If `TRUE`, then if all settings are at their defaults,
#' the directory configuration is simplified or removed from `_projr.yml`.
#' Default is `TRUE`.
#' @param profile character.
#' Profile to modify.
#' If `"default"` (the default), modifies the default profile (`_projr.yml`).
#' If `NULL`, the active profile is used and written to `_projr.yml`.
#' If another character vector, modifies `_projr-<profile>.yml`.
#'
#' @examples
#' \dontrun{
#' # Set path for output directory
#' projr_dir_set("output", path = "_my_output")
#'
#' # Set ignore settings for cache directory
#' projr_dir_set("cache", ignore_git = TRUE, ignore_rbuild = TRUE)
#'
#' # Create a new directory label
#' projr_dir_set("results", path = "_results", ignore_git = TRUE)
#'
#' # Revert to defaults
#' projr_dir_set_default("cache")
#' }
#' @export
projr_dir_set <- function(label,
                          path = NULL,
                          ignore = NULL,
                          ignore_git = NULL,
                          ignore_rbuild = NULL,
                          git_skip_adjust = NULL,
                          simplify_default = TRUE,
                          profile = "default") {
  .yml_dir_set_check(
    label = label,
    path = path,
    ignore = ignore,
    ignore_git = ignore_git,
    ignore_rbuild = ignore_rbuild,
    git_skip_adjust = git_skip_adjust,
    simplify_default = simplify_default,
    profile = profile
  )

  .yml_dir_set_ind(
    label = label,
    path = path,
    ignore = ignore,
    ignore_git = ignore_git,
    ignore_rbuild = ignore_rbuild,
    git_skip_adjust = git_skip_adjust,
    simplify_default = simplify_default,
    profile = profile
  )

  invisible(TRUE)
}

#' @rdname yml-dir-set
#' @export
projr_dir_set_default <- function(label, profile = "default") {
  .assert_string(label, TRUE)
  .assert_string(profile)

  # Remove the label configuration entirely, reverting to defaults
  yml_projr <- .yml_get(profile)
  if (!is.null(yml_projr[["directories"]]) &&
    label %in% names(yml_projr[["directories"]])) {
    yml_projr[["directories"]][[label]] <- NULL
    .yml_set(yml_projr, profile)
  }

  invisible(TRUE)
}

# Internal helper functions
# ==============================================================================

.yml_dir_set_check <- function(label,
                               path,
                               ignore,
                               ignore_git,
                               ignore_rbuild,
                               git_skip_adjust,
                               simplify_default,
                               profile) {
  .assert_string(label, TRUE)
  .dir_check_label_strip(label)

  if (!is.null(path)) {
    .assert_string(path, TRUE)
  }

  .yml_dir_set_check_ignore(ignore, "ignore")
  .yml_dir_set_check_ignore(ignore_git, "ignore_git")
  .yml_dir_set_check_ignore(ignore_rbuild, "ignore_rbuild")

  if (!is.null(git_skip_adjust)) {
    .assert_flag(git_skip_adjust)
  }

  .assert_flag(simplify_default, TRUE)
  .assert_string(profile)
}

.yml_dir_set_check_ignore <- function(val, nm) {
  if (is.null(val)) {
    return(invisible(TRUE))
  }

  .assert_class_any(val, c("logical", "character"), TRUE)
  .assert_len_1(val, TRUE)

  if (is.logical(val)) {
    .assert_flag(val)
  } else if (is.character(val)) {
    .assert_in(val, c("manual", "ignore", "no-ignore"))
  }

  invisible(TRUE)
}

.yml_dir_set_ind <- function(label,
                             path,
                             ignore,
                             ignore_git,
                             ignore_rbuild,
                             git_skip_adjust,
                             simplify_default,
                             profile) {
  # Get current directory config or create new one
  yml_dir <- .yml_dir_get_label(label, profile)
  if (is.null(yml_dir)) {
    yml_dir <- list()
  }

  # Require path for new labels
  if (length(yml_dir) == 0 && is.null(path)) {
    stop(
      "Setting '", label, "' directory requires a 'path' argument ",
      "for new directory labels.",
      call. = FALSE
    )
  }

  # Set path
  if (!is.null(path)) {
    yml_dir[["path"]] <- path
  }

  # Set ignore settings
  if (!is.null(ignore)) {
    yml_dir[["ignore"]] <- ignore
  }

  if (!is.null(ignore_git)) {
    yml_dir[["ignore-git"]] <- ignore_git
  }

  if (!is.null(ignore_rbuild)) {
    yml_dir[["ignore-rbuild"]] <- ignore_rbuild
  }

  if (!is.null(git_skip_adjust)) {
    yml_dir[["git-skip-adjust"]] <- git_skip_adjust
  }

  # Apply simplification if requested
  if (simplify_default) {
    yml_dir <- .yml_dir_simplify_default(yml_dir, label, profile)
  }

  # Write back to YAML
  .yml_dir_set_label(yml_dir, label, profile)

  invisible(TRUE)
}

.yml_dir_simplify_default <- function(yml_dir, label, profile) {
  # If all settings match defaults, simplify or remove
  # For simplification: only remove entire directory config if path is default
  # and no other settings exist

  # Check if this is a standard label with default path
  default_path <- .yml_dir_get_path_default(label)

  # If path is default and no other settings, can remove entirely
  if (!is.null(default_path) &&
    !is.null(yml_dir[["path"]]) &&
    identical(yml_dir[["path"]], default_path) &&
    length(yml_dir) == 1) {
    return(NULL)
  }

  yml_dir
}
