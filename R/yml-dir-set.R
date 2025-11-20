# Directory path setting functions
# ==============================================================================

#' @rdname yml-dir-path
#' @title Set directory path
#'
#' @description
#' `projr_yml_dir_path_set` sets the path for a directory label in the project.
#'
#' `projr_yml_dir_path_rm` removes the custom path setting for a directory,
#' reverting to default behavior.
#'
#' @param label character.
#' Directory label to configure (e.g., "output", "raw-data", "cache").
#' Must be a valid directory label.
#' @param path character.
#' Path to the directory.
#' @param profile character.
#' Profile to modify.
#' If `"default"` (the default), modifies the default profile (`_projr.yml`).
#' If another character vector, modifies `_projr-<profile>.yml`.
#'
#' @examples
#' \dontrun{
#' # Set path for output directory
#' projr_yml_dir_path_set("output", "_my_output")
#'
#' # Set path for cache directory
#' projr_yml_dir_path_set("cache", "_my_cache")
#'
#' # Revert to default path
#' projr_yml_dir_path_rm("output")
#' }
#' @export
projr_yml_dir_path_set <- function(label, path, profile = "default") {
  .assert_string(label, TRUE)
  .dir_check_label_strip(label)
  .assert_string(path, TRUE)
  .assert_string(profile)

  # Use existing internal function to set path
  .yml_dir_set_path(path, label, profile)

  invisible(TRUE)
}

#' @rdname yml-dir-path
#' @export
projr_yml_dir_path_rm <- function(label, profile = "default") {
  .assert_string(label, TRUE)
  .assert_string(profile)

  # Get the default path for this label
  default_path <- .yml_dir_get_path_default(label)

  if (!is.null(default_path)) {
    # If there's a default path, set it explicitly
    .yml_dir_set_path(default_path, label, profile)
  } else {
    # If no default path, remove the label configuration entirely
    yml_projr <- .yml_get(profile)
    if (!is.null(yml_projr[["directories"]]) &&
      label %in% names(yml_projr[["directories"]])) {
      yml_projr[["directories"]][[label]] <- NULL
      .yml_set(yml_projr, profile)
    }
  }

  invisible(TRUE)
}
