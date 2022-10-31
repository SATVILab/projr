#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' @param type character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"bookdown"}.
#' Class of directory to return.
#' The \code{"bookdown"} option returns the path to
#' the output directory from \code{bookdown::render_book}
#' (as specified in \code{"_bookdown.yml"}),
#' whereas the others returns paths as specified in \code{"_projr.yml"}.
#' @param ... Specifies sub-directory of directory returned.
#' Passed to `file.path`.
#' @param force_relative logical.
#' If \code{TRUE}, then forces that the returned
#' path is relative to the project root.
#' Default is \code{FALSE}.
#' @return Character.
#' Path to directory requested.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname projr_dir_get
#' @export
projr_dir_get <- function(type, ..., rel = TRUE) {
  if (!type %in% c("data_raw", "cache", "output", "archive", "bookdown")) {
    stop(paste0("type `", type, "` not recognised."))
  }
  dir_proj <- rprojroot::is_r_package$find_file()

  if (type == "bookdown") {
    yml_bd <- yaml::read_yaml(file.path(dir_proj, "_bookdown.yml"))
    path_final <- file.path(yml_bd[["output_dir"]], ...)
    if (!dir.exists(path_final)) {
      dir.create(path_final, recursive = TRUE)
    }
    return(path_final)
  }

  # get active directories
  yml_active <- projr_get_yml_active(
    path_yml = file.path(dir_proj, "_projr.yml"),
    silent = TRUE
  )

  # get current version
  version_format_list <- .get_version_format_list(
    version_format = yml_active[["version"]]
  )
  yml_bd <- yaml::read_yaml(
    file.path(dir_proj, "_bookdown.yml")
  )
  proj_nm <- .get_proj_nm(
    fn = yml_bd$book_filename,
    version_format = yml_active[["version"]]
  )
  version_current <- gsub(
    paste0("^", proj_nm), "", yml_bd$book_filename
  )

  dir_active <- yml_active[["directories"]]
  yml_active_dir_curr <- yml_active[["directories"]][type]
  yml_active[["directories"]] <- yml_active_dir_curr
  projr_set_up_dir(
    yml_active,
    version_current = version_current,
    create_var = FALSE, env = .GlobalEnv
  )

  path_final <- file.path(dir_active[[type]]$path, ...)
  if (!dir.exists(path_final)) {
    dir.create(path_final, recursive = TRUE)
  }
  if (force_relative) {
    path_final <- fs::path_rel(
      path_final,
      start = rprojroot::is_r_package$find_file()
    )
  }
  path_final
}
