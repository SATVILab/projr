# Exported, "large" functions
# ==============================================================================


#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' @param label character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"docs"}.
#' Class of directory to return.
#' The \code{"docs"} option returns the path to
#' the output directory from \code{bookdown::render_book}
#' (as specified in \code{"_bookdown.yml"}),
#' whereas the others returns paths as specified in \code{"_projr.yml"}.
#' @param ... Specifies sub-directory of directory returned.
#' Passed to `file.path`.
#' @param create logical.
#' If \code{TRUE}, then the directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @param path_relative_force logical.
#' If \code{TRUE}, then forces that the returned
#' path is relative to the project root.
#' Default is \code{FALSE}.
#' @param path_absolute_force logical.
#' If `TRUE`, then forces the returned path
#' to be absolute.
#' Default is `FALSE`.
#' @param output_safe logical.
#' If \code{TRUE}, then the output directory
#' is set to be \code{"<path_to_cache>/projr_output"}
#' instead of \code{<path_to_output>} (as specified in \code{_projr.yml}).
#' The only time that this should be set to \code{TRUE}
#' should be when `projr_build_output` is being run, as otherwise
#' "development" or test runs will add to, delete or overwrite files
#' from the previous run of `projr_build_output`.
#' Default is \code{TRUE}.
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
#'
projr_dir_get <- function(label, ...,
                          create = TRUE,
                          path_relative_force = FALSE,
                          output_safe = TRUE,
                          path_absolute_force = FALSE) {
  # get path
  path_dir <- .projr_dir_get(label = label, ..., output_safe = output_safe)

  if (create) {
    .projr_dir_create(path_dir)
  }

  if (path_relative_force) {
    path_dir <- fs::path_rel(
      path_dir,
      start = rprojroot::is_r_package$find_file()
    )
  }
  as.character(path_dir) |>
    fs::path_norm() |>
    as.character()
}

#' @title Return path
#'
#' @description Returns path to \code{projr} profile-specific directory.
#' Differs from \code{projr_dir_get} in that it does not assume
#' that the path is to a directory.
#'
#' Will create the parent directory of the specified
#' path if it does not exist, and ignore it
#' if requested by `_projr.yml`.
#'
#' @param label character.
#' One of \code{"data_raw"}, \code{"cache"},\code{"output"},
#' \code{"archive"} and \code{"docs"}.
#' Class of directory to return.
#' The \code{"docs"} option returns the path to
#' the output directory from \code{bookdown::render_book}
#' (as specified in \code{"_bookdown.yml"}),
#' whereas the others returns paths as specified in \code{"_projr.yml"}.
#' @param ... Specifies sub-path of directory returned.
#' @param create logical.
#' If \code{TRUE}, then the parent directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @inheritParams projr_dir_get
#'
#' @return Character.
#' Path to directory requested.
#'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname projr_path_get
#' @export
projr_path_get <- function(label, ...,
                           create = TRUE,
                           path_relative_force = FALSE,
                           output_safe = TRUE) {
  args_dotted <- list(...)
  if (length(args_dotted) == 0) {
    path_dir <- projr_dir_get(
      label = label,
      create = create,
      path_relative_force = path_relative_force,
      output_safe = output_safe
    )
    return(path_dir)
  }
  if (length(args_dotted) > 1) {
    path_dir <- do.call(
      what = "projr_dir_get",
      args = list(
        label = label,
        args_dotted[-length(args_dotted)] |> unlist(),
        create = create,
        path_relative_force = path_relative_force,
        output_safe = output_safe
      )
    )
  } else {
    path_dir <- projr_dir_get(
      label = label,
      create = create,
      path_relative_force = path_relative_force,
      output_safe = output_safe
    )
  }
  file.path(path_dir, args_dotted[length(args_dotted)]) |>
    fs::path_norm() |>
    as.character()
}


#' @title Create a directory in _projr.yml
#'
#' @description Creates a directory that is
#' listed in _projr.yml for the current projr profile.
#' Will add to \code{.gitignore} and \code{.Rbuildignore}
#' as well if required.
#' @inheritParams projr_dir_get
#'
#' @export
projr_dir_create <- function(label, ..., output_safe = TRUE) {
  for (x in label) {
    projr_dir_get(
      label = x,
      ...,
      create = TRUE,
      output_safe = output_safe
    )
  }
}

#' @title Update ignore settings as per projr config
#'
#' @description
#' Ensure `projr` directories are ignored
#' appropriately (in `.gitignore` and `.Rbuildignore`)
#' as per `_projr.yml`.
#'
#' @description git_skip_adjust logical.
#' If \code{TRUE}, then the directories
#' are skipped by Git if already tracked
#' and they are supposed to be ignored
#' as per \code{_projr.yml}.
#' Default is \code{NULL}, in which
#' case the settings as per \code{_projr.yml}
#' are used (or the default, if unspecified there).
#'
#' @export
projr_dir_ignore <- function(git_skip_adjust = NULL) {
  label_vec <- names(projr_yml_get()[["directories"]])
  for (i in seq_along(label_vec)) {
    .projr_ignore_label_set(
      label = label_vec[[i]],
      git_skip_adjust = git_skip_adjust
    )
  }
  invisible(TRUE)
}

# Start of projr section: do not edit by hand (until `# End of projr section`)
