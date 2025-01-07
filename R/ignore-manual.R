# ===========================================================================
# Exported functions for ignoring specified files/directories from specified paths
# ===========================================================================

#' @title Manually ignore specified files/directories from .gitignore and/or .Rbuildignore
#'
#' @description
#' These functions are used to add files and directories to ignore lists for 
#' different contexts.
#' - projr_ignore_manual: Adds specified files and directories to both Git
#'  and R build ignore files. Automatically determines whether the input is a
#' file or directory, and if it does not exist, it is assumed to be a file.
#' - projr_ignore_manual_dir,projr_ignore_manual_file: 
#'   Explicitly adds directories or files to both Git and R build ignore files.
#' - projr_ignore_manual_dir_git,projr_ignore_manual_file_git:
#'   Explicitly adds directories or files to the `.gitignore` file.
#' - projr_ignore_manual_dir_rbuild,projr_ignore_manual_file_rbuild:
#'   Explicitly adds directories or files to the `.Rbuildignore` file.
#'
#' @details
#' - `projr_ignore_add`: Adds specified files and directories to both Git 
#'   and R build ignore files.
#' - `projr_ignore_add_git`: Adds specified files and directories to 
#'   the `.gitignore` file.
#' - `projr_ignore_add_rbuild`: Adds specified files and directories to 
#'   the `.Rbuildignore` file.
#' 
#'
#' @param ignore A character vector of file or directory paths to be ignored. 
#'   Paths must be valid non-empty strings.
#'
#' @return
#' The functions return `invisible(FALSE)` if the input contains invalid 
#' (empty) paths. Otherwise, the operations are performed invisibly.
#'
#' @seealso
#' `.projr_ignore_file_git`, `.projr_ignore_dir_git`, `.projr_ignore_file_rbuild`, 
#' `.projr_ignore_dir_rbuild` for the underlying helper functions.
#'
#' @export
projr_ignore_manual <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)]
  ignore_nonexistent <- ignore[!fs::file_exists(ignore)]
  .projr_ignore_manual_file_git(c(ignore_file, ignore_nonexistent))
  .projr_ignore_manual_dir_git(ignore_dir)
  .projr_ignore_manual_file_rbuild(c(ignore_file, ignore_nonexistent))
  .projr_ignore_manual_dir_rbuild(ignore_dir)
}

projr_ignore_manual_dir <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  .projr_ignore_manual_dir_git(ignore)
  .projr_ignore_manual_dir_rbuild(ignore)
}

projr_ignore_manual_file <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  .projr_ignore_manual_file_git(ignore)
  .projr_ignore_manual_file_rbuild(ignore)
}

projr_ignore_manual_file_git <- function(ignore) {
  .projr_ignore_manual_path_add(ignore, projr_path_get(".gitignore"))
}

projr_ignore_manual_dir_git <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore <- if (grepl("/\\*\\*$", ignore)) ignore else paste0(ignore, "/**")
  .projr_ignore_manual_path_add(ignore, projr_path_get(".gitignore"))
}

projr_ignore_manual_file_rbuild <- function(ignore) {
  ignore <- gsub("/+$", "", ignore) |>
    trimws() |>
    utils::glob2rx()
  .projr_ignore_manual_path_add(ignore, projr_path_get(".Rbuildignore"))
}

projr_ignore_manual_dir_rbuild <- function(ignore) {
  # Remove trailing slashes and trim whitespace
  ignore <- gsub("/+$", "", ignore)
  ignore <- trimws(ignore)

  # Convert the glob pattern to a regular expression pattern
  patterns <- utils::glob2rx(ignore)

  # Handle directory-specific patterns
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- c(patterns, utils::glob2rx(ignore))
  .projr_ignore_manual_path_add(ignore, projr_path_get(".Rbuildignore"))
}

# ===========================================================================
# Ignore specified lines from .gitignore/.Rbuildignore
# ===========================================================================

.projr_ignore_manual_path_add <- function(ignore, path) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  .assert_string(path, TRUE)

  file_vec <- .projr_ignore_manual_path_add_get_updated(path, ignore, FALSE)
  .projr_ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.projr_ignore_manual_path_add_get_updated <- function(path,
                                                      ignore) {
  ignore_list <- .projr_ignore_path_get_list(path, ignore)
  c(
    c(ignore_list[["start"]], ignore) |> unique(),
    ignore_list$content,
    ignore_list$end
  )
}

