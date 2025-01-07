# ===========================================================================
# Exported functions for ignoring specified files/directories from specified paths
# ===========================================================================

#' @title Manually ignore specified files/directories from specified paths
#'
#' @description
#' These functions are used to add files and directories to ignore lists for 
#' different contexts. The `projr_ignore_add` function serves as a general 
#' interface to add items to both Git and R build ignore files. The 
#' `projr_ignore_add_git` and `projr_ignore_add_rbuild` functions are specific 
#' to Git and R build ignore files, respectively.
#'
#' @details
#' - `projr_ignore_add`: Adds specified files and directories to both Git 
#'   and R build ignore files.
#' - `projr_ignore_add_git`: Adds specified files and directories to 
#'   the `.gitignore` file.
#' - `projr_ignore_add_rbuild`: Adds specified files and directories to 
#'   the `.Rbuildignore` file.
#' 
#' Each function ensures that the input paths are valid (non-empty strings) 
#' before proceeding. Files and directories are handled separately to ensure 
#' correct categorization. If a path does not exist, it is assumed to be a file.
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
  projr_ignore_add_git(ignore)
  projr_ignore_add_rbuild(ignore)
}

.projr_ignore_manual_file_git <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)]
  ignore_nonexistent <- ignore[!fs::file_exists(ignore)]
  .projr_ignore_file_git(ignore_file)
  .projr_ignore_dir_git(ignore_dir)
}

#' @rdname projr_ignore_add
#' @export
projr_ignore_manual_git <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)]
  ignore_nonexistent <- ignore[!fs::file_exists(ignore)]
  .projr_ignore_file_git(ignore_file)
  .projr_ignore_dir_git(ignore_dir)
}

.projr_ignore_manual_git_file <- 

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_rbuild <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)]
  ignore_nonexistent <- ignore[!fs::file_exists(ignore)]
  .projr_ignore_file_rbuild(ignore_file)
  .projr_ignore_dir_rbuild(ignore_dir)
}
