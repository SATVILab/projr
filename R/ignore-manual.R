#' @title Manually Ignore Files or Directories in `.gitignore` and `.Rbuildignore`
#'
#' @description
#' These functions allow manual addition of files and directories to the 
#' `.gitignore` and `.Rbuildignore` files, outside of the automatic management 
#' provided by the `projr` package.
#'
#' - `projr_ignore_manual`: General function to add both files and directories 
#'   to both `.gitignore` and `.Rbuildignore`. If a path does not exist, it is 
#'   treated as a file.
#' - `projr_ignore_manual_dir`: Specifically adds directories to both `.gitignore` 
#'   and `.Rbuildignore`.
#' - `projr_ignore_manual_file`: Specifically adds files to both `.gitignore` 
#'   and `.Rbuildignore`.
#' - `projr_ignore_manual_dir_git` and `projr_ignore_manual_file_git`: Add 
#'   directories or files explicitly to `.gitignore`.
#' - `projr_ignore_manual_dir_rbuild` and `projr_ignore_manual_file_rbuild`: Add 
#'   directories or files explicitly to `.Rbuildignore`.
#'
#' @details
#' These functions provide fine-grained control for cases where users want to 
#' manually ignore specific paths permanently. They do not interact with the 
#' automated ignore management system of `projr`. 
#' - Non-existent paths provided to `projr_ignore_manual` are assumed to be files.
#' - For `.gitignore`, directories are automatically appended with `/**` if 
#'   missing, ensuring proper Git ignore syntax.
#' - For `.Rbuildignore`, paths are converted to regular expressions using 
#'   `glob2rx` for compatibility with R's build tools.
#'
#' @param ignore A character vector of file or directory paths to be ignored. 
#'   Paths must be valid non-empty strings.
#'
#' @return
#' Invisibly returns `TRUE` if the operation succeeds, or `FALSE` if the input 
#' contains invalid (empty) paths.
#'
#' @seealso
#' `projr_ignore_auto` for dynamically managed ignore entries, and `projr_unignore_manual`
#' for forcing certain paths to not be ignored.
#'
#' @examples
#' # Manually ignore files and directories
#' projr_ignore_manual(c("output", "tempfile.log"))
#'
#' # Specifically ignore directories
#' projr_ignore_manual_dir("data")
#'
#' # Specifically ignore files
#' projr_ignore_manual_file("README.md")
#'
#' @export
projr_ignore_manual <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)] |>
    c(ignore[grepl("/$", ignore)]) |>
    unique()
  ignore_nonexistent <- setdiff(
    ignore, c(ignore_file, ignore_dir)
  )
  projr_ignore_manual_file_git(c(ignore_file, ignore_nonexistent))
  projr_ignore_manual_dir_git(ignore_dir)
  projr_ignore_manual_file_rbuild(c(ignore_file, ignore_nonexistent))
  projr_ignore_manual_dir_rbuild(ignore_dir)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_dir <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  projr_ignore_manual_dir_git(ignore)
  projr_ignore_manual_dir_rbuild(ignore)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_file <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  projr_ignore_manual_file_git(ignore)
  projr_ignore_manual_file_rbuild(ignore)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_file_git <- function(ignore) {
  .projr_ignore_manual_path_add(ignore, .dir_proj_get(".gitignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_dir_git <- function(ignore) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  ignore <- if (grepl("/\\*\\*$", ignore)) ignore else paste0(ignore, "/**")
  .projr_ignore_manual_path_add(ignore, .dir_proj_get(".gitignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_file_rbuild <- function(ignore) {
  ignore <- gsub("/+$", "", ignore) |>
    trimws() |>
    utils::glob2rx()
  .projr_ignore_manual_path_add(ignore, .dir_proj_get(".Rbuildignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_manual_dir_rbuild <- function(ignore) {
  ignore <- gsub("/+$", "", ignore)
  ignore <- trimws(ignore)
  patterns <- utils::glob2rx(ignore)
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- c(patterns, utils::glob2rx(ignore))
  .projr_ignore_manual_path_add(ignore, .dir_proj_get(".Rbuildignore"))
}

# ===========================================================================
# Ignore specified lines from .gitignore/.Rbuildignore
# ===========================================================================

.projr_ignore_manual_path_add <- function(ignore, path) {
  if (!all(nzchar(ignore))) {
    return(invisible(FALSE))
  }
  .assert_string(path, TRUE)

  file_vec <- .projr_ignore_manual_path_add_get_updated(path, ignore)
  .projr_ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.projr_ignore_manual_path_add_get_updated <- function(path,
                                                      ignore) {
  ignore_list <- .projr_ignore_path_get_list(path, ignore)
  
  c(
    .projr_ignore_manual_path_add_get_updated_start(ignore, ignore_list$start),
    ignore_list$content,
    ignore_list$end
  )
}

.projr_ignore_manual_path_add_get_updated_start <- function(ignore, start) {
  if (.is_len_0(start)) {
    ignore
  } else if (.is_len_1(start)) {
    c(ignore, "", start)
  else 
    c(start[-length(start)], ignore, "", start[length(start)])
  }
}

.projr_unignore_manual_path_add_get_updated <- function(path,
                                                      ignore) {
  ignore_list <- .projr_ignore_path_get_list(path, ignore)
  c(
    ignore_list[["start"]],
    ignore_list$content,
    c(ignore_list[["end"]], ignore) |> unique()
  )
}

