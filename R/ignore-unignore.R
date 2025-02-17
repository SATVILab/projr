#' @title Manually Unignore Files or Directories in `.gitignore` and `.Rbuildignore`
#'
#' @description
#' These functions allow manual addition of files and directories to the 
#' `.gitignore` and `.Rbuildignore` files **after** the projr-managed block,
#' thereby forcing them to be _not_ ignored.
#'
#' - .unignore_manual`: General function to unignore both files and 
#'   directories in both `.gitignore` and `.Rbuildignore`. If a path does not 
#'   exist, it is treated as a file.
#' - .unignore_manual_dir`: Specifically unignores directories in both 
#'   `.gitignore` and `.Rbuildignore`.
#' - .unignore_manual_file`: Specifically unignores files in both 
#'   `.gitignore` and `.Rbuildignore`.
#' - .unignore_manual_dir_git` and .unignore_manual_file_git`: Add 
#'   directories or files explicitly (with a `!` prefix) to `.gitignore`.
#' - .unignore_manual_dir_rbuild` and .unignore_manual_file_rbuild`: 
#'   Add directories or files explicitly (with a `!` prefix) to `.Rbuildignore`.
#'
#' @details
#' These functions provide fine-grained control for cases where users want to
#' _undo_ any ignoring behavior for specific paths permanently. They do not 
#' interact with the automated ignore management system of `projr`.
#' - Non-existent paths provided to .unignore_manual` are assumed to be files.
#' - For `.gitignore`, unignored directories are automatically appended with 
#'   `/**` if missing, then prepended with `!`, ensuring proper Git _unignore_ 
#'   syntax.
#' - For `.Rbuildignore`, paths are converted to regular expressions using 
#'   `glob2rx()`, and then prepended with `!` for compatibility with R's build 
#'   tools.
#'
#' @param unignore A character vector of file or directory paths to be unignored. 
#'   Paths must be valid non-empty strings.
#'
#' @return
#' Invisibly returns `TRUE` if the operation succeeds, or `FALSE` if the input 
#' contains invalid (empty) paths.
#'
#' @seealso
#' .ignore_manual` for manually ignoring paths, and .ignore_auto` for 
#' dynamically managed ignore entries.
#'
#' @examples
#' # Manually unignore files and directories
#' projr_unignore_manual(c("output", "tempfile.log"))
#'
#' # Specifically unignore directories
#' projr_unignore_manual_dir("data")
#'
#' # Specifically unignore files
#' projr_unignore_manual_file("README.md")
#'
#' @export
projr_unignore_manual <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  unignore_file <- unignore[fs::is_file(unignore)]
  unignore_dir <- unignore[fs::is_dir(unignore)] |>
    c(unignore[grepl("/$", unignore)]) |>
    unique()
  unignore_nonexistent <- setdiff(
    unignore, c(unignore_file, unignore_dir)
  )

  # Force-unignore files (or nonexistent paths) in .gitignore and .Rbuildignore
 .unignore_manual_file_git(c(unignore_file, unignore_nonexistent))
 .unignore_manual_dir_git(unignore_dir)
 .unignore_manual_file_rbuild(c(unignore_file, unignore_nonexistent))
 .unignore_manual_dir_rbuild(unignore_dir)
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_dir <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
 .unignore_manual_dir_git(unignore)
 .unignore_manual_dir_rbuild(unignore)
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_file <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
 .unignore_manual_file_git(unignore)
 .unignore_manual_file_rbuild(unignore)
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_file_git <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  # Remove any existing "!" to avoid duplication, then prepend it
  unignore <- gsub("^!+", "", unignore)
  unignore <- paste0("!", unignore)
  .unignore_manual_path_add(unignore, .path_get(".gitignore"))
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_dir_git <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  # Ensure trailing "/**" if not already present
  unignore <- if (grepl("/\\*\\*$", unignore)) unignore else paste0(unignore, "/**")
  # Remove any existing "!" and prepend it
  unignore <- gsub("^!+", "", unignore)
  unignore <- paste0("!", unignore)
  .unignore_manual_path_add(unignore, .path_get(".gitignore"))
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_file_rbuild <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  # Trim trailing slashes, convert to regex, then prepend "!"
  unignore <- gsub("/+$", "", unignore) |>
    trimws() |>
    utils::glob2rx()
  unignore <- gsub("^!+", "", unignore)
  unignore <- paste0("!", unignore)
  .unignore_manual_path_add(unignore, .path_get(".Rbuildignore"))
}

#' @rdname projr_unignore_manual
#' @export
projr_unignore_manual_dir_rbuild <- function(unignore) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  unignore <- gsub("/+$", "", unignore)
  unignore <- trimws(unignore)
  patterns <- utils::glob2rx(unignore)
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- c(patterns, utils::glob2rx(unignore))

  # Prepend "!" to each pattern
  patterns <- gsub("^!+", "", patterns)
  patterns <- paste0("!", patterns)

  .unignore_manual_path_add(patterns, .path_get(".Rbuildignore"))
}

# ===========================================================================
# Helper for appending unignore lines after projr-managed blocks
# ===========================================================================

.unignore_manual_path_add <- function(unignore, path) {
  unignore <- setdiff(unignore, "")
  if (!.is_chr(unignore)) {
    return(invisible(FALSE))
  }
  .assert_string(path, TRUE)
  unignore <- unignore |> unique()

  file_vec <- .unignore_manual_path_add_get_updated(path, unignore)
  .ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.unignore_manual_path_add_get_updated <- function(path, unignore) {
  # Retrieve the current state of the ignore file in a structured way
  ignore_list <- .ignore_path_get_list(path, unignore)
  # Append unignore patterns after the 'end' portion
  c(
    ignore_list[["start"]],
    ignore_list$content |> setdiff("") |> unique(),
    .unignore_manual_path_add_get_updated_end(unignore, ignore_list$end)
  )
}

.unignore_manual_path_add_get_updated_end <- function(ignore, end) {
  if (.is_len_0(end)) {
    ignore
  } else if (.is_len_1(end)) {
    end <- if (end == "") end else c(end, "")
    c(end, ignore[!ignore %in% end])
  } else {
    c(end, ignore[!ignore %in% end])
  }
}
