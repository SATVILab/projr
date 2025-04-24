#' @title Manually Ignore Files or Directories in `.gitignore` and `.Rbuildignore`
#'
#' @description
#' These functions allow manual addition of files and directories to the
#' `.gitignore` and `.Rbuildignore` files, outside of the automatic management
#' provided by the `projr` package.
#'
#' - .ignore_manual`: General function to add both files and directories
#'   to both `.gitignore` and `.Rbuildignore`. If a path does not exist, it is
#'   treated as a file.
#' - .ignore_manual_dir`: Specifically adds directories to both `.gitignore`
#'   and `.Rbuildignore`.
#' - .ignore_manual_file`: Specifically adds files to both `.gitignore`
#'   and `.Rbuildignore`.
#' - .ignore_manual_dir_git` and .ignore_manual_file_git`: Add
#'   directories or files explicitly to `.gitignore`.
#' - .ignore_manual_dir_rbuild` and .ignore_manual_file_rbuild`: Add
#'   directories or files explicitly to `.Rbuildignore`.
#'
#' @details
#' These functions provide fine-grained control for cases where users want to
#' manually ignore specific paths permanently. They do not interact with the
#' automated ignore management system of `projr`.
#' - Non-existent paths provided to .ignore_manual` are assumed to be files.
#' - For `.gitignore`, directories are automatically appended with `/**` if
#'   missing, ensuring proper Git ignore syntax.
#' - For `.Rbuildignore`, paths are converted to regular expressions using
#'   `glob2rx` for compatibility with R's build tools.
#'
#' @param ignore A character vector of file or directory paths to be ignored.
#'   Paths must be valid non-empty strings.
#' @param force_create logical.
#' If `FALSE`, then the function will only add to the corresponding
#' ignore file (`.gitignore`/`.Rbuildignore`) if it already exists, OR
#' if it is warranted
#' (i.e. if there is a Git repository or DESCRIPTION file, respectively).
#' If `TRUE`, then the function will create the ignore file
#' if it does not exist,
#' even if there is no Git repository or DESCRIPTION file,
#' and will add the specified paths to it.
#' Default is `TRUE`.
#'
#' @return
#' Invisibly returns `TRUE` if the operation succeeds, or `FALSE` if the input
#' contains invalid (empty) paths.
#'
#' @seealso
#' .ignore_auto` for dynamically managed ignore entries, and .unignore_manual`
#' for forcing certain paths to not be ignored.
#'
#' @examples
#' # Manually ignore files and directories
#' projr_ignore_manual(c("output", "tempfile.log"))
#'
#' # Specifically ignore directories
#' projr_ignore_dir("data")
#'
#' # Specifically ignore files
#' projr_ignore_file("README.md")
#'
#' @export
projr_ignore <- function(ignore, force_create = TRUE) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore_file <- ignore[fs::is_file(ignore)]
  ignore_dir <- ignore[fs::is_dir(ignore)] |>
    c(ignore[grepl("/$", ignore)]) |>
    unique()
  ignore_nonexistent <- setdiff(
    ignore, c(ignore_file, ignore_dir)
  )
  projr_ignore_file_git(c(ignore_file, ignore_nonexistent), force_create)
  projr_ignore_dir_git(ignore_dir, force_create)
  projr_ignore_file_rbuild(c(ignore_file, ignore_nonexistent), force_create)
  projr_ignore_dir_rbuild(ignore_dir, force_create)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_dir <- function(ignore, force_create = TRUE) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  projr_ignore_dir_git(ignore, force_create)
  projr_ignore_dir_rbuild(ignore, force_create)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_file <- function(ignore) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  projr_ignore_file_git(ignore)
  projr_ignore_file_rbuild(ignore)
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_file_git <- function(ignore, force_create = TRUE) {
  if (!force_create) {
    # don't add if .gitignore doesn't already exist,
    # unless there is a Git repo
    if (!fs::file_exists(.path_get(".gitignore"))) {
      if (!.git_repo_check_exists()) {
        return(invisible(FALSE))
      }
    }
  }
  if (!fs::file_exists(.path_get(".gitignore"))) {
    file.create(.path_get(".gitignore"))
    .newline_append(.path_get(".gitignore"))
  }
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  .ignore_manual_path_add(ignore, .path_get(".gitignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_dir_git <- function(ignore, force_create = TRUE) {
  if (!force_create) {
    # don't add if .gitignore doesn't already exist,
    # unless there is a Git repo
    if (!fs::file_exists(.path_get(".gitignore"))) {
      if (!.git_repo_check_exists()) {
        return(invisible(FALSE))
      }
    }
  }
  if (!fs::file_exists(.path_get(".gitignore"))) {
    file.create(.path_get(".gitignore"))
    .newline_append(.path_get(".gitignore"))
  }
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore <- vapply(ignore, function(x) {
    if (grepl("/\\*\\*$", x)) {
      x
    } else {
      paste0(x, "/**")
    }
  }, character(1L))
  .ignore_manual_path_add(ignore, .path_get(".gitignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_file_rbuild <- function(ignore, force_create = TRUE) {
  if (!force_create) {
    # don't add if rbuild doesn't already exist,
    # unless there is a DESCRIPTION file
    # (which is a sign of a package)
    if (!fs::file_exists(.path_get(".Rbuildignore"))) {
      if (!fs::file_exists(.path_get("DESCRIPTION"))) {
        return(invisible(FALSE))
      }
    }
  }
  if (!fs::file_exists(.path_get(".Rbuildignore"))) {
    file.create(.path_get(".Rbuildignore"))
    .newline_append(.path_get(".Rbuildignore"))
  }
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore <- gsub("/+$", "", ignore) |>
    trimws() |>
    utils::glob2rx()
  .ignore_manual_path_add(ignore, .path_get(".Rbuildignore"))
}

#' @rdname projr_ignore_manual
#' @export
projr_ignore_dir_rbuild <- function(ignore, force_create = TRUE) {
  if (!force_create) {
    # don't add if rbuild doesn't already exist,
    # unless there is a DESCRIPTION file
    # (which is a sign of a package)
    if (!fs::file_exists(.path_get(".Rbuildignore"))) {
      if (!fs::file_exists(.path_get("DESCRIPTION"))) {
        return(invisible(FALSE))
      }
    }
  }
  if (!fs::file_exists(.path_get(".Rbuildignore"))) {
    file.create(.path_get(".Rbuildignore"))
    .newline_append(.path_get(".Rbuildignore"))
  }
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  ignore <- gsub("/+$", "", ignore)
  ignore <- trimws(ignore)
  patterns <- utils::glob2rx(ignore)
  patterns <- gsub("\\$$", "", patterns)
  patterns <- paste0(patterns, "/")
  patterns <- lapply(seq_along(patterns), function(i) {
    c(patterns[i], utils::glob2rx(ignore[i]))
  }) |>
    unlist()
  .ignore_manual_path_add(patterns, .path_get(".Rbuildignore"))
}

# ===========================================================================
# Ignore specified lines from .gitignore/.Rbuildignore
# ===========================================================================

.ignore_manual_path_add <- function(ignore, path) {
  ignore <- setdiff(ignore, "")
  if (!.is_chr(ignore)) {
    return(invisible(FALSE))
  }
  .assert_string(path, TRUE)

  file_vec <- .ignore_manual_path_add_get_updated(path, ignore)
  .ignore_path_write(file_vec, path)
  invisible(TRUE)
}

.ignore_manual_path_add_get_updated <- function(path,
                                                ignore) {
  ignore_list <- .ignore_path_get_list(path, ignore)

  c(
    .ignore_manual_path_add_get_updated_start(ignore, ignore_list$start),
    ignore_list$content |> setdiff("") |> unique(),
    ignore_list$end
  )
}

.ignore_manual_path_add_get_updated_start <- function(ignore, start) {
  if (.is_len_0(start)) {
    ignore
  } else if (.is_len_1(start)) {
    .ignore_manual_path_add_get_updated_start_len_1(ignore, start)
  } else {
    .ignore_manual_path_add_get_updated_start_len_g1(ignore, start)
  }
}

.ignore_manual_path_add_get_updated_start_len_1 <- function(ignore, start) {
  start <- .ignore_manual_path_add_get_updated_start_len_1_start(start)
  c(ignore[!ignore %in% start], start)
}

.ignore_manual_path_add_get_updated_start_len_1_start <- function(start) {
  if (grepl(match_str_top, start)) {
    c("", start)
  } else if (start == "") {
    start
  } else {
    c(start, "")
  }
}

.ignore_manual_path_add_get_updated_start_len_g1 <- function(ignore, start) {
  ignore <- ignore[!ignore %in% start]
  start <- .ignore_manual_path_add_get_updated_start_len_g1_start(start)
  c(ignore, start)
}

.ignore_manual_path_add_get_updated_start_len_g1_start <- function(start) {
  if (grepl(match_str_top, start[length(start)])) {
    start_pre <- start[-length(start)]
    if (start_pre[length(start_pre)] != "") {
      start_pre <- c(start_pre, "")
    }
    c(start_pre, start[length(start)])
  } else {
    if (start[length(start)] != "") {
      c(start, "")
    } else {
      start
    }
  }
}
