# Exported, "large" functions
# ==============================================================================


#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' @param label character.
#' One of \code{"raw"}, \code{"cache"},\code{"output"},
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
#' @param relative logical.
#' If \code{TRUE}, then forces that the returned
#' path is relative to the project root.
#' Default is \code{FALSE}.
#' @param absolute logical.
#' If `TRUE`, then forces the returned path
#' to be absolute.
#' Default is `FALSE`.
#' @param safe logical.
#' If \code{TRUE}, then the output directory
#' is set to be \code{"<path_to_cache>.output"}
#' instead of \code{<path_to_output>} (as specified in \code{_projr.yml}).
#' The only time that this should be set to \code{TRUE}
#' should be when .build_output` is being run, as otherwise
#' "development" or test runs will add to, delete or overwrite fabciles
#' from the previous run of .build_output`.
#' Default is \code{TRUE}.
#' Do not change this unless you know what you are doing.
#' @return Character.
#' Path to directory requested.
#' @rdname projr_path_get_dir
#' @export
projr_path_get_dir <- function(label, ...,
                               create = TRUE,
                               relative = FALSE,
                               absolute = FALSE,
                               safe = TRUE) {
  dots_vec <- .dots_get_chr_vec(...)
  .dir_get_check(label, dots_vec, relative, absolute, safe)

  .dir_get(label = label, ..., safe = safe) |>
    .dir_get_create(create) |>
    .dir_get_rel(relative) |>
    .dir_get_abs(absolute)
}

.dir_get_check <- function(label, dots_list, relative, absolute, safe) {
  if (.is_len_pos(dots_list)) {
    dots_list |>
      unlist() |>
      .assert_chr()
  }
  .dir_check_label(label, NULL)
  .assert_flag(relative)
  .assert_flag(absolute)
  .assert_flag(safe)
  if (relative && absolute) {
    stop("relative and absolute cannot both be TRUE")
  }
}

.dir_check_label <- function(label, profile) {
  .assert_string(label)
  .dir_check_label_found(label, profile)
}

.dir_check_label_found <- function(label, profile) {
  opt_vec <- c(
    names(.yml_dir_get(profile)), "docs", "data", "project", "code"
  )
  label_found <- label %in% opt_vec
  if (!label_found) {
    stop("label '", label, "' not found in _projr.yml", call. = FALSE)
  }
  invisible(TRUE)
}

.dir_check_label_strip <- function(label) {
  label_strip <- .dir_label_strip(label)
  label_valid <- grepl("^docs|^raw|^cache|^output", label_strip)
  if (!label_valid) {
    stop(
      paste0("label '", label, "' not valid.\n"),
      "Must begin with 'docs', 'raw', 'cache', 'output' or 'archive'\n",
      "However, it can:\n",
      " - be capitalised any which way, e.g. RAW-DATA or rAw-dAtA)\n",
      " - have any suffix, e.g. 'raw-data-foo' or 'raw-data-foo-bar'\n",
      " - use a hyphen, underscore or neither, e.g. 'raw', 'raw-data' or rawdata\n",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.dir_label_strip <- function(x) {
  gsub("_", "", gsub("-", "", x)) |>
    tolower()
}


.dir_get_create <- function(path, create) {
  if (create) {
    .dir_create(path)
  }
  invisible(path)
}

.dir_get_rel <- function(path, relative) {
  if (!relative) {
    return(path)
  }
  .path_force_rel(path)
}

.dir_get_abs <- function(path, absolute) {
  if (!absolute) {
    return(path)
  }
  .path_force_abs(path)
}

#' @title Return path
#'
#' @description Returns path to \code{projr} profile-specific directory.
#' Differs from \code.dir_get} in that it does not assume
#' that the path is to a directory.
#'
#' Will create the parent directory of the specified
#' path if it does not exist, and ignore it
#' if requested by `_projr.yml`.
#'
#' @param label character.
#' One of \code{"raw_data"}, \code{"cache"},\code{"output"},
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
#' @inheritParams.path_get_dir
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
                           relative = FALSE,
                           absolute = FALSE,
                           safe = TRUE) {
  args_dotted <- list(...)
  if (length(args_dotted) == 0) {
    path_dir <-.path_get_dir(
      label = label,
      create = create,
      relative = relative,
      absolute = absolute,
      safe = safe
    )
    return(path_dir)
  }
  if (length(args_dotted) > 1) {
    path_dir <- do.call(
      what = ".path_get_dir",
      args = list(
        label = label,
        args_dotted[-length(args_dotted)] |> unlist(),
        create = create,
        relative = relative,
        absolute = absolute,
        safe = safe
      )
    )
  } else {
    path_dir <-.path_get_dir(
      label = label,
      create = create,
      relative = relative,
      safe = safe
    )
  }
  file.path(path_dir, args_dotted[length(args_dotted)]) |>
    fs::path_norm() |>
    as.character()
}

.dir_create <- function(label, ..., safe = TRUE) {
  # create directories
  for (x in label) {
   .path_get_dir(
      label = x,
      ...,
      create = TRUE,
      safe = safe
    )
  }
  invisible(TRUE)
}
