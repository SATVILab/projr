#' @title Return path to profile-specific directory
#' @description Returns path to \code{projr} profile-specific directory.
#' Also creates the directory if it does not exist, and
#' ignores it if requested by `_projr.yml`.
#' If the path is relative, then it is calculated with respect to the project directory, but returned relative to the current working directory.
#' @param label character.
#' A directory label declared under `directories` in the active project configuration.
#' Each label must have a `path`, which may be inherited from another profile.
#' @param ... Specifies sub-directory of directory returned.
#' Passed to `file.path`.
#' @param create logical.
#' If \code{TRUE}, then the directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @param format character.
#' One of \code{"auto"}, \code{"relative"} or \code{"absolute"}.
#' If \code{"auto"}, then the format of the path returned
#' is inherited from the format of the path specified in \code{_projr.yml}.
#' If \code{"relative"}, then the path returned is relative to the current working directory.
#' If \code{"absolute"}, then the path returned is absolute.
#' @param safe logical.
#' When `TRUE` (the default), stage outputs, documentation and package data under
#' the configured cache for the current build version. When `FALSE`, return
#' their configured final paths. Inputs and cache directories use their configured paths.
#' @return Character.
#' Path to directory requested.
#' @rdname projr_path_get_dir
#' @export
projr_path_get_dir <- function(label, ...,
                               create = TRUE,
                               format = c("auto", "relative", "absolute"),
                               safe = TRUE) {
  dots_vec <- .dots_get_chr_vec(...)
  format <- match.arg(format)

  .assert_flag(create, TRUE)
  .dir_get_check(label, dots_vec, safe)

  # Get the raw path (inheriting the format from config)
  path_raw <- .dir_get(label = label, ..., safe = safe)
  
  # Safely create the directory (resolves to absolute under the hood)
  .dir_get_create(path_raw, create)
  
  # Determine the inherited format from the YAML
  is_yaml_abs <- all(fs::is_absolute_path(path_raw))
  path_abs <- .path_resolve_root(path_raw)
  
  switch(format,
    "absolute" = path_abs,
    "relative" = .path_force_rel(path_abs, getwd()),
    "auto"     = {
      if (is_yaml_abs) path_abs else .path_force_rel(path_abs, getwd())
    }
  )
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
#' A directory label declared under `directories` in the active project configuration.
#' @param ... Specifies sub-path of directory returned.
#' @param create logical.
#' If \code{TRUE}, then the parent directory
#' is created if it does not exist and
#' it is ignored (or not) from \code{.gitignore}
#' and \code{.Rbuildignore} as specified
#' in \code{_projr.yml}.
#' Default is \code{TRUE}.
#' @inheritParams projr_path_get_dir
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
                           format = c("auto", "relative", "absolute"),
                           safe = TRUE) {
  args_dotted <- list(...)

  if (length(args_dotted) == 0) {
    path_dir <- projr_path_get_dir(
      label = label,
      create = create,
      format = format,
      safe = safe
    )
    return(path_dir)
  }
  path_dir <- do.call(
    projr_path_get_dir,
    c(list(label = label), args_dotted[-length(args_dotted)],
      list(create = create, format = format, safe = safe))
  )
  file.path(path_dir, args_dotted[length(args_dotted)]) |>
    fs::path_norm() |>
    as.character()
}

.dir_check_label <- function(label, profile) {
  .assert_string(label, TRUE)
  .dir_check_label_found(label, profile)
}

.dir_check_label_found <- function(label, profile) {
  missing <- setdiff(label, names(.yml_dir_get(profile)))
  if (length(missing)) {
    stop("label '", paste(missing, collapse = "', '"), "' not found in _projr.yml", call. = FALSE)
  }
  invisible(TRUE)
}

.dir_check_label_strip <- function(label) {
  label_strip <- .dir_label_strip(label)
  label_valid <- grepl("^docs|^raw|^cache|^output", label_strip) || label %in% c("project", "data", "code")
  if (!label_valid) {
    stop(
      paste0("label '", label, "' not valid.\n"),
      "Use 'project', 'code', 'data' or a label beginning with 'docs', 'raw', 'cache' or 'output'\n",
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


.dir_get_check <- function(label, dots_list, safe) {
  if (.is_len_pos(dots_list)) {
    dots_list |>
      unlist() |>
      .assert_chr()
  }
  .assert_string(label, TRUE)
  .assert_flag(safe, TRUE)
  .yml_dir_get_path(label, NULL)
}

.dir_get_create <- function(path, create) {
  if (create) {
    # Resolve to absolute just for creation to prevent Quarto working-directory bugs
    # This safely relies on the .dir_create(path_dir) function located in path.R
    .dir_create(.path_resolve_root(path))
  }
  invisible(path)
}

# Renamed from .dir_create to avoid shadowing the system path creator in path.R
.dir_create_by_label <- function(label, ..., safe = TRUE) {
  # create directories by their projr labels
  for (x in label) {
    projr_path_get_dir(
      label = x,
      ...,
      create = TRUE,
      safe = safe
    )
  }
  invisible(TRUE)
}
