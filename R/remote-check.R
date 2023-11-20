# =================
# base checks
# =================

.projr_remote_check_base <- function(type, ...) {
  switch(type,
    local = .projr_remote_check_base_local(...),
    osf = .projr_remote_check_base_osf(...)
  )
  invisible(TRUE)
}

# local
.projr_remote_check_base_local <- function(path) {
  if (missing(path)) {
    stop("path must be supplied")
  }
  if (!is.character(path)) {
    stop("path must be a character vector")
  }
  invisible(TRUE)
}
# osf
.projr_remote_check_base_osf <- function(title,
                                         id,
                                         id_parent,
                                         category,
                                         body,
                                         public,
                                         parent_title,
                                         path,
                                         path_append_label) {
  .projr_remote_check_osf_title(title = title)
  .projr_remote_check_osf_public(public = public)
  .projr_remote_check_osf_body(body = body)
  .projr_remote_check_osf_category(category = category)
  .projr_remote_check_osf_id_parent(id_parent = id_parent)
  .projr_remote_check_osf_parent_title(parent_title = parent_title)
  .projr_remote_check_osf_proj_with_parent(
    id_parent = id_parent, parent_title = parent_title, category = category
  )
  .projr_remote_check_osf_path(path = path)
  .projr_remote_check_osf_path_append_label(
    path_append_label = path_append_label
  )
}


# =================
# OSF-specific
# =================
.projr_remote_check_osf_title <- function(title) {
  if (is.null(title)) {
    return(invisible(FALSE))
  }
  if (!length(title) == 1L) {
    stop("title must be a character vector of length 1")
  }
  if (!is.character(title)) {
    stop("title must be a character vector")
  }
  if (!nchar(title) > 0L) {
    stop("title must have at least one character") # nolint
  }
  invisible(TRUE)
}

.projr_remote_check_osf_label <- function(label, type_opt) {
  label_nm <- deparse(substitute(label))
  # check supplied
  if (missing(label)) {
    stop(paste0(
      label_nn, " must be supplied"
    ))
  }
  if (is.null(label)) {
    stop(paste0(
      label_nn, " must be a character vector"
    ))
  }
  # check it only matches restricted types
  short_to_match_type <- c(
    "cache" = "^cache",
    "data-raw" = "^dataraw",
    "output" = "^output",
    "archive" = "^archive",
    "docs" = "^docs"
  )
  match_str <- paste0(
    short_to_match_type[type_opt],
    collapse = "|"
  )
  if (!grepl(match_str, .projr_dir_label_strip(label))) {
    stop(paste0(
      label_nm, " entries must match the following: ",
      match_str
    ))
  }

  # check that it's actually found
  label_vec <- unique(c(
    names(projr_yml_get_unchecked()[["directories"]]),
    "docs"
  ))
  if (!all(label %in% label_vec)) {
    diff_vec <- setdiff(content, label_vec)
    stop(paste0(
      "content must match a directory key in _projr.yml. ",
      "The following do not: ",
      paste0('"', diff_vec, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}
.projr_remote_check_osf_public <- function(public) {
  if (!is.logical(public)) {
    stop("public must be logical")
  }
  invisible(TRUE)
}
.projr_remote_check_osf_body <- function(body) {
  body_invalid <- (!is.null(body)) &&
    (!all(is.character(body)) || length(body) != 1)
  if (body_invalid) {
    stop("body must be a character vector (with one element)")
  }
  invisible(TRUE)
}
.projr_remote_check_osf_category <- function(category) {
  category_vec_valid <- c(
    "analysis",
    "communication",
    "data",
    "hypothesis",
    "instrumentation",
    "methods and measures",
    "procedure",
    "project",
    "software",
    "other"
  )
  if (is.null(category)) {
    return(invisible(FALSE))
  }
  if (!length(category) == 1L) {
    stop("category must be a character vector of length 1")
  }
  if (!is.character(category)) {
    stop("category must be a character vector")
  }
  if (!all(category %in% category_vec_valid)) {
    stop(paste0(
      "category must be one of: ",
      paste0('"', category_vec_valid, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}
.projr_remote_check_osf_id <- function(id) {
  if (is.null(id)) {
    return(invisible(FALSE))
  }
  if (!length(id) == 1L) {
    stop("id must be a character vector of length 1")
  }
  if (!is.character(id)) {
    stop("id must be a character vector")
  }
  if (!nchar(id) == 5L) {
    stop("id must have five characters") # nolint
  }
  invisible(TRUE)
}
.projr_remote_check_osf_id_parent <- function(id_parent) {
  if (is.null(id_parent)) {
    return(invisible(FALSE))
  }
  if (!length(id_parent) == 1L) {
    stop("id_parent must be a character vector of length 1")
  }
  if (!is.character(id_parent)) {
    stop("id_parent must be a character vector")
  }
  if (!nchar(id_parent) == 5L) {
    stop("id_parent must have five characters") # nolint
  }
  invisible(TRUE)
}
.projr_remote_check_osf_parent_title <- function(parent_title) {
  if (is.null(parent_title)) {
    return(invisible(FALSE))
  }
  if (!length(parent_title) == 1L) {
    stop("parent_title must be a character vector of length 1")
  }
  if (!is.character(parent_title)) {
    stop("parent_title must be a character vector")
  }
  if (!nchar(parent_title) > 0L) {
    stop("parent_title must have at least one character") # nolint
  }
  invisible(TRUE)
}
.projr_remote_check_osf_proj_with_parent <- function(id_parent = NULL,
                                                     parent_title = NULL,
                                                     category) {
  if (is.null(category)) {
    return(invisible(FALSE))
  }
  if (category != "project") {
    return(invisible(FALSE))
  }

  if (!is.null(id_parent) || !is.null(parent_title)) {
    stop(paste0(
      "id_parent and parent_title must be NULL when category is project"
    ))
  }
  invisible(TRUE)
}

.projr_remote_check_osf_overwrite <- function(overwrite) {
  if (!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }
  invisible(TRUE)
}
.projr_remote_check_osf_path <- function(path) {
  if (is.null(path)) {
    return(invisible(FALSE))
  }
  if (!length(path) == 1L) {
    stop("path must be a character vector of length 1")
  }
  if (!is.character(path)) {
    stop("path must be a character vector")
  }
  if (!nchar(path) > 0L) {
    stop("path must have at least one character") # nolint
  }
  invisible(TRUE)
}

.projr_remote_check_osf_path_append_label <- function(path_append_label) {
  if (is.null(path_append_label)) {
    return(invisible(FALSE))
  }
  if (!length(path_append_label) == 1L) {
    stop("path_append_label must be a logical vector of length 1")
  }
  if (!is.logical(path_append_label)) {
    stop("path_append_label must be a logical vector")
  }
  invisible(TRUE)
}
.projr_remote_check_osf_remote_structure <- function(remote_structure,
                                                     nm_opt = NULL) {
  if (is.null(remote_structure)) {
    return(invisible(FALSE))
  }
  if (is.null(nm_opt)) {
    nm_opt <- c("latest", "version", "content")
  }
  diff_vec <- setdiff(remote_structure, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      "remote_structure options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}
# transfer
# ----------------------

.projr_remote_check_osf_trans_list <- function(trans_list) {
  direction <- deparse(substitute(trans_list))
  if (!is.list(trans_list)) {
    stop(paste0(direction, " options must be given as a list"))
  }
  invisible(TRUE)
}

.projr_remote_check_osf_trans_names <- function(trans_list,
                                                nm_opt = NULL,
                                                nm_opt_exc = NULL) {
  if (is.null(nm_opt)) {
    nm_opt <- c("cue", "sync-approach", "version-course", "conflict")
  }
  if (!is.null(nm_opt_exc)) {
    nm_opt <- setdiff(nm_opt, nm_opt_exc)
  }
  direction <- deparse(substitute(trans_list))
  diff_vec <- setdiff(names(trans_list), nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      direction,
      " options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.projr_remote_check_osf_cue <- function(trans_list,
                                        nm_opt = NULL) {
  direction <- deparse(substitute(trans_list))
  cue_vec <- trans_list[["cue"]]
  if (is.null(cue_vec)) {
    return(invisible(FALSE))
  }
  if (is.null(nm_opt)) {
    nm_opt <- c("none", "build", "major", "minor", "patch")
  }
  diff_vec <- setdiff(cue_vec, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      direction,
      " cue options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.projr_remote_check_osf_sync_approach <- function(trans_list,
                                                  nm_opt = NULL) {
  direction <- deparse(substitute(trans_list))
  sync_approach_vec <- trans_list[["sync-approach"]]
  if (is.null(sync_approach_vec)) {
    return(invisible(FALSE))
  }
  if (is.null(nm_opt)) {
    stop("nm_opt must be supplied for checking sync_approach")
  }
  diff_vec <- setdiff(sync_approach_vec, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      direction,
      " sync_approach options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.projr_remote_check_osf_conflict <- function(trans_list,
                                             nm_opt = NULL) {
  direction <- deparse(substitute(trans_list))
  conflict_vec <- trans_list[["conflict"]]
  if (is.null(nm_opt)) {
    nm_opt <- c("error", "overwrite", "skip")
  }
  diff_vec <- setdiff(conflict_vec, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      direction,
      " conflict options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.projr_osf_yml_version_source <- function(trans_list,
                                          nm_opt = NULL) {
  direction <- deparse(substitute(trans_list))
  version_source_vec <- trans_list[["version-source"]]
  if (is.null(version_source_vec)) {
    return(invisible(FALSE))
  }
  if (is.null(nm_opt)) {
    nm_opt <- c("manifest", "osf")
  }
  diff_vec <- setdiff(version_source_vec, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      direction,
      " version_source options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
}
