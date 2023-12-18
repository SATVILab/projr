.projr_yml_remote_check <- function(role,
                                    type,
                                    title = NULL,
                                    content = NULL,
                                    structure = NULL,
                                    path = NULL,
                                    path_append_label = NULL,
                                    overwrite = FALSE,
                                    public = FALSE,
                                    category = NULL,
                                    description = NULL,
                                    id = NULL,
                                    id_parent = NULL,
                                    title_parent = NULL,
                                    get_list = NULL,
                                    send_list = NULL) {
  # checks
  # ----------

  # common
  .projr_yml_remote_check_title(title = title, content = content)
  .projr_yml_remote_check_content(
    type = type, category = category, content = content
  )
  .projr_yml_remote_check_path(type = type, path = path)
  .projr_yml_remote_check_path_append_label(
    path_append_label = path_append_label
  )
  .projr_yml_remote_check_overwrite(overwrite = overwrite)
  .projr_yml_remote_check_structure(type = type, structure = structure)
  .projr_yml_remote_check_trans_list(trans_list = get_list)
  .projr_yml_remote_check_trans_names(
    trans_list = get_list, nm_opt = c("sync-approach", "conflict")
  )
  .projr_yml_remote_check_sync_approach(
    trans_list = get_list,
    nm_opt = c(
      "download-all",
      "delete-then-download-all",
      "download-missing"
    )
  )
  .projr_yml_remote_check_cue(trans_list = get_list)
  .projr_yml_remote_check_conflict(trans_list = get_list)
  .projr_yml_remote_check_sync_approach(
    trans_list = get_list,
    nm_opt = c(
      "upload-missing",
      "upload-all",
      "sync-using-deletion",
      "sync-using-version" # haven't implemented this one yet
    )
  )
  .projr_yml_remote_check_version_source(trans_list = get_list)
  .projr_yml_remote_check_trans_list(trans_list = send_list)
  .projr_yml_remote_check_trans_names(trans_list = send_list)
  .projr_yml_remote_check_cue(trans_list = send_list)
  .projr_yml_remote_check_conflict(trans_list = send_list)
  .projr_yml_remote_check_version_source(trans_list = send_list)

  # osf and github only
  .projr_yml_remote_check_description(type = type, description = description)

  # osf only
  .projr_yml_remote_check_public(type = type, public = public)
  .projr_yml_remote_check_category(type = type, category = category)
  .projr_yml_remote_check_id(type = type, id = id)
  .projr_yml_remote_check_id_parent(type = type, id_parent = id_parent)
  .projr_yml_remote_check_title_parent(
    type = type,
    title_parent = title_parent
  )
}

# all remote types
# ---------------------

# title
.projr_yml_remote_check_title <- function(title, content) {
  if (is.null(title)) {
    if (!missing(content)) {
      if (is.null(content)) {
        stop("title must be supplied if content is given")
      }
      .projr_yml_remote_check_content(content = content)
    }
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

# content
.projr_yml_remote_check_content <- function(type,
                                            category,
                                            content) {
  ..assert_chr(content, TRUE)
  if (!all(is.character(content))) {
    stop("content must be a character vector")
  }
  dir_vec <- projr_yml_get_unchecked()[["directories"]] |>
    names()
  opt_vec <- dir_vec |>
    c("code", "docs") |>
    unique()

  if (!all(content %in% opt_vec)) {
    stop(paste0(
      "all content must be one of: ",
      paste0('"', opt_vec, '"', collapse = ", ")
    ))
  }
  invisible(TRUE)
}

# label
.projr_yml_remote_check_label <- function(label) {
  label_nm <- deparse(substitute(label))
  # check supplied
  if (missing(label)) {
    stop(paste0(
      label_nn, " must be supplied"
    ))
  }
  if (is.null(label)) {
    stop(paste0(
      label_nm, " must be a character vector"
    ))
  }
  # check it only matches restricted types
  short_to_match_type <- c(
    "cache" = "^cache",
    "data-raw" = "^dataraw",
    "output" = "^output",
    "archive" = "^archive",
    "docs" = "^docs",
    "code" = "^code"
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

# path
.projr_yml_remote_check_path <- function(type, path) {
  .projr_yml_remote_check_path_required(type = type, path = path)
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

.projr_yml_remote_check_path_required <- function(type, path) {
  if (!type == "local") {
    return(invisible(FALSE))
  }
  if (is.null(path)) {
    stop("path must be supplied if remote is local")
  }
  if (!length(path) == 1L) {
    stop("path must be a character vector of length 1")
  }
}

# path_append_label
.projr_yml_remote_check_path_append_label <- function(type,
                                                      path_append_label) {
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

# overwrite
.projr_yml_remote_check_overwrite <- function(overwrite) {
  if (!length(overwrite) == 1L) {
    stop("overwrite must be a logical vector of length 1")
  }
  if (!is.logical(overwrite)) {
    stop("overwrite must be logical")
  }
  invisible(TRUE)
}

# structure
.projr_yml_remote_check_structure <- function(type,
                                              structure,
                                              nm_opt = NULL) {
  if (is.null(structure)) {
    return(invisible(FALSE))
  }
  if (is.null(nm_opt)) {
    nm_opt <- switch(type,
      # github could noever be content-addressable
      "github" = c("latest", "version"),
      c("latest", "version", "content")
    )
  }
  diff_vec <- setdiff(structure, nm_opt)
  if (length(diff_vec) > 0) {
    stop(paste0(
      "structure options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", "),
      "for remote type ", type
    ))
  }
  invisible(TRUE)
}

# trans options
.projr_yml_remote_check_trans_list <- function(trans_list) {
  direction <- deparse(substitute(trans_list))
  if (!is.list(trans_list)) {
    stop(paste0(direction, " options must be given as a list"))
  }
  invisible(TRUE)
}

# names of trans options
.projr_yml_remote_check_trans_names <- function(trans_list,
                                                nm_opt = NULL,
                                                nm_opt_exc = NULL) {
  if (is.null(nm_opt)) {
    nm_opt <- c("cue", "sync-approach", "version-source", "conflict")
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

# cue
.projr_yml_remote_check_cue <- function(trans_list,
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

# sync approach
.projr_yml_remote_check_sync_approach <- function(trans_list,
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

# conflict
.projr_yml_remote_check_conflict <- function(trans_list,
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

# version source
.projr_yml_remote_check_version_source <- function(trans_list,
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
  if (.is_len_pos(diff_vec)) {
    stop(paste0(
      direction,
      " version_source options must be one of: ",
      paste0('"', nm_opt, '"', collapse = ", ")
    ))
  }
}

# restricted
# ----------------------

# description (osf and github)
.projr_yml_remote_check_description <- function(type, description) {
  switch(type,
    "github" = ,
    "osf" = .projr_yml_remote_check_description_actual(
      description = description
    ),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_description_actual <- function(description) {
  if (is.null(description)) {
    return(invisible(FALSE))
  }
  if (!all(is.character(description))) {
    stop("description must be a character vector")
  }
  if (!length(description) == 1L) {
    stop("description must be a character vector of length 1")
  }
  invisible(TRUE)
}

# public (osf)
.projr_yml_remote_check_public <- function(type, public) {
  switch(type,
    "osf" = .projr_yml_remote_check_public_actual(public),
    FALSE
  )
}

# category (osf)
.projr_yml_remote_check_public_actual <- function(public) {
  if (!all(is.logical(public))) {
    stop("public must be logical")
  }
  if (!length(public) == 0L) {
    stop("public must be a logical vector of length 1")
  }
  invisible(TRUE)
}

# category (osf)
.projr_yml_remote_check_category <- function(type, category) {
  switch(type,
    "osf" = .projr_yml_remote_check_category_actual(ategory),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_category_actual <- function(category) {
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

# id (osf)
.projr_yml_remote_check_id <- function(type, id) {
  switch(type,
    "osf" = .projr_yml_remote_check_id_actual(id),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_id_actual <- function(id) {
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

# id_parent (osf)
.projr_yml_remote_check_id_parent <- function(type, id_parent) {
  switch(type,
    "osf" = .projr_yml_remote_check_id_parent_actual(id_parent),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_id_parent_actual <- function(id_parent) {
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

# title_parent (osf)
.projr_yml_remote_check_title_parent <- function(type, title_parent) {
  switch(type,
    "osf" = .projr_yml_remote_check_title_parent_actual(title_parent),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_title_parent_actual <- function(title_parent) {
  if (is.null(title_parent)) {
    return(invisible(FALSE))
  }
  if (!length(title_parent) == 1L) {
    stop("title_parent must be a character vector of length 1")
  }
  if (!is.character(title_parent)) {
    stop("title_parent must be a character vector")
  }
  if (!nchar(title_parent) > 0L) {
    stop("title_parent must have at least one character") # nolint
  }
}

# project with parent (osf)
.projr_yml_remote_check_proj_with_parent <- function(type,
                                                     title_parent,
                                                     id_parent,
                                                     category) {
  switch(type,
    "osf" = .projr_yml_remote_check_proj_with_parent_actual(
      title_parent = title_parent, id_parent = id_parent, category = category
    ),
    invisible(FALSE)
  )
}

.projr_yml_remote_check_proj_with_parent_actual <- function(id_parent,
                                                            title_parent,
                                                            category) {
  if (is.null(category)) {
    return(invisible(FALSE))
  }
  if (category != "project") {
    return(invisible(FALSE))
  }

  if (!is.null(id_parent) || !is.null(title_parent)) {
    stop(paste0(
      "id_parent and title_parent must be NULL when category is project"
    ))
  }
  invisible(TRUE)
}
