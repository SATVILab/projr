#' @title Add an OSF node as a destination
#'
#' @description
#' Add a project or component to the _projr.yml
#' specification for OSF.
#'
#' @param title character. The title of the project or component.
#' Must be supplied.
#' @param create logical. Whether to create the project or component on OSF.
#' Default is `FALSE`.
#' @param body character. Description on OSF. Default is `NULL`.
#' @param content character vector. Keys to include in the OSF upload.
#' @param public logical. Whether the project or component is public on OSF.
#' Default is `FALSE`.
#' @param category character. The category of the project or component.
#' The following are valid options: `"project"`, `"analysis"`,
#' `"communication"`, `"data"`, `"hypothesis"`, `"instrumentation"`,
#' `"methods and measures"`, `"procedure"`, `"project"`, `"software"` and other
#' `"other"`. Default is `NULL.`
#' @param id character. The id of the project or component. Must be five
#' characters. Default is `NULL`.
#' @param parent_id character. The id of the parent project or component.
#' Must be five characters. Default is `NULL`.
#' @param parent_title character. The title of the parent project or component.
#' Default is `NULL`.
#' @param overwrite logical. If `TRUE`, then the project or component
#' will be overwitten if it exists.
#' Note that any components of the project/component
#' will be deleted from the YAML file. If `FALSE`, then an error will be thrown.
#' Default is `FALSE`.
#' element in
#' @export
projr_osf_dest_add <- function(title,
                               content = NULL,
                               id = NULL,
                               body = NULL,
                               public = FALSE,
                               category = NULL,
                               parent_id = NULL,
                               parent_title = NULL,
                               path = NULL,
                               path_append_label = NULL,
                               remote_structure = NULL,
                               download_sync_approach = NULL,
                               download_conflict = NULL,
                               # download_cue has to be manual
                               upload_cue = NULL,
                               upload_sync_approach = NULL,
                               upload_version_source = NULL,
                               upload_conflict = NULL,
                               overwrite = FALSE) {
  # format inputs
  # ------------
  if (missing(title)) {
    if (all(is.character(content))) {
      title <- paste0(content, collapse = "-")
    } else {
      stop("title must be specified if no content is given")
    }
  }

  yml_projr <- projr_yml_get_unchecked()

  download_list <- .projr_osf_yml_add_load_get_list(
    sync_approach = download_sync_approach,
    conflict = download_conflict
  )

  upload_list <- .projr_osf_yml_add_load_get_list(
    cue = upload_cue,
    sync_approach = upload_sync_approach,
    version_source = upload_version_source,
    conflict = upload_conflict
  )

  .projr_osf_dest_add_check( # nolint
    id = id,
    title = title,
    parent_id = parent_id,
    parent_title = parent_title,
    content = content,
    body = body,
    public = public,
    category = category,
    path = path,
    path_append_label = path_append_label,
    remote_structure = remote_structure,
    download = download_list,
    upload = upload_list,
    overwrite = overwrite
  )


  # create list to add
  # ------------------
  list_add <- .projr_osf_dest_get_list_add(
    title = title,
    id = id,
    remote_structure = remote_structure,
    path = path,
    path_append_label = path_append_label,
    download_list = download_list,
    upload_list = upload_list
  )

  # find where to add list, and add it
  # ----------------------------------

  # extract pre-existing osf config
  yml_projr_osf <- yml_projr[["build"]][["osf"]]

  # if category is not explicitly a project, then find parent
  is_component <- is.null(category) || category != "project"
  if (is_component) {
    # get what its parent is
    parent_vec <- .projr_osf_yml_get_parent_vec(
      parent_id = parent_id,
      parent_title = parent_title,
      yml_projr_osf = yml_projr_osf
    )
    # get the parent's id
    parent_id <- .projr_osf_dest_add_get_parent_id(
      parent_id = parent_id,
      parent_vec = parent_vec,
      yml_projr_osf = yml_projr_osf
    )
    # create node (if necessary) and get id
    id <- .projr_osf_get_node_as_node(
      title = title,
      id = id,
      parent_id = parent_id,
      category = category,
      body = body,
      public = public
    )[["id"]][[1]]
    list_add[[1]][["id"]] <- id

    if (length(parent_vec) == 0L) {
      # no parent
      yml_projr_osf <- .projr_osf_yml_dest_add_comp_root(
        yml_projr_osf = yml_projr_osf,
        overwrite = overwrite,
        title = title,
        list_add = list_add,
        parent_id = parent_id
      )
    } else {
      # if specified, add to parent
      yml_projr_osf <- .projr_osf_yml_dest_add_comp_parent(
        yml_projr_osf = yml_projr_osf,
        overwrite = overwrite,
        title = title,
        parent_vec = parent_vec,
        list_add = list_add
      )
    }
  } else {
    # if project, then add to top level
    id <- .projr_osf_get_node_as_node(
      title = title,
      id = id,
      category = "project",
      body = body,
      public = public
    )[["id"]][[1]]
    list_add[[1]][["id"]] <- id
    yml_projr_osf <- .projr_osf_yml_dest_add_project(
      yml_projr_osf = yml_projr_osf,
      list_add = list_add,
      title = title,
      overwrite = overwrite
    )
  }
  yml_projr[["build"]][["osf"]] <- yml_projr_osf
  .projr_yml_set(yml_projr)
  id
}

.projr_osf_dest_add_check <- function(id,
                                      title,
                                      parent_id,
                                      parent_title,
                                      content,
                                      body,
                                      public,
                                      category,
                                      path = NULL,
                                      path_append_label = NULL,
                                      remote_structure = NULL,
                                      download,
                                      upload,
                                      overwrite) {
  .projr_osf_yml_check_title(title = title)
  # no requirement that there is content
  # if it's a project
  if (category != "project") {
    .projr_osf_yml_check_label(
      label = content,
      type_opt = c("data-raw", "cache", "output", "archive", "docs")
    )
  } else if (!is.null(content)) {
    .projr_osf_yml_check_label(
      label = content,
      type_opt = c("data-raw", "cache", "output", "archive", "docs")
    )
  }

  .projr_osf_yml_check_public(public = public)
  .projr_osf_yml_check_body(body = body)
  .projr_osf_yml_check_category(category = category)
  .projr_osf_yml_check_parent_id(parent_id = parent_id)
  .projr_osf_yml_check_parent_title(parent_title = parent_title)
  .projr_osf_yml_check_proj_with_parent(
    parent_id = parent_id, parent_title = parent_title, category = category
  )
  .projr_osf_yml_check_overwrite(overwrite = overwrite)
  .projr_osf_yml_check_path(path = path)
  .projr_osf_yml_check_path_append_label(
    path_append_label = path_append_label
  )
  .projr_osf_yml_check_remote_structure(
    remote_structure = remote_structure,
    nm_opt = c("latest", "version", "content")
  )

  # download
  # ---------------------
  .projr_osf_yml_check_trans_list(trans_list = download)
  .projr_osf_yml_check_trans_names(
    trans_list = download,
    nm_opt = c("sync_approach", "conflict")
  )
  .projr_osf_yml_check_cue(
    trans_list = download,
    nm_opt = c("none", "build", "major", "minor", "patch")
  )
  .projr_osf_yml_check_sync_approach(
    trans_list = download,
    nm_opt = c(
      "download-all",
      "delete-then-download-all",
      "download-missing" # haven't implemented this one yet
    )
  )
  # upload
  # -----------------------
  .projr_osf_yml_check_trans_list(trans_list = upload)
  .projr_osf_yml_check_trans_names(
    trans_list = upload,
    nm_opt = c("cue", "sync-approach", "version-source", "conflict")
  )
  .projr_osf_yml_check_cue(
    trans_list = upload,
    nm_opt = c("none", "build", "major", "minor", "patch", "change")
  )
  .projr_osf_yml_check_sync_approach(
    trans_list = upload,
    nm_opt = c(
      "upload-all",
      "delete-then-upload-all",
      "upload-missing", # haven't implemented this one yet
      "upload-changed" # haven't implemented this one yet
    )
  )
  .projr_osf_yml_check_conflict(trans_list = upload)
}

.projr_osf_dest_get_list_add <- function(title,
                                         id,
                                         path,
                                         path_append_label,
                                         remote_structure,
                                         download_list,
                                         upload_list) {
  list_add <- list()
  if (!is.null(id)) {
    list_add[["id"]] <- id
  }
  if (!is.null(path)) {
    list_add[["path"]] <- path
  }
  if (!is.null(path_append_label)) {
    list_add[["path_append_label"]] <- path_append_label
  }
  if (!is.null(remote_structure)) {
    list_add[["remote_structure"]] <- remote_structure
  }
  if (!length(download_list) == 0L) {
    list_add[["download"]] <- download_list
  }
  if (!length(upload_list) == 0L) {
    list_add[["upload"]] <- upload_list
  }

  list(list_add) |> stats::setNames(title)
}

.projr_osf_yml_get_parent_vec <- function(parent_id,
                                          parent_title,
                                          yml_projr_osf) {
  parent_specified <- !is.null(parent_id) || !is.null(parent_title)
  if (!parent_specified) {
    return(character())
  }
  parent_vec <- .projr_osf_yml_find_parent(
    parent_id = parent_id,
    parent_title = parent_title,
    yml_projr_osf = yml_projr_osf
  )
  if (is.null(parent_id) && length(parent_vec) == 0L) {
    stop(paste0(
      "parent_id must be specified for components with no parent in build OSF hierarchy" # nolint
    ))
  }
  parent_vec
}

.projr_osf_yml_find_parent <- function(parent_id = NULL,
                                       parent_title = NULL,
                                       yml_projr_osf = NULL,
                                       parent_search = NULL) {
  .projr_osf_yml_find_parent_rec(
    parent_id = parent_id,
    parent_title = parent_title,
    yml_projr_osf = yml_projr_osf,
    parent_search = parent_search
  ) |>
    unlist()
}

.projr_osf_yml_find_parent_rec <- function(parent_id = NULL,
                                           parent_title = NULL,
                                           yml_projr_osf = NULL,
                                           parent_search = NULL) {
  if (is.null(parent_search)) {
    yml_projr_osf_lvl <- yml_projr_osf
  } else {
    yml_projr_osf_lvl_chr <- paste0(
      "yml_projr_osf[[",
      paste0(paste0('"', parent_search, '"'), collapse = "]][["),
      "]]"
    )
    yml_projr_osf_lvl <- eval(parse(text = yml_projr_osf_lvl_chr))
  }
  # return NULL for no parent found

  # no parent found if no parent info provided
  if (is.null(parent_id) && is.null(parent_title)) {
    return(invisible(character()))
  }

  # no parent found if there are no previous osf entries
  if (is.null(yml_projr_osf)) {
    yml_projr_osf <- projr_yml_get_unchecked()[["build"]][["osf"]]
    if (is.null(yml_projr_osf)) {
      return(invisible(character()))
    }
  }

  # To do:
  # check that names are globally unique

  lapply(
    names(yml_projr_osf_lvl),
    function(x) {
      # look within this level
      if (!is.null(parent_id)) {
        if (identical(yml_projr_osf_lvl[[x]][["id"]], parent_id)) {
          return(invisible(c(parent_search, x)))
        }
      }
      if (!is.null(parent_title)) {
        if (identical(x, parent_title)) {
          return(invisible(c(parent_search, x)))
        }
      }
      # if there are no components, then return nothing
      if (!"component" %in% names(yml_projr_osf_lvl[[x]])) {
        return(invisible(character()))
      }

      .projr_osf_yml_find_parent_rec(
        parent_id = parent_id,
        parent_title = parent_title,
        yml_projr_osf = yml_projr_osf,
        parent_search = c(parent_search, x, "component")
      )
    }
  )
}

.projr_osf_yml_add_comp_root_check_present <- function(overwrite,
                                                       yml_projr_osf,
                                                       title) {
  if (title %in% names(yml_projr_osf)) {
    if (!overwrite) {
      stop("title already exists in _projr.yml")
    } else {
      title_present <- TRUE
    }
  } else {
    title_present <- FALSE
  }
}

#' @rdname projr_osf_dest_add
#' @export
projr_osf_dest_add_proj <- function(title,
                                    body = NULL,
                                    content = NULL,
                                    public = FALSE,
                                    id = NULL) {
  projr_osf_dest_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = "project",
    id = id
  )
}

#' @rdname projr_osf_dest_add
#' @export
projr_osf_dest_add_comp <- function(title,
                                    body = NULL,
                                    content = NULL,
                                    public = FALSE,
                                    category = NULL,
                                    parent_title = NULL,
                                    parent_id = NULL,
                                    id = NULL) {
  if (missing(parent_id) && missing(parent_title)) {
    stop("either parent_id or parent_title must be specified")
  }
  projr_osf_dest_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = category,
    parent_id = parent_id,
    parent_title = parent_title,
    id = id
  )
}

.projr_osf_yml_dest_add_comp_root <- function(yml_projr_osf,
                                              overwrite,
                                              title,
                                              list_add,
                                              parent_id) {
  # need to know if parent is already
  # there with the title so that we
  # can overwrite it
  .projr_osf_yml_add_comp_root_check_present(
    overwrite = overwrite,
    yml_projr_osf = yml_projr_osf,
    title = title
  )
  yml_projr_osf[[title]] <- list_add[[1]]
  yml_projr_osf
}



.projr_osf_yml_dest_add_project <- function(yml_projr_osf,
                                            list_add,
                                            title,
                                            overwrite) {
  if (title %in% names(yml_projr_osf) && !overwrite) {
    stop("title already exists in _projr.yml")
  }
  yml_projr_osf[[title]] <- list_add[[title]]
  yml_projr_osf
}

.projr_osf_yml_dest_add_comp_parent <- function(yml_projr_osf,
                                                parent_vec,
                                                overwrite,
                                                title,
                                                list_add) {
  # get parent to add to
  yml_projr_osf_parent_chr <-
    paste0(
      "yml_projr_osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(
    text = paste0("yml_projr_osf_parent <- ", yml_projr_osf_parent_chr)
  ))
  if (title %in% names(yml_projr_osf_parent[["component"]])) {
    if (!overwrite) {
      stop("title already exists in _projr.yml")
    }
  }
  yml_projr_osf_parent[["component"]][[title]] <-
    list_add[[1]]
  yml_projr_osf_chr <-
    paste0(
      "yml_projr_osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(text = paste0(yml_projr_osf_chr, " <- yml_projr_osf_parent")))
  yml_projr_osf
}

# get the parent_id, I think
.projr_osf_dest_add_get_parent_id <- function(parent_id,
                                              parent_vec,
                                              yml_projr_osf) {
  if (!is.null(parent_id)) {
    return(parent_id)
  }
  if (length(parent_vec) == 0L) {
    return(NULL)
  }
  yml_projr_osf_parent_chr <-
    paste0(
      "yml_projr_osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(
    text = paste0(
      "yml_projr_osf_parent <- ", yml_projr_osf_parent_chr
    )
  ))
  yml_projr_osf_parent[["id"]]
}
