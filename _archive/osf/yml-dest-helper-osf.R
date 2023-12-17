.projr_yml_dest_add_add_osf <- function(category,
                                        title_parent,
                                        id_parent,
                                        id,
                                        body,
                                        public,
                                        overwrite) {
  # extract pre-existing osf config
  yml_projr_osf <- projr_yml_get_unchecked()[["build"]][["osf"]]

  # if category is not explicitly a project, then find parent
  is_component <- is.null(category) || category != "project"
  if (is_component) {
    # get what its parent is
    parent_vec <- .projr_dest_add_list_get_osf_parent_vec(
      id_parent = id_parent,
      title_parent = title_parent,
      yml_projr_osf = yml_projr_osf
    )
    # get the parent's id
    id_parent <- .projr_dest_add_osf_get_id_parent(
      id_parent = id_parent,
      parent_vec = parent_vec,
      yml_projr_osf = yml_projr_osf
    )
    # create node (if necessary) and get id
    id <- .projr_osf_get_node_as_node(
      title = title,
      id = id,
      id_parent = id_parent,
      category = category,
      body = body,
      public = public
    )[["id"]][[1]]
    list_add[[1]][["id"]] <- id

    if (length(parent_vec) == 0L) {
      # no parent
      yml_projr_osf <- .projr_dest_add_list_get_osf_dest_add_comp_root(
        yml_projr_osf = yml_projr_osf,
        overwrite = overwrite,
        title = title,
        list_add = list_add,
        id_parent = id_parent
      )
    } else {
      # if specified, add to parent
      yml_projr_osf <- .projr_dest_add_list_get_osf_dest_add_comp_parent(
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
    yml_projr_osf <- .projr_dest_add_list_get_osf_dest_add_project(
      yml_projr_osf = yml_projr_osf,
      list_add = list_add,
      title = title,
      overwrite = overwrite
    )
  }
  # to write final list
  yml_projr[["build"]][["osf"]] <- yml_projr_osf
  yml_projr_root_final <- .projr_dest_add_get_final(
    yml_root_orig = yml_projr_orig_root,
    yml_merge_final = yml_projr,
    type = "osf",
    title = title
  )
  .projr_yml_set_root(yml_projr_root_final)
  invisible(character())
}

.projr_dest_add_list_get_osf_parent_vec <- function(id_parent,
                                                    title_parent,
                                                    yml_projr_osf) {
  parent_specified <- !is.null(id_parent) || !is.null(title_parent)
  if (!parent_specified) {
    return(character())
  }
  parent_vec <- .projr_dest_add_list_get_osf_find_parent(
    id_parent = id_parent,
    title_parent = title_parent,
    yml_projr_osf = yml_projr_osf
  )
  if (is.null(id_parent) && length(parent_vec) == 0L) {
    stop(paste0(
      "id_parent must be specified for components with no parent in build OSF hierarchy" # nolint
    ))
  }
  parent_vec
}

.projr_dest_add_list_get_osf_find_parent <- function(id_parent = NULL,
                                                     title_parent = NULL,
                                                     yml_projr_osf = NULL,
                                                     parent_search = NULL) {
  .projr_dest_add_list_get_osf_find_parent_rec(
    id_parent = id_parent,
    title_parent = title_parent,
    yml_projr_osf = yml_projr_osf,
    parent_search = parent_search
  ) |>
    unlist()
}

.projr_dest_add_list_get_osf_find_parent_rec <- function(id_parent = NULL,
                                                         title_parent = NULL,
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
  if (is.null(id_parent) && is.null(title_parent)) {
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
      if (!is.null(id_parent)) {
        if (identical(yml_projr_osf_lvl[[x]][["id"]], id_parent)) {
          return(invisible(c(parent_search, x)))
        }
      }
      if (!is.null(title_parent)) {
        if (identical(x, title_parent)) {
          return(invisible(c(parent_search, x)))
        }
      }
      # if there are no components, then return nothing
      if (!"component" %in% names(yml_projr_osf_lvl[[x]])) {
        return(invisible(character()))
      }

      .projr_dest_add_list_get_osf_find_parent_rec(
        id_parent = id_parent,
        title_parent = title_parent,
        yml_projr_osf = yml_projr_osf,
        parent_search = c(parent_search, x, "component")
      )
    }
  )
}

.projr_dest_add_list_get_osf_add_comp_root_check_present <- function(overwrite,
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



.projr_dest_add_list_get_osf_dest_add_comp_root <- function(yml_projr_osf,
                                                            overwrite,
                                                            title,
                                                            list_add,
                                                            id_parent) {
  # need to know if parent is already
  # there with the title so that we
  # can overwrite it
  .projr_dest_add_list_get_osf_add_comp_root_check_present(
    overwrite = overwrite,
    yml_projr_osf = yml_projr_osf,
    title = title
  )
  yml_projr_osf[[title]] <- list_add[[1]]
  yml_projr_osf
}

.projr_dest_add_list_get_osf_dest_add_project <- function(yml_projr_osf,
                                                          list_add,
                                                          title,
                                                          overwrite) {
  if (title %in% names(yml_projr_osf) && !overwrite) {
    stop("title already exists in _projr.yml")
  }
  yml_projr_osf[[title]] <- list_add[[title]]
  yml_projr_osf
}

.projr_dest_add_list_get_osf_dest_add_comp_parent <- function(yml_projr_osf,
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

# get the id_parent, I think
.projr_dest_add_osf_get_id_parent <- function(id_parent,
                                              parent_vec,
                                              yml_projr_osf) {
  if (!is.null(id_parent)) {
    return(id_parent)
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
