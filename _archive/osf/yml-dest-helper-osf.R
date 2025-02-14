.yml_dest_add_add_osf <- function(category,
                                        title_parent,
                                        id_parent,
                                        id,
                                        body,
                                        public,
                                        overwrite) {
  # extract pre-existing osf config
  yml.osf <- .yml_get()[["build"]][["osf"]]

  # if category is not explicitly a project, then find parent
  is_component <- is.null(category) || category != "project"
  if (is_component) {
    # get what its parent is
    parent_vec <- .dest_add_list_get_osf_parent_vec(
      id_parent = id_parent,
      title_parent = title_parent,
      yml.osf = yml.osf
    )
    # get the parent's id
    id_parent <- .dest_add_osf_get_id_parent(
      id_parent = id_parent,
      parent_vec = parent_vec,
      yml.osf = yml.osf
    )
    # create node (if necessary) and get id
    id <- .osf_get_node_as_node(
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
      yml.osf <- .dest_add_list_get_osf_dest_add_comp_root(
        yml.osf = yml.osf,
        overwrite = overwrite,
        title = title,
        list_add = list_add,
        id_parent = id_parent
      )
    } else {
      # if specified, add to parent
      yml.osf <- .dest_add_list_get_osf_dest_add_comp_parent(
        yml.osf = yml.osf,
        overwrite = overwrite,
        title = title,
        parent_vec = parent_vec,
        list_add = list_add
      )
    }
  } else {
    # if project, then add to top level
    id <- .osf_get_node_as_node(
      title = title,
      id = id,
      category = "project",
      body = body,
      public = public
    )[["id"]][[1]]
    list_add[[1]][["id"]] <- id
    yml.osf <- .dest_add_list_get_osf_dest_add_project(
      yml.osf = yml.osf,
      list_add = list_add,
      title = title,
      overwrite = overwrite
    )
  }
  # to write final list
  yml_projr[["build"]][["osf"]] <- yml.osf
  yml.root_final <- .dest_add_get_final(
    yml_root_orig = yml.orig_root,
    yml_merge_final = yml_projr,
    type = "osf",
    title = title
  )
  .yml_set_root(yml.root_final)
  invisible(character())
}

.dest_add_list_get_osf_parent_vec <- function(id_parent,
                                                    title_parent,
                                                    yml.osf) {
  parent_specified <- !is.null(id_parent) || !is.null(title_parent)
  if (!parent_specified) {
    return(character())
  }
  parent_vec <- .dest_add_list_get_osf_find_parent(
    id_parent = id_parent,
    title_parent = title_parent,
    yml.osf = yml.osf
  )
  if (is.null(id_parent) && length(parent_vec) == 0L) {
    stop(paste0(
      "id_parent must be specified for components with no parent in build OSF hierarchy" # nolint
    ))
  }
  parent_vec
}

.dest_add_list_get_osf_find_parent <- function(id_parent = NULL,
                                                     title_parent = NULL,
                                                     yml.osf = NULL,
                                                     parent_search = NULL) {
  .dest_add_list_get_osf_find_parent_rec(
    id_parent = id_parent,
    title_parent = title_parent,
    yml.osf = yml.osf,
    parent_search = parent_search
  ) |>
    unlist()
}

.dest_add_list_get_osf_find_parent_rec <- function(id_parent = NULL,
                                                         title_parent = NULL,
                                                         yml.osf = NULL,
                                                         parent_search = NULL) {
  if (is.null(parent_search)) {
    yml.osf_lvl <- yml.osf
  } else {
    yml.osf_lvl_chr <- paste0(
      "yml.osf[[",
      paste0(paste0('"', parent_search, '"'), collapse = "]][["),
      "]]"
    )
    yml.osf_lvl <- eval(parse(text = yml.osf_lvl_chr))
  }
  # return NULL for no parent found

  # no parent found if no parent info provided
  if (is.null(id_parent) && is.null(title_parent)) {
    return(invisible(character()))
  }

  # no parent found if there are no previous osf entries
  if (is.null(yml.osf)) {
    yml.osf <- .yml_get()[["build"]][["osf"]]
    if (is.null(yml.osf)) {
      return(invisible(character()))
    }
  }

  # To do:
  # check that names are globally unique

  lapply(
    names(yml.osf_lvl),
    function(x) {
      # look within this level
      if (!is.null(id_parent)) {
        if (identical(yml.osf_lvl[[x]][["id"]], id_parent)) {
          return(invisible(c(parent_search, x)))
        }
      }
      if (!is.null(title_parent)) {
        if (identical(x, title_parent)) {
          return(invisible(c(parent_search, x)))
        }
      }
      # if there are no components, then return nothing
      if (!"component" %in% names(yml.osf_lvl[[x]])) {
        return(invisible(character()))
      }

      .dest_add_list_get_osf_find_parent_rec(
        id_parent = id_parent,
        title_parent = title_parent,
        yml.osf = yml.osf,
        parent_search = c(parent_search, x, "component")
      )
    }
  )
}

.dest_add_list_get_osf_add_comp_root_check_present <- function(overwrite,
                                                                     yml.osf,
                                                                     title) {
  if (title %in% names(yml.osf)) {
    if (!overwrite) {
      stop("title already exists in _projr.yml")
    } else {
      title_present <- TRUE
    }
  } else {
    title_present <- FALSE
  }
}



.dest_add_list_get_osf_dest_add_comp_root <- function(yml.osf,
                                                            overwrite,
                                                            title,
                                                            list_add,
                                                            id_parent) {
  # need to know if parent is already
  # there with the title so that we
  # can overwrite it
  .dest_add_list_get_osf_add_comp_root_check_present(
    overwrite = overwrite,
    yml.osf = yml.osf,
    title = title
  )
  yml.osf[[title]] <- list_add[[1]]
  yml.osf
}

.dest_add_list_get_osf_dest_add_project <- function(yml.osf,
                                                          list_add,
                                                          title,
                                                          overwrite) {
  if (title %in% names(yml.osf) && !overwrite) {
    stop("title already exists in _projr.yml")
  }
  yml.osf[[title]] <- list_add[[title]]
  yml.osf
}

.dest_add_list_get_osf_dest_add_comp_parent <- function(yml.osf,
                                                              parent_vec,
                                                              overwrite,
                                                              title,
                                                              list_add) {
  # get parent to add to
  yml.osf_parent_chr <-
    paste0(
      "yml.osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(
    text = paste0("yml.osf_parent <- ", yml.osf_parent_chr)
  ))
  if (title %in% names(yml.osf_parent[["component"]])) {
    if (!overwrite) {
      stop("title already exists in _projr.yml")
    }
  }
  yml.osf_parent[["component"]][[title]] <-
    list_add[[1]]
  yml.osf_chr <-
    paste0(
      "yml.osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(text = paste0(yml.osf_chr, " <- yml.osf_parent")))
  yml.osf
}

# get the id_parent, I think
.dest_add_osf_get_id_parent <- function(id_parent,
                                              parent_vec,
                                              yml.osf) {
  if (!is.null(id_parent)) {
    return(id_parent)
  }
  if (length(parent_vec) == 0L) {
    return(NULL)
  }
  yml.osf_parent_chr <-
    paste0(
      "yml.osf[[",
      paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
      "]]"
    )
  eval(parse(
    text = paste0(
      "yml.osf_parent <- ", yml.osf_parent_chr
    )
  ))
  yml.osf_parent[["id"]]
}
