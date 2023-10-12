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
                               upload_cue = NULL,
                               upload_sync_approach = NULL,
                               upload_version_source = NULL,
                               upload_conflict = NULL,
                               overwrite = FALSE) {
  # check inputs
  # ------------
  if (missing(title)) {
    stop("title must be specified")
  }
  if (!is.logical(create)) {
    stop("create must be logical")
  }
  if (!is.logical(public)) {
    stop("public must be logical")
  }
  content_invalid <- (!is.null(content)) &&
    !all(content %in% c("docs", names(yml_projr[["directories"]])))
  if (content_invalid) {
    stop("content must match a directory key in _projr.yml")
  }
  body_invalid <- (!is.null(body)) &&
    (!all(is.character(body)) || length(body) != 1)
  if (body_invalid) {
    stop("body must be a character of length one")
  }
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
  category_invalid <- (!is.null(category)) &&
    !all(category %in% category_vec_valid)
  if (category_invalid) {
    stop("category must match a valid category")
  }
  id_invalid <- (!is.null(id)) &&
    (!all(is.character(id)) || all(nchar(id) == 5L) || length(id) != 1)
  if (id_invalid) {
    stop("id must be a string with five characters")
  }
  parent_id_invalid <- (!is.null(parent_id)) &&
    (!all(is.character(parent_id)) || all(nchar(parent_id) == 5L))
  if (parent_id_invalid) {
    stop("parent_id must be a string with five characters")
  }
  parent_title_invalid <- (!is.null(parent_title)) &&
    (!all(is.character(parent_title)) || length(parent_title) != 1)
  if (parent_title_invalid) {
    stop("parent_title must be a character of length one")
  }
  project_with_parent <- ((!is.null(category)) && category == "project") &&
    ((!is.null(parent_id)) || (!is.null(parent_title)))
  if (project_with_parent) {
    stop("project cannot have a parent")
  }

  # create list to add
  # ------------------

  yml_projr <- projr_yml_get_unchecked()
  list_add <- list(
    content = content,
    body = body,
    category = category,
    public = public,
    id = id
  )
  yml_projr_osf <- yml_projr[["build"]][["osf"]]

  # find where to add list, and add it
  # ----------------------------------

  # if category is not explicitly a project, then find parent
  if (is.null(category) || category != "project") {
    # find if the parent is specified anywhere
    if ((!is.null(parent_id)) || (!is.null(parent_title))) {
      parent_vec <- .projr_osf_yml_find_parent(
        parent_id = parent_id,
        parent_title = parent_title,
        yml_projr_osf = yml_projr_osf
      )
    } else {
      parent_vec <- character()
    }

    # if not specified, then add to top level
    if (length(parent_vec) == 0) {
      # check if it's already there
      if (!is.null(yml_projr_osf)) {
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

      list_add <- list_add |>
        append(
          list(
            parent_id = parent_id,
            parent_title = parent_title
          )
        )
      list_add <- list(list_add) |> stats::setNames(title)
      if (is.null(yml_projr_osf)) {
        yml_projr_osf <- list_add
      } else {
        if (title_present) {
          yml_projr_osf[[title]] <- list_add[[title]]
        } else {
          yml_projr_osf <- yml_projr_osf |> append(list_add)
        }
      }
    } else {
      # if specified, add to parent
      yml_projr_osf_parent_chr <-
        paste0(
          "yml_projr_osf[[",
          paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
          "]]"
        )
      eval(parse(
        text = paste0("yml_projr_osf_parent <- ", yml_projr_osf_parent_chr)
      ))
      if (!"component" %in% names(yml_projr_osf_parent)) {
        yml_projr_osf_parent[["component"]] <- list()
      }
      list_add <- list(list_add) |> stats::setNames(title)
      if (title %in% names(yml_projr_osf_parent[["component"]])) {
        if (!overwrite) {
          stop("title already exists in _projr.yml")
        } else {
          yml_projr_osf_parent[["component"]][[title]] <- list_add[[title]]
        }
      } else {
        yml_projr_osf_parent[["component"]] <-
          yml_projr_osf_parent[["component"]] |> append(list_add)
      }

      yml_projr_osf_chr <-
        paste0(
          "yml_projr_osf[[",
          paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
          "]]"
        )
      eval(parse(text = paste0(yml_projr_osf_chr, " <- yml_projr_osf_parent")))
    }
  } else {
    # if project, then add to top level
    list_add <- list(list_add) |> stats::setNames(title)
    if (is.null(yml_projr_osf)) {
      yml_projr_osf <- list_add
    } else {
      if (title %in% names(yml_projr_osf) && !overwrite) {
        stop("title already exists in _projr.yml")
      }
      yml_projr_osf[[title]] <- list_add[[title]]
    }
  }
  yml_projr[["build"]][["osf"]] <- yml_projr_osf
  .projr_yml_set(yml_projr)
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

  lapply(
    names(yml_projr_osf),
    function(x) {
      if (!is.null(parent_title)) {
        if (identical(x, parent_title)) {
          return(invisible(c(parent_search, x)))
        }
      }
      if (!is.null(parent_id)) {
        if (identical(yml_projr_osf[[x]][["id"]], parent_id)) {
          return(invisible(c(parent_search, x)))
        }
      }
      if (!"component" %in% names(yml_projr_osf[[x]])) {
        return(invisible(character()))
      }
      .projr_osf_yml_find_parent_rec(
        parent_id = parent_id,
        parent_title = parent_title,
        yml_projr_osf = yml_projr_osf[[x]][["component"]],
        parent_search = c(x, "component")
      )
    }
  )
}
