.projr_osf_upload <- function(output_run) {
  # consider early exit
  # ------------------

  if (!.projr_osf_check_run(output_run)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------

  if (!requireNamespace("osfr", quietly = TRUE)) {
    renv::install("osfr", prompt = FALSE)
    .projr_dep_add("osfr")
  }
  for (i in seq_along(projr_yml_get()[["build"]][["osf"]])) {

  }
}

.projr_osf_check_run <- function(output_run) {
  yml_projr <- projr_yml_get()
  # either a dev run or else no osf upload specified
  if ((!output_run) ||
    (!"osf" %in% names(yml_projr[["build"]]))) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' @title Add an OSF node
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
#' @export
projr_osf_add <- function(title,
                          create = TRUE,
                          body = NULL,
                          content = NULL,
                          public = FALSE,
                          category = NULL,
                          id = NULL,
                          parent_id = NULL,
                          parent_title = NULL) {
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
    !all(is.character(body))
  body_invalid <- body_invalid || length(body) != 1
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
    !all(is.character(id))
  id_invalid <- id_invalid || all(nchar(id) == 5L) || length(id) != 1
  if (id_invalid) {
    stop("id must be a string with five characters")
  }
  parent_id_invalid <- (!is.null(parent_id)) &&
    !all(is.character(parent_id))
  parent_id_invalid <- parent_id_invalid ||
    all(nchar(parent_id) == 5L) ||
    length(parent_id) != 1
  if (parent_id_invalid) {
    stop("parent_id must be a string with five characters")
  }
  parent_title_invalid <- (!is.null(parent_title)) &&
    !all(is.character(parent_title))
  parent_title_invalid <- parent_title_invalid || length(parent_title) != 1
  if (parent_title_invalid) {
    stop("parent_title must be a character of length one")
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
  ) |>
    utils::setNames(title)
  yml_projr_osf <- yml_projr[["build"]][["osf"]]

  # find where to add list, and add it
  # ----------------------------------

  # if category is not explicitly a project, then find parent
  if (is.null(category) || category != "project") {
    # find if the parent is specified anywhere
    if ((!is.null(parent_id)) || (!is.null(parent_title))) {
      parent_vec <- .projr_osf_find_parent(
        parent_id = parent_id,
        parent_title = parent_title,
        yml_projr_osf = yml_projr_osf
      )
    } else {
      parent_vec <- character()
    }

    # if not specified, then add to top level
    if (!nzchar(parent_vec)) {
      list_add <- list_add |>
        append(
          list(
            parent_id = parent_id,
            parent_title = parent_title
          )
        )
      yml_projr_osf <- yml_projr_osf |>
        append(
          list(list_add) |> utils::setNames(title)
        )
    } else {
      # if specified, add to parent
      yml_projr_osf_parent_chr <-
        paste0(
          "yml_projr_osf[[",
          paste0(paste0('"', parent_vec, '"'), collapse = "]][["),
          "]]"
        )
      eval(parse(text = paste0(yml_projr_osf_parent_chr, " <- list_add")))
    }
  } else {
    # if project, then add to top level
    yml_projr_osf <- yml_projr_osf |>
      append(
        list(list_add) |> utils::setNames(title)
      )
  }
  yml_projr[["build"]][["osf"]] <- yml_projr_osf
  .projr_yml_set(yml_projr)
}

yml_projr_osf <- list(
  "abc" = list(
    "component" = list(
      "ghi" = list(
        id = "12345"
      )
    )
  ),
  "def" = list()
)

.projr_osf_find_parent(
  parent_id = "1235",
  yml_projr_osf = yml_projr_osf
) |>
  unlist()

.projr_osf_find_parent <- function(parent_id = NULL,
                                   parent_title = NULL,
                                   yml_projr_osf = NULL,
                                   parent_search = NULL) {
  .projr_osf_find_parent_rec(
    parent_id = parent_id,
    parent_title = parent_title,
    yml_projr_osf = yml_projr_osf,
    parent_search = parent_search
  ) |>
    unlist()
}

.projr_osf_find_parent_rec <- function(parent_id = NULL,
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
      if (identical(x, parent_title)) {
        return(invisible(c(parent_search, x)))
      }
      if (identical(parent_id, yml_projr_osf[[x]][["id"]])) {
        return(invisible(c(parent_search, x)))
      }
      if (!"component" %in% names(yml_projr_osf[[x]])) {
        return(invisible(character()))
      }
      .projr_osf_find_parent_rec(
        parent_id = parent_id,
        parent_title = parent_title,
        yml_projr_osf = yml_projr_osf[[x]][["component"]],
        parent_search = c(parent_search, x, "component")
      )
    }
  )
}

  parent_id <- yml_projr_osf[["parent_id"]]
  parent_title <- yml_projr_osf[["parent_title"]]
  if (is.null(parent_id) && is.null(parent_title)) {
    return(invisible(NULL))
  }
  if (!is.null(parent_id)) {
    parent_id <- parent_id[[1]]
  }
  if (!is.null(parent_title)) {
    parent_title <- parent_title[[1]]
  }
  if (is.null(parent_id) && is.null(parent_title)) {
    return(invisible(NULL))
  }
  invisible(list(parent_id = parent_id, parent_title = parent_title))
}

projr_osf_add_project <- function(title,
                                  body = NULL,
                                  content = NULL,
                                  public = FALSE,
                                  id = NULL) {
  projr_osf_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = "project",
    id = id
  )
}

projr_osf_add_component <- function(title,
                                    body = NULL,
                                    content = NULL,
                                    public = FALSE,
                                    category = NULL,
                                    parent_title = NULL,
                                    parent_id = NULL,
                                    id = NULL) {
  if (missing(parent_id)) {
    stop("parent_id must be specified")
  }
  projr_osf_add(
    title = title,
    body = body,
    content = content,
    public = public,
    category = category,
    parent_id = parent_id,
    parent_title = parent_title,
    node_id = node_id
  )
}
