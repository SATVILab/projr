.projr_osf_get_node <- function(title, yml_param, parent_id) {
  # get node from id
  osf_tbl <- .projr_osf_get_node_id(yml_param[["id"]])
  if (!is.null(osf_tbl)) {
    return(osf_tbl)
  }

  # get node from parent_id and title
  osf_tbl <- .projr_osf_get_node_id_parent(title, yml_param, parent_id)
  if (!is.null(osf_tbl)) {
    return(osf_tbl)
  }

  # create node
  .projr_osf_create_node(
    title = title, yml_param = yml_param, parent_id = parent_id
  )
}

.projr_osf_get_node_id <- function(id) {
  if (is.null(id)) {
    return(NULL)
  }

  tryCatch(
    osfr::osf_retrieve_node(paste0("https://osf.io/", id)),
    error = function(e) {
      stop(paste0(
        "Could not retrieve OSF node (project/component):", id
      ))
    }
  )
}

.projr_osf_get_parent_id <- function(yml_param, parent_id) {
  # actually, what matters is the parent
  parent_id_yml <- yml_param[["parent_id"]]
  if ((!is.null(parent_id_yml)) && (!is.null(parent_id))) {
    stop(
      "parent_id cannot be specified in both _projr.yml and function call"
    )
  }
  if (!is.null(parent_id_yml)) {
    parent_id <- parent_id_yml
  }
  parent_id
}

.projr_osf_get_node_id_parent <- function(title, yml_param, parent_id) {
  parent_id <- .projr_osf_get_parent_id(
    yml_param = yml_param, parent_id = parent_id
  )
  if (is.null(parent_id)) {
    return(NULL)
  }
  osf_tbl_parent <- .projr_osf_get_node_id(parent_id)
  osf_tbl_parent_comp <- osf_tbl_parent |>
    osfr::osf_ls_nodes()
  if (nrow(osf_tbl_parent_comp) == 0L) {
    return(NULL)
  }
  id <- osf_tbl_parent_comp[["id"]][
    osf_tbl_parent_comp[["name"]] == title
  ]
  if (length(id) == 0L) {
    return(NULL)
  }
  .projr_osf_get_node_id(id)
}

.projr_osf_create_node <- function(title, yml_param, parent_id = NULL) {
  if (is.null(parent_id)) {
    if (is.null(yml_param[["category"]])) {
      stop("parent_id must be specified for components")
    }
    if (yml_param[["category"]] != "project") {
      stop("parent_id must be specified for components")
    }
    osf_tbl <- try(osfr::osf_create_project(
      title = title,
      description = yml_param[["body"]],
      public = yml_param[["public"]],
      category = yml_param[["category"]]
    ))
  } else {
    osf_tbl_parent <- tryCatch(
      .projr_osf_get_node_id(parent_id),
      error = function(e) {
        stop(paste0("Could not get OSF node for id: ", parent_id))
      }
    )
    if (sum(osf_tbl_parent[["name"]] == title) > 0) {
      stop(paste0(
        "OSF node already exists for title: ", title,
        " in node ", parent_id
      ))
    }
    osf_tbl <- try(osfr::osf_create_component(
      title = title,
      description = yml_param[["body"]],
      public = yml_param[["public"]],
      category = yml_param[["category"]],
      x = osf_tbl_parent
    ))
  }
  if (inherits(osf_tbl, "try-error")) {
    stop("Could not create OSF node (project/component):", title)
  }
  osf_tbl
}

projr_osf_create_project <- function(title,
                                     body = NULL,
                                     public = FALSE) {
  if (!length(title) == 1L && is.character(title)) {
    stop(paste0("title must be a character string"))
  }
  if (!length(body) == 1L && is.character(body)) {
    stop(paste0("body must be a character vector of length one"))
  }
  if (!length(public) == 1L && is.logical(public)) {
    stop(paste0("public must be a logical vector of length one"))
  }

  osfr::osf_create_project(
    title = title,
    description = body,
    public = public
  )
}

.projr_osf_rm_node_id <- function(id) {
  try(
    {
      osfr::osf_rm(
        x = .projr_osf_get_node_id(id), recurse = TRUE, check = FALSE
      )
    },
    silent = TRUE
  )
}
