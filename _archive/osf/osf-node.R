.osf_get_node <- function(title = NULL,
                                label = NULL,
                                id = NULL,
                                id_parent = NULL,
                                id_parent_force = NULL,
                                category = NULL,
                                body = NULL,
                                public = FALSE,
                                path_append_label = NULL,
                                path = NULL,
                                create = TRUE) {
  id_parent <- .osf_get_id_parent(
    id_parent_force = id_parent_force, id_parent = id_parent
  )
  osf_tbl <- .osf_get_node_as_node(
    title = title, id = id, id_parent = id_parent,
    category = category,
    body = body,
    public = public,
    create = create
  )
  if (is.null(label)) {
    return(osf_tbl)
  }
  .osf_get_node_sub_dir(
    osf_tbl = osf_tbl,
    label = label,
    path_append_label = path_append_label,
    path = path
  )
}

.osf_get_node_as_node <- function(title = NULL,
                                        id = NULL,
                                        id_parent = NULL,
                                        category = NULL,
                                        body = NULL,
                                        public = FALSE,
                                        create = TRUE) {
  # get node from id
  osf_tbl <- .osf_get_node_id(id = id)
  if (!is.null(osf_tbl)) {
    return(osf_tbl)
  }

  # get node from id_parent and title
  osf_tbl <- .osf_get_node_id_parent(
    title = title, id_parent = id_parent
  )
  if (!is.null(osf_tbl)) {
    return(osf_tbl)
  } else if (!create) {
    stop("OSF table not found and set to do not create")
  }

  # create node
  .osf_create_node(
    title = title, id_parent = id_parent,
    category = category, body = body, public = public
  )
}

.osf_get_node_id <- function(id) {
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


.osf_get_id_parent <- function(id_parent_force, id_parent) {
  # actually, what matters is the parent
  if ((!is.null(id_parent_force)) && (!is.null(id_parent))) {
    stop(
      "id_parent cannot be specified in both _projr.yml and function call"
    )
  }
  if (!is.null(id_parent_force)) {
    id_parent <- id_parent_force
  }
  id_parent
}

.osf_get_node_id_parent <- function(title, id_parent) {
  if (is.null(id_parent)) {
    return(NULL)
  }
  osf_tbl_parent <- .osf_get_node_id(id_parent)
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
  .osf_get_node_id(id)
}

.osf_create_node <- function(title = NULL,
                                   id_parent_force = NULL,
                                   id_parent = NULL,
                                   category = NULL,
                                   body = NULL,
                                   public = FALSE) {
  if (is.null(id_parent)) {
    if (is.null(category)) {
      stop("id_parent must be specified for components")
    }
    if (category != "project") {
      stop("id_parent must be specified for components")
    }
    osf_tbl <- try(osfr::osf_create_project(
      title = title,
      description = body,
      public = public,
      category = category
    ))
  } else {
    osf_tbl_parent <- tryCatch(
      .osf_get_node_id(id_parent),
      error = function(e) {
        stop(paste0("Could not get OSF node for id: ", id_parent))
      }
    )
    if (sum(osf_tbl_parent[["name"]] == title) > 0) {
      stop(paste0(
        "OSF node already exists for title: ", title,
        " in node ", id_parent
      ))
    }
    osf_tbl <- try(osfr::osf_create_component(
      title = title,
      description = body,
      public = public,
      category = category,
      x = osf_tbl_parent
    ))
  }
  if (inherits(osf_tbl, "try-error")) {
    stop("Could not create OSF node (project/component):", title)
  }
  osf_tbl
}

.osf_get_node_sub_dir <- function(osf_tbl,
                                        label,
                                        path_append_label,
                                        path) {
  path_dir_sub <- .osf_get_path_dir_sub(
    label = label, path_append_label = path_append_label, path = path
  )
  if (is.null(path_dir_sub)) {
    return(osf_tbl)
  }
  osfr::osf_mkdir(
    x = osf_tbl, path = path_dir_sub
  )
}

.osf_get_path_dir_sub <- function(label, path_append_label, path) {
  # set up subdirectories
  append_label <- is.null(path_append_label) ||
    path_append_label
  path_pre <- !is.null(path)
  if (!append_label && !path_pre) {
    return(NULL)
  }
  if (append_label && path_pre) {
    path_dir_sub <- file.path(path, label)
  } else if (append_label) {
    path_dir_sub <- label
  } else {
    path_dir_sub <- path
  }
  path_dir_sub
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

.osf_rm_node_id <- function(id) {
  try(
    {
      osfr::osf_rm(
        x = .osf_get_node_id(id), recurse = TRUE, check = FALSE
      )
    },
    silent = TRUE
  )
}

.osf_node_empty <- function(osf_tbl) {
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_rm(x = osf_tbl_file[i, ], recurse = TRUE, check = FALSE)
  }
}
