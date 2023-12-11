# so, we need functions to restore downloads,
# we also need functions to download
# outputs.
# should both be called restored?
.projr_checkout_osf <- function(label = NULL,
                                version = NULL,
                                input_only = TRUE) {
  if (is.null(label)) {
    label <- projr_yml_get_unchecked()[["directories"]]
  }
  if (input_only) {
    match_str <- "^cache|^dataraw"
  } else {
    match_str <- "^cache|^dataraw|^output"
  }
  label <- label[grepl(match_str, .projr_dir_label_strip(label))]
  for (i in seq_along(label)) {
    .projr_checkout_osf_label(label[[i]], version = version)
  }
}

.projr_checkout_osf_label <- function(label, version, id_parent_force = NULL) {
  if (missing(label)) {
    stop("label must be specified")
  }
  if (!is.character(label)) {
    stop(paste0(
      "label must be a character vector, not ",
      paste0(class(label), collapse = "; ")
    ))
  }
  yml_label <- projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_label)) {
    return(invisible(FALSE))
  }
  # configure with defaults (need to turn this into a function)
  yml_label[["osf"]][[1]][["remote-structure"]] <-
    yml_label[["osf"]][[1]][["remote-structure"]] %||% "latest"
  yml_label[["osf"]][[1]][["path-append-label"]] <-
    yml_label[["osf"]][[1]][["path-append-label"]] %||% TRUE
  yml_osf <- yml_label[["osf"]][[1]]

  # if remote is latest only, then cannot specify the version
  version <- version %||% "latest"
  version_incompat_with_remote_str <- !(version == "latest") && # nolint
    yml_label[["osf"]][[1]][["remote-structure"]] == "latest"
  if (version_incompat_with_remote_str) {
    stop(paste0(
      "Cannot specify the version as something other than `latest` when the remote structure is `latest` for label ", label # nolint
    ))
  }
  # do not create as we're restoring.
  # do not append label or path as
  # we need to know that for the `download_to_dir`
  # function below
  osf_tbl <- .projr_osf_get_node(
    title = names(yml_label[["osf"]]),
    label = label,
    id = yml_osf[["id"]],
    create = FALSE
  )

  # get download settings
  yml_osf[["download"]] <- .projr_osf_complete_dnld_list(yml_osf[["download"]])

  # actually download
  .projr_osf_dnld_to_dir(
    osf_tbl = osf_tbl,
    version = version,
    # path to save to locally
    path_save = yml_label[["path"]],
    # whether we're downloading from an OSF sub-directory or the node itself
    sub_dir = yml_osf[["path-append-label"]] || !is.null(yml_osf[["path"]]),
    # type of OSF directory (latest, version or content)
    structure = yml_osf[["remote-structure"]],
    # how to choose what to delete (locally) and what to download
    sync_approach = yml_osf[["download"]][["sync-approach"]],
    conflict = yml_osf[["download"]][["conflict"]]
  )

  # return table you downloaded to
  osf_tbl
}

.projr_osf_dnld_to_dir <- function(osf_tbl,
                                   version,
                                   sub_dir,
                                   path_save,
                                   structure,
                                   sync_approach,
                                   conflict) {
  # ensure we clear it before checking
  # so that it's empty to match the remote
  # before leaving
  if (sync_approach == "delete-then-download-all") {
    unlink(path_save, recursive = TRUE)
  }
  .projr_dir_create(path_save)
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  if (grepl("download\\-all$", sync_approach)) {
    .projr_osf_dnld_to_dir_all(
      osf_tbl_file = osf_tbl_file,
      version = version,
      structure = structure,
      path_save = path_save,
      sub_dir = sub_dir,
      conflict = conflict
    )
  }
  if (grepl("^download-missing$", sync_approach)) {
    stop("download_missing not implemented yet")
  }
  invisible(TRUE)
}

.projr_osf_dnld_to_dir_all <- function(osf_tbl_file,
                                       version,
                                       structure,
                                       path_save,
                                       sub_dir,
                                       conflict) {
  if (structure == "content") {
    stop("content-addressable remote structure not yet supported")
  } else if (structure == "version") {
    osf_tbl_file <- osf_tbl_file[
      grepl("^v\\d+", osf_tbl_file[["name"]]),
    ]
    if (version == "latest") {
      osf_tbl_file <- osf_tbl_file[
        osf_tbl_file[["name"]] == max(osf_tbl_file[["name"]]),
      ]
    } else {
      # choose the previous version closest
      # to the request version as the requested
      # version is in fact this version
      # as if the nearest previous version is not the same
      # then that means that there are no updates since then
      osf_tbl_file <- osf_tbl_file[
        gsub("$v", "", osf_tbl_file[["name"]]) >= version,
      ]
      osf_tbl_file <- osf_tbl_file[
        osf_tbl_file[["name"]] == min(osf_tbl_file[["name"]]),
      ]
    }
    if (nrow(osf_tbl_file) == 0L) {
      stop("No compatible version found in OSF")
    }
    osf_tbl_file <- osf_tbl_file[1, ]
    osf_tbl_file <- osf_tbl_file |> osfr::osf_ls_files(n_max = Inf)
  }
  # what is the point of all of this?
  # this just seems like we're just obeying
  # that we must only download one thing at a time
  # in the first instance.
  # I suppose the idea is
  # that the second is just a node.
  # but it's not a node.
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
      x = osf_tbl_file[i, ],
      path = path_save,
      recurse = TRUE,
      conflicts = conflict
    )
  }
  invisible(TRUE)
}

.projr_osf_complete_dnld_list <- function(yml_projr) {
  if (!is.null(yml_projr)) {
    if (is.null(yml_projr[["conflict"]])) {
      yml_projr[["conflict"]] <- "overwrite"
    }
    if (is.null(yml_projr[["sync-approach"]])) {
      yml_projr[["sync-approach"]] <- "download-all"
    }
  } else {
    yml_projr[["conflict"]] <- "overwrite"
    yml_projr[["sync-approach"]] <- "download-all"
  }
  yml_projr[c("conflict", "sync-approach")]
}

.projr_osf_download_dir <- function() {
  if (!.projr_osf_download_dir_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_dir <- projr_yml_get()[["directories"]]
  for (i in seq_along(yml_projr_dir)) {
    .projr_osf_download_dir_label(
      label = names(yml_projr_dir)[i],
    )
  }
}

.projr_osf_download_build <- function(safe) {
  if (!.projr_osf_download_build_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_build_osf <- projr_yml_get()[["build"]][["osf"]]
  for (i in seq_along(yml_projr_build_osf)) {
    .projr_osf_download_build_node(
      title = names(yml_projr_build_osf)[i],
      id_parent = NULL,
      safe = safe
    )
  }
}

.projr_osf_download_dir_check_run <- function() {
  yml_projr <- projr_yml_get()
  osf_is_source <- vapply(
    yml_projr[["directories"]], function(x) "osf" %in% names(x), logical(1)
  ) |>
    any()
  if (!osf_is_source) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_download_build_check_run <- function() {
  yml_projr <- projr_yml_get()
  osf_is_dest <- "osf" %in% names(yml_projr[["build"]])
  if (!osf_is_dest) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_download_dir_label <- function(label) {
  yml_param <- projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_param)) {
    return(invisible(FALSE))
  }
  yml_param_osf <- yml_param[["osf"]]
  # allowed to just specify id
  if (is.character(yml_param_osf)) {
    osf_tbl <- .projr_osf_get_node_id(yml_param_osf)
  } else {
    osf_tbl <- .projr_osf_get_node_id(yml_param_osf[["id"]])
  }
  if (is.null(osf_tbl)) {
    stop(paste0("osf node id not specified for label ", label))
  }
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  if (is.character(yml_param_osf)) {
    # use the label if it's a directory automatically
    if ("label" %in% osf_tbl_file[["name"]]) {
      osf_tbl_file_dir <- .projr_osf_filter_dir(osf_tbl_file)
      osf_tbl_file_dir_label <- osf_tbl_file_dir[
        osf_tbl_file_dir[["name"]] == label,
      ]
      if (nrow(osf_tbl_file_dir_label) == 1L) {
        osf_tbl_file <- osf_tbl_file_dir_label
      }
    }
    # specify conflict resolution
    conflicts <- "overwrite"
  } else {
    # use sub-directory if specified
    if ("sub-dir" %in% names(yml_param_osf)) {
      dir_vec <- fs::path_split(yml_param_osf[["sub-dir"]])
      osf_tbl_file_dir <- .projr_osf_filter_dir(osf_tbl_file)
      for (x in dir_vec) {
        osf_tbl_file_dir_sub <- osf_tbl_file_dir[
          osf_tbl_file_dir[["name"]] == x,
        ]
        osf_tbl_file_dir <- osf_tbl_file_dir_sub |>
          osfr::osf_ls_files(n_max = Inf) |>
          .projr_osf_filter_dir()
      }
      if (nrow(osf_tbl_file_dir) > 0L) {
        osf_tbl_file <- osf_tbl_file_dir
      } else {
        stop(paste0(
          "sub-dir ", yml_param_osf[["sub-dir"]],
          " not found for label ", label
        ))
      }
    }
    # specify conflict resolution
    if (is.null(yml_param_osf[["conflicts"]])) {
      conflicts <- "overwrite"
    } else {
      conflicts <- yml_param_osf[["conflicts"]]
    }
  }
  .projr_osf_download_osf_tbl(
    osf_tbl = osf_tbl_file,
    path_dir_save_local = projr_dir_get(label, safe = TRUE),
    conflicts = "overwrite"
  )
  invisible(TRUE)
}

.projr_osf_download_build_node <- function(title,
                                           id_parent = NULL,
                                           safe) {
  yml_param <- projr_yml_get_unchecked()[["build"]][["osf"]][[title]]
  id_parent <- .projr_osf_get_id_parent(
    yml_param = yml_param, id_parent = id_parent
  )
  id <- yml_param[["id"]]
  if (!is.null(id)) {
    osf_tbl <- .projr_osf_get_node_id(id)
  } else {
    osf_tbl <- .projr_osf_get_node_id_parent(title, id_parent)
  }
  if (is.null(osf_tbl)) {
    stop(paste0("osf node id not found for title ", title))
  }
  if (is.null(yml_param[["conflicts"]])) {
    conflicts <- "overwrite"
  } else {
    conflicts <- yml_param[["conflicts"]]
  }
  for (i in seq_along(yml_param[["content"]])) {
    .projr_osf_download_osf_tbl(
      osf_tbl = osf_tbl,
      path_dir_save_local = projr_dir_get(yml_param[["content"]][i], safe = TRUE),
      conflicts = conflicts
    )
  }
}

.projr_osf_download_osf_tbl <- function(osf_tbl,
                                        path_dir_save_local,
                                        conflicts = "overwrite") {
  .projr_dir_create(path_dir_save_local)
  # regardless of osf_tbl being a node or a sub-directory,
  # we always want to downloads its contents rather than
  # the node/folder itself
  osf_tbl <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl))) {
    osfr::osf_download(
      x = osf_tbl[i, ],
      path = path_dir_save_local,
      recurse = TRUE,
      conflicts = conflicts
    )
  }
}
