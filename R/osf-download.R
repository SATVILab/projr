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
    .projr_restore_osf_label(label[[i]])
  }
}

.projr_restore_osf_label <- function(label, parent_id_force = NULL) {
  if (missing(label)) {
    stop("label must be specified")
  }
  if (!is.character(label)) {
    stop(paste0(
      "label must be a character vector, not ",
      paste0(class(label), collapse = "; ")
    ))
  }
  yml_label <-
    projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_label)) {
    return(invisible(FALSE))
  }
  yml_osf <- yml_label[["osf"]][[1]]
  yml_osf[["remote-structure"]] <- "version"
  # do not create as we're restoring.
  # do not append label or path as
  # we need to know that for the `download_to_dir`
  # function below
  osf_tbl <- .projr_osf_get_node(
    title = names(yml_label[["osf"]]),
    label = yml_osf[["label"]],
    id = yml_osf[["id"]],
    create = FALSE
  )

  # can try the fancy method stuff here
  # all about that `download_sync_approach`,
  # `download_version_source` and `download_conflict`.
  # get download settings
  yml_dnld <- .projr_osf_complete_dnld_list(
    yml_osf[["download"]]
  )
  # get whether we're saving to an OSF sub-dir or not
  append_label <- is.null(yml_dnld[["path-append-label"]]) ||
    yml_dnld[["path-append-label"]]
  path_pre <- !is.null(yml_label[["path"]])
  .projr_osf_dnld_to_dir(
    osf_tbl = osf_tbl,
    sub_dir = append_label || path_pre,
    path_save = yml_label[["path"]],
    remote_structure = yml_label[["remote-structure"]],
    sync_approach = yml_dnld[["sync-approach"]],
    conflict = yml_dnld[["conflict"]]
  )
  osf_tbl
}

.projr_osf_dnld_to_dir <- function(osf_tbl,
                                   sub_dir,
                                   path_save,
                                   remote_structure,
                                   sync_approach,
                                   conflict) {
  # ensure we clear it before checking
  # so that it's empty to match the remote
  # before leaving
  if (sync_approach == "delete-then-download-all") {
    unlink(path_save, recursive = TRUE)
  }
  if (!dir.exists(path_save)) {
    dir.create(path_save, recursive = TRUE)
  }
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  if (grepl("download_all$", sync_approach)) {
    .projr_osf_dnld_to_dir_all(
      osf_tbl_file = osf_tbl_file,
      remote_structure = remote_structure,
      path_save = path_save,
      sub_dir = sub_dir,
      conflict = conflict
    )
  }
  if (grepl("^download_missing$", sync_approach)) {
    stop("download_missing not implemented yet")
  }
  invisible(TRUE)
}

.projr_osf_dnld_to_dir_all <- function(osf_tbl_file,
                                       remote_structure,
                                       path_save,
                                       sub_dir,
                                       conflict) {
  if (remote_structure == "content") {
    stop("content-addressable remote structure not yet supported")
  } else if (remote_structure == "version") {
    # choose the latest type
    osf_tbl_file <- osf_tbl_file[
      grepl("^v\\d+", osf_tbl_file[["name"]]),
    ]
    osf_tbl_file <- osf_tbl_file[
      osf_tbl_file[["name"]] == max(osf_tbl_file[["name"]]),
    ]
    if (nrow(osf_tbl_file) == 0L) {
      stop("No versions found in OSF")
    }
    osf_tbl_file <- osf_tbl_file[1, ]
    osf_tbl_file <- osf_tbl_file |> osfr::osf_ls_files()
    # START HERE
  }
  if (sub_dir) {
    for (i in seq_len(nrow(osf_tbl_file))) {
      osfr::osf_download(
        x = osf_tbl_file[i, ],
        path = path_save,
        recurse = TRUE,
        conflicts = conflict
      )
    }
  } else {
    osfr::osf_download(
      x = osf_tbl_file,
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

.projr_osf_download_build <- function(output_safe) {
  if (!.projr_osf_download_build_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_build_osf <- projr_yml_get()[["build"]][["osf"]]
  for (i in seq_along(yml_projr_build_osf)) {
    .projr_osf_download_build_node(
      title = names(yml_projr_build_osf)[i],
      parent_id = NULL,
      output_safe = output_safe
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
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
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
          osfr::osf_ls_files() |>
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
    dir_save = projr_dir_get(label, output_safe = TRUE),
    is_file = TRUE,
    conflicts = "overwrite"
  )
  invisible(TRUE)
}

.projr_osf_download_build_node <- function(title,
                                           parent_id = NULL,
                                           output_safe) {
  yml_param <- projr_yml_get_unchecked()[["build"]][["osf"]][[title]]
  parent_id <- .projr_osf_get_parent_id(
    yml_param = yml_param, parent_id = parent_id
  )
  id <- yml_param[["id"]]
  if (!is.null(id)) {
    osf_tbl <- .projr_osf_get_node_id(id)
  } else {
    osf_tbl <- .projr_osf_get_node_id_parent(title, parent_id)
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
      dir_save = projr_dir_get(yml_param[["content"]][i], output_safe = TRUE),
      file = FALSE,
      conflicts = conflicts
    )
  }
}

.projr_osf_download_osf_tbl <- function(osf_tbl,
                                        dir_save,
                                        is_file,
                                        conflicts = "overwrite") {
  if (!is_file) {
    osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  }
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
      x = osf_tbl_file[i, ],
      path = dir_save,
      recurse = TRUE,
      conflicts = conflicts
    )
  }
}
