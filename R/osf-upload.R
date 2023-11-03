.projr_osf_send_yml_content <- function(output_run,
                                        id,
                                        remote_structure,
                                        content,
                                        path,
                                        path_append_label,
                                        cue,
                                        sync_approach,
                                        version_source,
                                        conflict,
                                        component) {
  # would need to add cue here
  osf_tbl <- .projr_osf_get_node_id(id = id)
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  for (label in content) {
    .projr_osf_send_yml_label(
      label,
      osf_tbl,
      osf_tbl_file,
      path,
      path_append_label,
      remote_structure,
      sync_approach,
      version_source
    )
  }
  for (i in seq_along(component)) {
    yml_projr_osf_ind <- component[[i]]
    yml_projr_osf_ind_upload <- yml_projr_osf_ind[["upload"]]
    # need to make this recursive
    .projr_osf_send_yml_content(
      id = yml_projr_osf_ind[["id"]],
      remote_structure = yml_projr_osf_ind[["remote-structure"]],
      content = yml_projr_osf_ind[["content"]],
      path = yml_projr_osf_ind[["path"]], ,
      path_append_label = yml_projr_osf_ind[["path_append_label"]], ,
      cue = yml_projr_osf_ind_upload[["cue"]],
      sync_approach = yml_projr_osf_ind_upload[["sync-approach"]],
      version_source = yml_projr_osf_ind_upload[["version-source"]],
      conflict = yml_projr_osf_ind_upload[["conflict"]],
      component = yml_projr_osf_ind[["component"]]
    )
  }
}

.projr_osf_send_yml_label <- function(label,
                                      output_run,
                                      osf_tbl,
                                      path,
                                      path_append_label,
                                      remote_structure,
                                      sync_approach,
                                      version_source,
                                      conflict) {
  # get what must be transferred and where
  # ----------------------------

  switch(remote_structure,
    "latest" = .projr_osf_send_yml_latest(
      label = label,
      output_run = output_run,
      osf_tbl = osf_tbl,
      path = path,
      path_append_label = path_append_label,
      sync_approach = sync_approach,
      version_source = version_source,
      conflict = conflict
    ),
    "version" = .projr_osf_send_yml_version(
      label = label,
      output_run = output_run,
      osf_tbl = osf_tbl,
      path = path,
      path_append_label = path_append_label,
      sync_approach = sync_approach,
      version_source = version_source,
      conflict = conflict
    )
  )
  # TODO: add in code to make this recursive
  invisible(TRUE)
}

.projr_osf_send_yml_latest <- function(label,
                                       output_run,
                                       osf_tbl,
                                       path,
                                       path_append_label,
                                       osf_tbl_upload = NULL,
                                       sync_approach,
                                       version_source,
                                       conflict) {
  if (is.null(osf_tbl_upload)) {
    osf_tbl_upload <- .projr_osf_node_dir_get(
      osf_tbl = osf_tbl,
      path = path,
      path_append_label = path_append_label,
      label = label,
      remote_structure = "latest"
    )
  }

  # get local directory to upload from
  path_dir_local <- projr_dir_get(label, output_safe = !output_run)

  # this is effectively just wiping out what's there
  convert_to_sync_using_deletion <- sync_approach == "sync-using-version" &&
    version_source == "none"
  if (convert_to_sync_using_deletion) {
    sync_approach <- "sync-using-deletion"
  }
  # delete what's there if requred
  if (sync_approach == "sync-using-deletion") {
    .projr_osf_node_empty(osf_tbl = osf_tbl_upload)
  }
  switch(sync_approach,
    "upload-all" = ,
    "sync-using-delection" = .projr_osf_send_dir(
      path_dir_local = path_dir_local,
      osf_tbl = osf_tbl_upload,
      conflict = conflict
    ),
    "upload-missing" = .projr_osf_send_missing(
      path_dir_local = path_dir_local,
      osf_tbl = osf_tbl_upload,
      version_source = version_source,
      label = label,
      conflict = conflict
    ),
    "sync-using-version" = .projr_osf_send_version(
      path_dir_local = path_dir_local,
      label = label,
      osf_tbl_upload = osf_tbl_upload,
      version_source = version_source
    ),
    stop(paste0(
      "sync_approach must be one of: ",
      paste0('"', c(
        "upload-all", "sync-using-deletion", "upload-missing",
        "sync-using-version"
      ), '"', collapse = ", ")
    ))
  )
}

.projr_osf_send_yml_version <- function(label,
                                        output_run,
                                        osf_tbl,
                                        path,
                                        path_append_label,
                                        sync_approach,
                                        version_source,
                                        conflict) {
  # avoid creating new version upfront
  path_dir_osf <- .projr_osf_path_get(
    osf_tbl = osf_tbl,
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = "version",
    version = NULL
  )
  # get local directory to upload from
  path_dir_local <- projr_dir_get(label, output_safe = !output_run)

  switch(version_source,
    "none" = {
      # if the version_source is none, we won't only create a
      # new version if there are changes but
      # whenever we upload
      osf_tbl_upload <- osfr::osf_mkdir(x = osf_tbl, path = path_dir_osf)
      .projr_osf_send_yml_latest(
        label = label,
        output_run = output_run,
        osf_tbl_upload = osf_tbl_upload,
        sync_approach = sync_approach,
        version_source = "none",
        conflict = conflict
      )
    },
    "manifest" = .projr_osf_send_version(
      path_dir_local = path_dir_local,
      label = label,
      osf_tbl = osf_tbl,
      path_dir_osf = path_dir_osf,
      version_source = "manifest"
    ),
    "osf" = .projr_osf_send_version(
      path_dir_local = path_dir_local,
      label = label,
      osf_tbl = osf_tbl,
      path_dir_osf = path_dir_osf,
      version_source = "osf"
    )
  )
}

.projr_osf_send_missing <- function(path_dir_local,
                                    osf_tbl,
                                    version_source,
                                    label,
                                    conflict) {
  fn_vec_local <- list.files(path_dir_local, recursive = TRUE)
  fn_vec_missing <- switch(version_source,
    "osf" = setdiff(fn_vec_local, .projr_osf_ls_files(osf_tbl)),
    "manifest" =
      .projr_change_get_manifest(label = label)[["added"]][["fn"]],
    "none" = fn_vec_local
  )
  .projr_osf_send_file(
    fn_rel = fn_vec_missing, path_dir_local = path_dir_local,
    osf_tbl = osf_tbl, conflict = conflict
  )
}

.projr_osf_send_version <- function(path_dir_local,
                                    label,
                                    osf_tbl = NULL,
                                    osf_tbl_upload = NULL,
                                    path_dir_osf = NULL,
                                    version_source) {
  change_list <- switch(version_source,
    "manifest" = {

    },
    "osf" = {
      stop(paste0("osf version_source not yet implemented"))
    }
  )

  if (is.null(osf_tbl_upload)) {
    osf_tbl_upload <- osfr::osf_mkdir(x = osf_tbl, path = path_dir_osf)
  }

  .projr_osf_send_file(
    fn_rel = change_list[["add"]],
    path_dir_local = path_dir_local,
    osf_tbl = osf_tbl_upload,
    conflict = "overwrite"
  )
  .projr_osf_remove_fn(fn_rel = change_list[["rm"]], osf_tbl = osf_tbl_upload)
  invisible(TRUE)
}

# upload contents of a directory to a node
.projr_osf_send_dir <- function(path_dir_local,
                                osf_tbl,
                                conflict = "overwrite") {
  fn_vec_local <- list.files(path_dir_local, recursive = TRUE)
  .projr_osf_send_file(
    fn_rel = fn_vec_local, path_dir_local = path_dir_local,
    osf_tbl = osf_tbl, conflict = conflict
  )
}

# upload files within a directory to a node
.projr_osf_send_file <- function(fn_rel, # relative to directory
                                 path_dir_local,
                                 osf_tbl,
                                 conflict) {
  if (length(fn_rel) == 0) {
    return(invisible(FALSE))
  }
  plot_df <- data.frame(
    fn = fn_rel, dir = dirname(fn_rel)
  )
  dir_vec <- unique(plot_df[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_upload <- osfr::osf_mkdir(x = osf_tbl, path = x)
    } else {
      osf_tbl_upload <- osf_tbl
    }
    osfr::osf_upload(
      x = osf_tbl_upload,
      path = file.path(path_dir_local, plot_df[["fn"]][plot_df[["dir"]] == x]),
      conflicts = conflict
    )
  }
  invisible(TRUE)
}

# upload files within a directory to a node
.projr_osf_remove_fn <- function(fn_rel, # relative to directory
                                 osf_tbl) {
  if (length(fn_rel) == 0) {
    return(invisible(FALSE))
  }
  plot_df <- data.frame(
    fn = fn_rel, dir = dirname(fn_rel)
  )
  dir_vec <- unique(plot_df[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_rm <- osfr::osf_mkdir(x = osf_tbl, path = x)
    } else {
      osf_tbl_rm <- osf_tbl
    }
    osf_tbl_file <- osf_tbl_rm |> osfr::osf_ls_files()
    fn_vec <- plot_df[["fn"]][plot_df[["dir"]] == x]
    osf_tbl_file_rm <- osf_tbl_file[osf_tbl_file[["name"]] %in% fn_vec, ]

    # delete entire directory if it's a directory and
    # all files are being deleted
    if (nrow(osf_tbl_file_rm) == nrow(osf_tbl_file)) {
      if (inherits(osf_tbl_rm, "osf_tbl_file")) {
        osfr::osf_rm(x = osf_tbl_rm, check = FALSE, recurse = FALSE)
      }
      return(invisible(TRUE))
    }
    # delete files one by one if it's a node or if
    # not all files are deleted in directory
    for (i in seq_along(fn_vec)) {
      osfr::osf_rm(x = osf_tbl_file_rm[i, ], check = FALSE, recurse = FALSE)
    }
  }
  invisible(TRUE)
}
