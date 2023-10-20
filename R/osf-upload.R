.projr_osf_upload_yml_content <- function(output_run,
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
    .projr_osf_upload_yml_label(
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
    .projr_osf_upload_yml_content(
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

.projr_osf_upload_yml_label <- function(label,
                                        output_run,
                                        osf_tbl,
                                        osf_tbl_file,
                                        path,
                                        path_append_label,
                                        remote_structure,
                                        sync_approach,
                                        version_source,
                                        conflict) {
  # get what must be transferred and where
  # ----------------------------
  osf_tbl_upload <- .projr_osf_node_dir_get(
    osf_tbl = osf_tbl,
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure
  )
  path_dir_local <- projr_dir_get(label, output_safe = !output_run)
  if (sync_approach == "delete-then-upload-all") {
    .projr_osf_node_empty(osf_tbl = osf_tbl_upload)
  }
  if (grepl("upload\\-all$", sync_approach)) {
    .projr_osf_upload_dir(
      path_dir_local = path_dir_local,
      osf_tbl = osf_tbl_upload,
      conflict = conflict
    )
  }
  if (sync_approach == "upload-missing") {
    .projr_osf_upload_missing(
      path_dir_local = path_dir_local,
      osf_tbl = osf_tbl_upload,
      conflict = conflict
    )
  }
}

.projr_osf_upload_missing <- function(path_dir_local,
                                      osf_tbl,
                                      conflict) {
  fn_vec_local <- list.files(path_dir_local, recursive = TRUE)
  fn_vec_osf <- .projr_osf_ls_files(osf_tbl = osf_tbl)
  fn_vec_missing <- setdiff(fn_vec_local, fn_vec_osf)
  .projr_osf_upload_fn(
    fn_rel = fn_vec_missing, path_dir_local = path_dir_local,
    osf_tbl = osf_tbl, conflict = conflict
  )
}

# upload contents of a directory to a node
.projr_osf_upload_dir <- function(path_dir_local,
                                  osf_tbl,
                                  conflict = "overwrite") {
  fn_vec_local <- list.files(path_dir_local, recursive = TRUE)
  .projr_osf_upload_fn(
    fn_rel = fn_vec_local, path_dir_local = path_dir_local,
    osf_tbl = osf_tbl, conflict = conflict
  )
}

# upload files within a directory to a node
.projr_osf_upload_fn <- function(fn_rel, # relative to directory
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
