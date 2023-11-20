.projr_osf_dest_upload <- function(output_run) {
  # consider early exit
  # ------------------

  if (!.projr_osf_send_check_run(output_run)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------


  for (i in seq_along(yml_projr_osf)) {
    yml_projr_osf_ind <- yml_projr_osf[[i]]
    yml_projr_osf_ind_upload <- yml_projr_osf_ind[["upload"]]
    .projr_osf_send_yml_content(
      id = yml_projr_osf_ind[["id"]],
      remote_structure = yml_projr_osf_ind[["remote-structure"]],
      content = yml_projr_osf_ind[["content"]],
      path = yml_projr_osf_ind[["path"]], ,
      path_append_label = yml_projr_osf_ind[["path_append_label"]],
      cue = yml_projr_osf_ind_upload[["cue"]],
      sync_approach = yml_projr_osf_ind_upload[["sync-approach"]],
      version_source = yml_projr_osf_ind_upload[["version-source"]],
      conflict = yml_projr_osf_ind_upload[["conflict"]],
      component = yml_projr_osf_ind[["component"]]
    )
  }

  # upload manifest right at the end,
  # so that it's always the previous version uploaded
  # when comparing
  for (i in seq_along(yml_projr_osf)) {
    .projr_osf_send_node_manifest(
      title = names(yml_projr_osf)[i], yml_projr_osf[[i]], id_parent = NULL
    )
  }
}

.projr_osf_send_check_run <- function(output_run) {
  yml_projr <- projr_yml_get()
  # either a dev run or else no osf upload specified
  if ((!output_run) ||
    (!"osf" %in% names(yml_projr[["build"]]))) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_send_node <- function(title, yml_param, id_parent = NULL) {
  osf_tbl <- .projr_osf_get_node(
    title = title, yml_param = yml_param, id_parent = id_parent
  )
  # okay, so I actually need to perform recursive uploads
  for (x in yml_param[["content"]]) {
    osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
    .projr_osf_send_node_label(
      osf_tbl = osf_tbl, osf_tbl_file = osf_tbl_file,
      label = x, method = yml_param[["method"]]
    )
  }
}

#'
.projr_osf_send_node_label <- function(osf_tbl,
                                       osf_tbl_file,
                                       label,
                                       manifest_source = "manifest") {
  # upload all files if directory not present
  # need to adjust for path_append_label and path_sub
  upload_complete <- .projr_osf_send_node_label_new(
    osf_tbl = osf_tbl, osf_tbl_file = osf_tbl_file, label = label
  )
  if (upload_complete) {
    return(invisible(TRUE))
  }

  if (manifest_source == "manifest") {
    # upload files if hash different
    manifest_tbl_local <- .projr_manifest_read()
    manifest_tbl_osf <- .projr_osf_download_manifest(osf_tbl)

    # need to check that we're working with the correct version

    # sort out that thing about the IDs
    # WRONG.
    manifest_list_compare <- .projr_manifest_compare(
      manifest_tbl_local,
      manifest_tbl_osf
    )
  } else if (manifest_source == "download") {
    # download files and compare
    # (this is slower, but more accurate)
    dir_save <- file.path(tempdir, "manifest", label)
    .projr_osf_download_node_label(
      osf_tbl,
      dir_save = dir_save
    )
    # need to generate the manifest
  } else {
    stop("manifest_source must be either 'manifest' or 'download'")
  }
  manifest_tbl_added <- manifest_list_compare[["added"]]
  if (nrow(manifest_tbl_added) > 0) {

  }
  # so, what do we do with this information?
  #
  # need to get the entire listing on OSF
  # could recurse, or just download the entire thing
  # (might be faster)
}

.projr_osf_send_node_label_new <- function(osf_tbl,
                                           osf_tbl_file,
                                           label) {
  dir_label <- projr_dir_get("label", output_safe = FALSE)
  label_present <- label %in% osf_tbl_file[["name"]]
  if (label_present) {
    return(FALSE)
  }
  if (!label_present) {
    osfr::osf_upload(
      x = osf_tbl, file = dir_label, conflicts = "overwrite"
    )
  }
  TRUE
}

.projr_osf_send_node_label_add <- function(osf_tbl,
                                           osf_tbl_file,
                                           manifest_added,
                                           manifest_tbl_osf) {
  dir_label <- projr_dir_get("label", output_safe = FALSE)
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files(n_max = Inf)
  label_present <- label %in% osf_tbl_file[["name"]]
}

.projr_osf_send_node_manifest <- function(title,
                                          yml_param,
                                          id_parent) {
  osf_tbl <- .projr_osf_get_node(
    title = title, yml_param = yml_param, id_parent = id_parent
  )
  osfr::osf_upload(
    x = osf_tbl,
    file = projr_path_get("project", "manifest.csv"),
    conflicts = "overwrite"
  )
}
