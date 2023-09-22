.projr_osf_upload <- function(output_run) {
  # consider early exit
  # ------------------

  if (!.projr_osf_upload_check_run(output_run)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------

  if (!requireNamespace("osfr", quietly = TRUE)) {
    renv::install("osfr", prompt = FALSE)
    .projr_dep_add("osfr")
  }

  yml_projr_osf <- projr_yml_get()[["build"]][["osf"]]
  for (i in seq_along(yml_projr_osf)) {
    # need to make this recursive
    .projr_osf_upload_node(
      title = names(yml_projr_osf)[i], yml_projr_osf[[i]], parent_id = NULL
    )
  }

  # upload manifest right at the end,
  # so that it's always the previous version uploaded
  # when comparing
  for (i in seq_along(yml_projr_osf)) {
    .projr_osf_upload_node_manifest(
      title = names(yml_projr_osf)[i], yml_projr_osf[[i]], parent_id = NULL
    )
  }
}

.projr_osf_upload_check_run <- function(output_run) {
  yml_projr <- projr_yml_get()
  # either a dev run or else no osf upload specified
  if ((!output_run) ||
    (!"osf" %in% names(yml_projr[["build"]]))) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_upload_node <- function(title, yml_param, parent_id = NULL) {
  osf_tbl <- .projr_osf_get_node(
    title = title, yml_param = yml_param, parent_id = parent_id
  )
  # okay, so I can actually need to perform recursive uploads
  for (x in yml_param[["content"]]) {
    osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
    .projr_osf_upload_node_label(
      osf_tbl = osf_tbl, osf_tbl_file = osf_tbl_file,
      label = x, method = yml_param[["method"]]
    )
  }
}

#'
#'
.projr_osf_upload_node_label <- function(osf_tbl,
                                         osf_tbl_file,
                                         label,
                                         source_manifest = "manifest") {
  # upload all files if directory not present
  upload_complete <- .projr_osf_upload_node_label_new(
    osf_tbl = osf_tbl, osf_tbl_file = osf_tbl_file, label = label
  )
  if (upload_complete) {
    return(invisible(TRUE))
  }

  if (source_manifest == "manifest") {
    # upload files if hash different
    manifest_tbl_local <- .projr_manifest_read()
    manifest_tbl_osf <- .projr_osf_download_manifest(osf_tbl)

    # need to check that we're working with the correct version

    # sort out that thing about the IDs
    manifest_list_compare <- .projr_manifest_compare(
      manifest_tbl_local,
      manifest_tbl_osf
    )
  } else if (source_manifest == "download") {
    # download files and compare
    # (this is slower, but more accurate)
    dir_save <- file.path(tempdir, "manifest", label)
    .projr_osf_download_node_label(
      osf_tbl,
      dir_save = dir_save
    )
    # need to generate the manifest
  } else {
    stop("source_manifest must be either 'manifest' or 'download'")
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

.projr_osf_upload_node_label_new <- function(osf_tbl,
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

.projr_osf_upload_node_label_add <- function(osf_tbl,
                                             osf_tbl_file,
                                             manifest_added,
                                             manifest_tbl_osf) {
  dir_label <- projr_dir_get("label", output_safe = FALSE)
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  label_present <- label %in% osf_tbl_file[["name"]]
}
