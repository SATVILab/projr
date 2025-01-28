# ================================
# send functions
# ================================

# for a single label
# ------------------------

.projr_dest_send_label <- function(label,
                                   title,
                                   type,
                                   output_run,
                                   upload_github,
                                   upload_force) {
  force(title)
  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run)
  yml_title <- .projr_yml_dest_get_title_complete(
    title, type, NULL, upload_github, upload_force
    )
  remote <- .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    label = label,
    structure = yml_title[["structure"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]]
  )

  # get overall type of plan
  plan <- .projr_dest_send_get_plan(
    inspect = yml_title[["send"]][["inspect"]],
    strategy = yml_title[["send"]][["strategy"]],
    type = type,
    structure = yml_title[["structure"]]
  )

  # get details of what files to remove and add
  plan_list_detail <- .projr_dest_send_get_plan_detail(
    plan = plan,
    label = label,
    path_dir_local = path_dir_local,
    remote = remote,
    type = type,
    inspect = yml_title[["send"]][["inspect"]]
  )

  # remove and upload files as stated
  .projr_plan_implement(
    plan = plan,
    plan_detail = plan_list_detail,
    remote = remote,
    type = type,
    structure = yml_title[["structure"]],
    path_dir_local = path_dir_local,
    conflict = yml_title[["send"]][["conflict"]]
  )
  
  # update manifest.csv and VERSION file
  .projr_dest_send_label_versioning_update(
    type = type, 
    remote = remote,
    label = label,
    plan_detail = plan_list_detail
  )
}

.projr_dest_send_label_get_remote_final_yml <- function(type,
                                                        label,
                                                        yml_title) {
  .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    label = label,
    structure = yml_title[["structure"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]]
  )
}

# ================================
# clear function
# ================================

# delete the remote if it's empty and it's versioned
.projr_dest_send_label_clear <- function(strategy,
                                         type,
                                         remote,
                                         structure) {
  if (!.is_opt(strategy, "sync-using-deletion")) {
    return(invisible(FALSE))
  }
  .projr_remote_rm_final_if_empty(type, remote, structure)
}


# ================================
# update versioning files
# ================================

.projr_dest_send_label_versioning_update <- function(type,
                                                     remote,
                                                     label,
                                                     plan_detail) {
  .projr_dest_send_label_versioning_update_manifest(
    type, remote, label, plan_detail
  )
  .projr_dest_send_label_versioning_update_version_file(
    type, remote, label, plan_detail
  )
}

.projr_dest_send_label_versioning_update_manifest <- function(type, 
                                                              remote, 
                                                              label, 
                                                              plan_detail) {
  manifest_remote <- .projr_remote_get_manifest(type, remote)
  manifest_project <- .projr_manifest_read_project()
  manifest_add <- manifest_project |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(projr::projr_version_get())
  manifest_final <-
    .projr_manifest_append_previous_actual(manifest_add, manifest_remote) |>
    .projr_manifest_remove_duplicate()
  .projr_remote_write_manifest(type, remote, manifest_final)
  invisible(TRUE)
}

.projr_dest_send_label_versioning_update_check_skip <- function(plan_detail) {
  .is_len_0(plan_detail[["add"]]) && .is_len_0(plan_detail[["rm"]])
}

.projr_dest_send_label_versioning_update_version_file <- function(type,
                                                                  remote,
                                                                  label,
                                                                  plan_detail) {
  version_file_remote <- .projr_remote_get_version_file(type, remote)
  version_file <- .projr_version_file_update_project_version(
    version_file_remote
  )
  check_skip_update_label <-
    .projr_dest_send_label_versioning_update_check_skip_version_file(
      plan_detail, version_file, label
    )
  if (!check_skip_update_label) {
    version_file <- .projr_version_file_update_label_version(
      version_file, label
    )
  }
  .projr_remote_write_version_file(type, remote, version_file)
}

.projr_dest_send_label_versioning_update_check_skip_version_file <-
  function(plan_detail, version_file, label) {
    .is_len_0(plan_detail[["add"]]) && .is_len_0(plan_detail[["rm"]]) &&
      any(grepl(paste0("^", label, ": "), version_file))
}
