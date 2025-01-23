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
  plan <- .projr_dest_send_label_get_plan(
    type, label, path_dir_local, yml_title[["path_append_label"]],
    yml_title[["label"]], yml_title[["structure"]], yml_title[["strategy"]],
    path_dir_local
  )

}

.projr_dest_send_label_get_plan <- function(type,
                                            id,
                                            path,
                                            path_append_label,
                                            label,
                                            structure,
                                            strategy,
                                            path_dir_local) {
  remote_list <- .projr_dest_send_label_get_plan_get_remotes(
    type, id, path, path_append_label, label, structure,
    strategy
  )
  is_upload_empty <-
    .projr_dest_send_label_get_plan_is_upload_empty(strategy, path_dir_local)
  if (is_upload_empty) {
    return(.projr_dest_send_label_get_plan_upload_empty(
      type, remote_list[["remote_pre"]], label
    ))
  }

  version_min_acceptable <-
    .projr_dest_send_label_get_plan_get_version_min_acceptable(
      remote_list[["version_comp"]], label, strategy
    )
}

.projr_dest_send_label_get_plan_is_upload_empty <- function(strategy,
                                                            path_dir_local) {
  is_upload <- grepl("^upload", strategy)
  is_empty <- .is_len_0(.file_ls(path_dir_local))
  is_upload && is_empty
}

.projr_dest_send_label_get_plan_upload_empty <- function(type,
                                                         remote_pre,
                                                         label) {
  version_file <- .projr_dest_send_label_get_plan_upload_empty_version(
    type, remote_pre, label
  )
  manifest <- .projr_dest_send_label_get_plan_upload_empty_manifest(
    type, remote_pre, label
  )
}

.projr_dest_send_label_get_plan_upload_empty_version <- function(type,
                                                                 remote_pre,
                                                                 label) {
  version_remote <- .projr_remote_get_version_file(type, remote_pre)
  version_remote <- .projr_version_file_update_project_version(
    version_remote
  )
  version_remote |>
    .projr_version_file_update_label_version(label, TRUE)
}

.projr_dest_send_label_get_plan_upload_empty_manifest <- function(type,
                                                                  remote_pre,
                                                                  label) {
  # TODO: start here
}

.projr_dest_send_label_get_plan_get_remotes <- function(type,
                                                        id,
                                                        path,
                                                        path_append_label,
                                                        label,
                                                        structure,
                                                        strategy) {
  remote_pre <- .projr_remote_get_final(
    type, id, label, structure, path, path_append_label, NULL, TRUE
  )
  remote_dest <- .projr_remote_get_final_if_exists(
    type, id, label, structure, path, path_append_label, NULL
  )
  version_comp <- .projr_dest_send_label_get_plan_get_version_comp(
    remote_pre, type, label, structure, strategy
  )
  remote_comp <- .projr_dest_send_label_get_plan_get_remotes_comp(
    type, id, label, structure, path, path_append_label, version_comp
  )
  list(
    "remote_pre" = remote_pre,
    "remote_dest" = remote_dest,
    "remote_comp" = remote_comp,
    "version_comp" = version_comp
  )
}

.projr_dest_send_label_get_plan_get_version_comp <- function(remote_pre,
                                                             type,
                                                             label,
                                                             structure,
                                                             strategy) {
  
 }

.projr_dest_send_label_get_plan_get_remotes_comp <- function(type,
                                                             id,
                                                             label,
                                                             structure,
                                                             path,
                                                             path_append_label,
                                                             version) {
  if (is.null(version)) {
    NULL
  } else {
    .projr_remote_get_final(
      type, id, label, structure, path, path_append_label, version
    )
  }
}

.projr_dest_send_label_get_plan_get_version_min_acceptable <- function(version_comp,
                                                                       label,
                                                                       strategy) {
  version_min_acceptable <- projr_version_get() |>
    .projr_version_v_rm()
  if (is.null(version_comp) || grepl("^upload", strategy)) {
    return(version_min_acceptable)
  }
  manifest_project <- .projr_remote_get_manifest_project() |>
    .projr_manifest_filter_label(label)
  rownames(manifest_project) <- NULL
  manifest_latest <- manifest_project |>
    .projr_manifest_filter_version(projr_version_get())
  version_vec <- manifest_latest[["version"]] |>
    .projr_version_v_rm() |> 
    package_version() |>
    sort()
  version_vec <- version_vec[version_vec >= package_version(version_comp)]
  if (.is_len_0(version_vec)) {
    return(version_min_acceptable)
  }
  version_vec <- version_vec |> rev()
  for (i in seq_along(version_vec)) {
    version_curr <- version_vec[[i]]
    manifest_curr <- manifest_project |>
      .projr_manifest_filter_version(version_curr)
    if (!identical(manifest_curr, manifest_latest)) {
      return(version_curr)
    }
    version_min_acceptable <- version_curr
  }
  version_min_acceptable
}