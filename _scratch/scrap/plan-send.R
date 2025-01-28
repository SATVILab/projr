# ----------------------------------------
# Upload empty
# ----------------------------------------

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
  list(
    fn_add = character(0L),
    fn_rm = character(0L),
    version_file = version_file,
    manifest = manifest,
    purge = FALSE,
    create = TRUE
  )
  # do we want to create an empty remote, if this is the case?
  # I think so, as if the remote_dest does not exist,
  # it should be "added" to
  # this will create a bunch of empty remotes if there is nothing,
  # but if you say `upload-all` and there is nothing, then
  # that's kind of what you should expect
  # (well, seems a high price to pay if all you want to do is skip versioning;
  # but we're still figuring out this option)
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
  manifest_remote <- .projr_remote_get_manifest(type, remote_pre)
  manifest_append <- .projr_empty_tbl_get_manifest(
    label, projr_version_get() |> .projr_version_v_rm()
  )
  manifest_append |>
    .projr_manifest_append_previous_actual(manifest_remote) |>
    .projr_manifest_remove_duplicate()
}

# ----------------------------------------
# Get minimum acceptable version
# ----------------------------------------

.projr_dest_send_label_get_plan_get_version_min_acceptable <- function(version_comp,
                                                                       label,
                                                                       strategy) {
  version_min_acceptable <- projr_version_get() |>
    .projr_version_v_rm() |>
    package_version()
  if (is.null(version_comp)) {
    # r
    return(.projr_manifest_get_version_earliest_match(label, NULL))
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

# ----------------------------------------
# If change acceptable
# ----------------------------------------

.projr_dest_send_label_get_plan_is_if_change_acceptable <- function(cue,
                                                                    strategy,
                                                                    version_comp) {
  is_if_change <- grepl("^if-change$", cue)
  is_sync <- grepl("^sync$", strategy)
  is_acceptable <- !is.null(version_comp)
  if_if_change && is_sync && is_acceptable
}

.projr_dest_send_label_get_plan_if_change_acceptable <- function(type,
                                                                 remote_pre) {
  version_file <-
    .projr_dest_send_label_get_plan_if_change_acceptable_version(
      type, remote_pre
    )
  manifest <-
    .projr_dest_send_label_get_plan_if_change_acceptable_manifest(
      type, remote_pre, label
    )
  list(
    fn_add = character(0L),
    fn_rm = character(0L),
    version_file = version_file,
    manifest = manifest,
    purge = FALSE,
    create = FALSE # do not create, as we're avoiding that as
    # there was no change and we're synchronising
  )
}

.projr_dest_send_label_get_plan_if_change_acceptable_version <- function(type,
                                                                         remote_pre) {
  .projr_remote_get_version_file(type, remote_pre) |>
    .projr_version_file_update_project_version()
  # don't update label as we didn't do anything to it
}

.projr_dest_send_label_get_plan_if_change_acceptable_manifest <- function(type,
                                                                          remote_pre) {
  manifest_remote <- .projr_remote_get_manifest(type, remote_pre)
  manifest_project_latest_label <- .projr_manifest_get_add_project(label)
  manifest_project_latest_label |>
    .projr_manifest_append_previous_actual(manifest_remote) |>
    .projr_manifest_remove_duplicate()
}



# ----------------------------------------
# Get information about what files
# are where and how they differ
# ----------------------------------------

.projr_dest_send_label_get_plan_info_fn <- function(type,
                                                    remote_pre,
                                                    strategy,
                                                    label,
                                                    version_comp) {
  switch(strategy,
    "upload-all" = .projr_dest_send_label_get_plan_info_fn_upload_all(
      label, version_comp
    ),
    "upload-missing" = .projr_dest_send_label_get_plan_info_fn_upload_missing(
      label, version_comp, remote_comp
    ),
    "sync-diff" = .projr_dest_send_label_get_plan_info_fn_sync_diff(
      type, remote_pre, label, version_comp
    ),
    "sync-purge" = .projr_dest_send_label_get_plan_info_fn_sync_purge(
      type, remote_pre, label, version_comp
    )
  )

}

.projr_dest_send_label_get_plan_diff_upload_all <- function(label,
                                                            version_comp) {
  .projr_remote_get_manifest_project() |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(version_comp)
}

.projr_dest_send_label_get_plan_diff_upload_missing <- function(label,
                                                                version_comp,
                                                                remote_dest) {

}

.projr_dest_send_label_get_plan_info_fn_sync_diff <- function(type,
                                                              remote_pre,
                                                              label,
                                                              version_comp,
                                                              remote_comp) {

  manifest_project <- .projr_remote_get_manifest_project() |>
    .projr_manifest_filter_label(label)
  manifest_remote <- .projr_dest_send_label_get_plan_diff_get_manifest_remote(
    type, remote_pre, label, version_comp, remote_comp
  )

  .projr_change_get_hash(
    hash_pre = manifest_remote, hash_post = manifest_project
    )
}

.projr_dest_send_label_get_plan_diff_get_manifest_remote <- function(type,
                                                                     remote_pre,
                                                                     label,
                                                                     version_comp,
                                                                     remote_comp) {
  if (is.null(version_comp)) {
    .projr_remote_hash(type, remote_comp, version_comp, label)
  }

  .projr_remote_get_manifest(type, remote_pre) |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(version_comp)
}

.projr_dest_send_label_get_plan_diff_get_manifest_remote_null <- function(type,
                                                                          remote_pre,
                                                                          label,
                                                                          version_comp,
                                                                          remote_comp) {
  hash_tbl <- .projr_change_get_file_dir(
    type, remote_comp
  ) |>
    .projr_hash_dir(version_comp) |>
    .projr_manifest_hash_cache_filter(label)
  if (nrow(hash_tbl) == 0) {
    .projr_empty_tbl_get_manifest(label, version_comp)
  } else {
    cbind(
      data.frame(label = rep(label, nrow(hash_tbl))),
      hash_tbl
    )
  }
}

# ----------------------------------------
# Get files to add and remove
# ----------------------------------------

.projr_dest_send_label_get_plan_fn <- function(change_list,
                                               strategy,
                                               cue) {
  if (!is.null(change_list)) {
    args_list <- change_list |>
      append(
        list(strategy = strategy, cue = cue)
      )
    do.call(.projr_dest_send_label_get_plan_fn_version, args_list)
  } else {

  }
}

.projr_dest_send_label_get_plan_fn_version <- function(added,
                                                       kept_unchanged,
                                                       kept_changed,
                                                       removed,
                                                       strategy,
                                                       cue) {

}
