.projr_send <- function(bump_component,
                        id,
                        remote_structure,
                        content,
                        path,
                        path_append_label,
                        cue = is.null(),
                        sync_approach,
                        version_source,
                        conflict,
                        component) {
  # whether to run or not
  if (!.projr_cue_check(bump_component = bump_component)) {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "osf" = .projr_send_osf(
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict,
      sub_dir = sub_dir
    ),
    "local" = .projr_send_local(
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict,
      sub_dir = sub_dir
    )
  )
}

# send where we decide what to send based on version
# ---------------------------

.projr_send_version <- function(label,
                                bump_component,
                                remote_type,
                                remote_base = NULL,
                                remote_final = NULL,
                                path_remote_rel = NULL,
                                version_source) {
  output_run <- .projr_run_output_check(bump_component = bump_component)
  change_list <- .projr_change_get(
    label = label,
    remote_base = remote_base,
    remote_final = remote_final,
    path_remote_rel = path_remote_rel,
    remote_type = remote_type,
    version_source = version_source
  )
  .projr_send_file(
    fn_rel = change_list[["add"]],
    path_dir_local = projr::projr_dir_get(),
    dest = dest,
    remote_type = remote_type,
    conflict = conflict
  )
}

.projr_change_get_file_remote <- function(path_dir_local,
                                          label = NULL,
                                          bump_component = NULL,
                                          remote_type,
                                          remote_base = NULL,
                                          remote_final = NULL,
                                          path_remote_rel = NULL) {
  # download from remote, returns directory where it
  # downloads to
  path_dir_local_from_remote <- switch(remote_type,
    "local" = .projr_get_dir_local(
      label = label, bump_component = bump_component
    ),
    "osf" = NULL
  )
}


# utility functions
# --------------------------

.projr_send_dir <- function(path_dir_local,
                            dest,
                            remote_type,
                            conflict = "overwrite") {
  switch(remote_type,
    "local" = .projr_local_send_dir(
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict
    ),
    "osf" = .projr_osf_send_dir(
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict
    )
  )
}

.projr_send_file <- function(fn_rel,
                             path_dir_local,
                             dest,
                             remote_type,
                             conflict) {
  switch(remote_type,
    "local" = .projr_local_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict
    ),
    "osf" = .projr_osf_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict
    )
  )
}
