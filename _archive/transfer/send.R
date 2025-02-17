.send <- function(bump_component,
                        id,
                        structure,
                        content,
                        path,
                        path_append_label,
                        cue = NULL,
                        strategy,
                        inspect,
                        conflict,
                        component) {
  # whether to run or not
  switch(remote_type,
    "osf" = .send_osf(
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict,
      sub_dir = sub_dir
    ),
    "local" = .send_local(
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict,
      sub_dir = sub_dir
    )
  )
}

# send where we decide what to send based on version
# ---------------------------

.send_version <- function(label,
                                bump_component,
                                remote_type,
                                remote_base = NULL,
                                remote_final = NULL,
                                path_remote_rel = NULL,
                                inspect) {
  output_run <- .run_output_check(bump_component)
  change_list <- .change_get(
    label = label,
    remote_base = remote_base,
    remote_final = remote_final,
    path_remote_rel = path_remote_rel,
    remote_type = remote_type,
    inspect = inspect
  )
  .send_file(
    fn_rel = change_list[["add"]],
    path_dir_local = projr::projr_dir_get(),
    dest = dest,
    remote_type = remote_type,
    conflict = conflict
  )
}

.change_get_file_remote <- function(path_dir_local,
                                          label = NULL,
                                          bump_component = NULL,
                                          remote_type,
                                          remote_base = NULL,
                                          remote_final = NULL,
                                          path_remote_rel = NULL) {
  # download from remote, returns directory where it
  # downloads to
  path_dir_local_from_remote <- switch(remote_type,
    "local" = .get_dir_local(
      label = label, bump_component = bump_component
    ),
    "osf" = NULL
  )
}


# utility functions
# --------------------------

.send_dir <- function(path_dir_local,
                            dest,
                            remote_type,
                            conflict = "overwrite") {
  switch(remote_type,
    "local" = .local_send_dir(
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict
    ),
    "osf" = .osf_send_dir(
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict
    )
  )
}

.send_file <- function(fn_rel,
                             path_dir_local,
                             dest,
                             remote_type,
                             conflict) {
  switch(remote_type,
    "local" = .local_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      path_dir_local_dest = dest,
      conflict = conflict
    ),
    "osf" = .osf_send_file(
      fn_rel = fn_rel,
      path_dir_local = path_dir_local,
      osf_tbl = dest,
      conflict = conflict
    )
  )
}
