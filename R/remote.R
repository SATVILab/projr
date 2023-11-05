# =====================
# main function
# =====================

# wrapper functions to get the "final" remote,
# i.e. including any sub-directories for
# specifying the version and so on for
# hierarchical remotes (like "local" and "OSF")
.projr_remote_final_get <- function(remote_type,
                                    remote_name,
                                    id,
                                    path,
                                    path_append_label,
                                    label,
                                    remote_structure) {
  switch(remote_type,
    "local" = .projr_remote_get_final_local(
      path = path,
      path_append_label = path_append_label,
      label = label,
      remote_structure = remote_structure
    ),
    "osf" = .projr_remote_get_final_osf(
      id = id,
      path = path,
      path_append_label = path_append_label,
      label = label,
      remote_structure = remote_structure
    ),
    "github" = .projr_remote_get_final_github(
      remote_name = remote_name
    ),
    stop(paste0("remote_type '", remote_type, "' not recognized"))
  )
}

# ---------------------
# local
# ---------------------

.projr_remote_get_final_local <- function(path,
                                          path_append_label,
                                          label,
                                          remote_structure) {
  # the local destination is just the
  # local directory where files are get, so
  # it is just the path.
  # note that `output_run` does not matter,
  # as this is a "remote" in the sense of something
  # interacted without either before or after the build process,
  # unlike the direoctires specified in
  # _projr.yml[["directories"]]`
  .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure,
    remote_type = "local"
  )
}

# ---------------------
# osf
# ---------------------

.projr_remote_get_final_osf <- function(id,
                                        path,
                                        path_append_label,
                                        label,
                                        remote_structure) {
  path_rel <- .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure
  )
  osf_tbl <- .projr_osf_get_node_id(id = id)
  if (length(path_rel) > 0L) {
    osf_tbl <- osfr::osf_mkdir(osf_tbl, path_rel)
  }
  osf_tbl
}

# ---------------------
# github
# ---------------------

.projr_remote_get_final_github <- function(remote_name) {
  # everything uploaded to a gh release
  # is a single file, and all other remotes
  # are just directories where files can
  # be uploaded to (and possibly folders,
  # but giithub releases don't do that), so
  # the remote for github release is just the
  # name of the tag
  # (given generally as the remote_name here)
  remote_name
}

# =====================
# get relative paths
# =====================

# path_rel
# ---------------------

# overall function
.projr_remote_get_path_rel <- function(path,
                                       path_append_label,
                                       label,
                                       remote_structure,
                                       remote_type) {
  fn <- switch(remote_type,
    "osf" = , # same as local
    "local" = .projr_remote_get_path_rel_hierarchy,
    "github" = .projr_remote_get_path_rel_flat
  )
  do.call(fn, list(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure
  ))
}

# hierarchical remotes
.projr_remote_get_path_rel_hierarchy <- function(path,
                                                 path_append_label,
                                                 label,
                                                 remote_structure) {
  args_list <- path
  if (path_append_label) {
    args_list <- args_list |> append(label)
  }
  if (remote_structure == "version") {
    args_list <- args_list |> append(projr_version_get())
  }
  do.call(file.path, args_list)
}

# flat remotes
.projr_remote_get_path_rel_flat <- function(...) character()


# ========================
# delete an unused empty remote directory
# ========================

.projr_remote_rm_final_empty <- function(remote_final,
                                         remote_type,
                                         remote_structure) {
  if (!remote_structure == "version") {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "local" = .projr_remote_rm_final_empty_local(
      remote_final = remote_final
    ),
    "osf" = .projr_remote_rm_final_empty_osf(
      remote_final = remote_final
    ),
    "github" = .projr_remote_rm_final_empty_github(
      remote_final = remote_final
    )
  )
}

# local
.projr_remote_rm_final_empty_local <- function(remote_final) {
  if (!dir.exists(remote_final)) {
    return(invisible(FALSE))
  }
  unlink(remote_final, recursive = TRUE)
}

# osf
.projr_remote_rm_final_empty_osf <- function(remote_final) {
  if (!inherits(remote_final, "osf_tbl_file")) {
    return(invisible(FALSE))
  }
  if (!nrow(osfr::osf_ls_files(remote_final)) > 0L) {
    return(invisible(FALSE))
  }
  osfr::osf_rm(x = remote_final, check = FALSE)
}

# github
.projr_remote_rm_final_empty_github <- function(remote_final) {
  # never any need to, as the release is only
  # created if it's to be uploaded to
  invisible(FALSE)
}

# ========================
# delete all contents of a remote
# ========================

# this is different to the above, as it
# only deletes the contents and not
# the remote directory itself (also,
# the above will only delete the remote if
# it's actually got nothing, so it's very different)
.projr_remote_empty <- function(remote,
                                remote_type) {
  switch(remote_type,
    "local" = .projr_remote_empty_local(remote = remote),
    "osf" = .projr_remote_empty_osf(remote = remote),
    "github" = .projr_remote_empty_github(remote = remote)
  )
}

# local
.projr_remote_empty_local <- function(remote) {
  if (!dir.exists(remote)) {
    return(invisible(FALSE))
  }
  dir_vec <- list.dirs(remote, recursive = TRUE)
  if (length(dir_vec) > 0) {
    unlink(dir_vec, recursive = TRUE)
  }
  fn_vec <- list.files(remote, full.names = TRUE, all.files = TRUE)
  if (length(fn_vec) > 0) {
    file.remove(file.path(remote, fn_vec))
  }
  invisible(TRUE)
}
