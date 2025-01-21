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
  remote_dest_and_exists_list <- 
    .projr_dest_send_label_get_remote_dest_and_exists(
      type, label, yml_title
    )
  remote_comparison_and_info_list <- 
    .projr_dest_send_label_get_remote_comp_and_info(
      type, label, yml_title, remote_dest_and_exists_list[["remote"]],
      remote_dest_and_exists_list[["exists"]]
    )
}

.projr_dest_send_label_get_remote_dest_and_exists <- function(type,
                                                              label,
                                                              yml_title) {
    remote_dest_exists <- .projr_remote_final_check_exists(
    type, yml_title[["id"]], label, yml_title[["structure"]],
    yml_title[["path"]], yml_title[["path-append-label"]], NULL
  )
  # this is now created, which is a problem...
  # if the upload fails somehow, we will have an intermediate state.
  # we also need to delete it at the end, if nothing is uploaded
  # and it's not meant to be empty (e.g. if there
  # was no change and cue was if-change)
  remote_dest <- .projr_remote_get_final(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]], label, yml_title[["structure"]]
  )
  list(
    "remote" = remote_dest,
    "exists" = remote_dest_exists
  )
}

.projr_dest_send_label_get_remote_comp_and_info <- function(type,
                                                            label,
                                                            yml_title,
                                                            remote_dest,
                                                            remote_dest_exists) {
  skip_upload <- grepl("^upload", yml_title[["structure"]])
  if (grepl("^upload", yml_title[["structure"]])) {
    return(list("remote" = NULL, "exists" = NULL, "version" = NULL))
  }

  version_comparison <-  .projr_remote_get_version_label(
    # TODO: need
    remote_dest, type, label
    )
  remote_comparison_exists <- .projr_remote_final_check_exists(
    type, yml_title[["id"]], label, yml_title[["structure"]],
    yml_title[["path"]], yml_title[["path-append-label"]], version_comparison
  )
  if (!remote_comparison_exists) {
    return(list("remote" = NULL, "exists" = FALSE, "version" = NULL))
  }
  # not necessarily the same as remote_dest, as it could
  # be a previous version (typically so if structure is archive).
  # it will now exist, for OSF and local, whether it did or not before.
  # (#TODO: guard against there being no version_comparison
  remote_comparison <- .projr_remote_get_final(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]], label, yml_title[["structure"]],
    version_comparison
  )
  list(
    "exists" = remote_comparison_exists,
    "remote" = remote_comparison,
    "version" = version_comparison
  )
}

.projr_dest_send_label_get_remote_dest <- function(type,
                                                   id,
                                                   path,
                                                   path_append_label,
                                                   label,
                                                   structure) {
  .projr_remote_get_final(
    type, id, label, structure, path, path_append_label
  )
}

.projr_dest_send_label_get_remote_comparison <- function(type,
                                                         id,
                                                         path,
                                                         path_append_label,
                                                         label,
                                                         structure,
                                                         version,
                                                         remote_dest) {                                           
  switch(structure,
    "latest" = .projr_dest_send_label_get_remote_comparison_latest(
      remote_dest
    ),
    "archive" = .projr_dest_send_label_get_remote_comparison_archive(
      type, id, path, path_append_label, label, structure, version, remote_dest
    )
  )
}

.projr_dest_send_label_get_remote_comparison_latest <- function(remote_dest) {
  remote_dest
}

.projr_dest_send_label_get_remote_comparison_archive <- function(type,
                                                                 id,
                                                                 path,
                                                                 path_append_label,
                                                                 label,
                                                                 structure,
                                                                 version,
                                                                 remote_dest) {     
  version_latest <- .projr_remote_get_version_label(remote_dest, type, label)
  remote_comparison_exist
  remote_comparison <- .projr_remote_get_final(
    type, id, label, structure, path, path_append_label, version_latest
  )

  # TODO: check that remote_compairson actually exists #557
  remote_comparison
}

