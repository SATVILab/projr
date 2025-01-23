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
  remote_list <- .projr_dest_send_label_get_remotes(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]], label, yml_title[["structure"]],
    yml_title[["send"]][["strategy"]]
  )

}

.projr_dest_send_label_get_remotes <- function(type,
                                               id,
                                               path,
                                               path_append_label,
                                               label,
                                               structure,
                                               strategy) {
  remote_pre <- .projr_dest_send_label_get_remote_pre(
    type, id, label, structure, path, path_append_label, NULL
  )
  remote_dest <- .projr_remote_get_final_if_exists(
    type, id, label, structure, path, path_append_label, NULL
  )
  version_comp <- .projr_dest_send_label_get_version_comp(
    remote_pre, type, label, structure, strategy
  )
  remote_comp <- .projr_dest_send_label_get_remotes_comp(
    type, id, label, structure, path, path_append_label, version_comp
  )
  list(
    "remote_pre" = remote_pre,
    "remote_dest" = remote_dest,
    "remote_comp" = remote_comp,
    "version_comp" = version_comp
  )
}

.projr_dest_send_label_get_remote_pre <- function(type,
                                                  id,
                                                  label,
                                                  structure,
                                                  path,
                                                  path_append_label,
                                                  version) {
  remote_pre_init <- .projr_remote_get_final(
    type, id, label, structure, path, path_append_label, version, TRUE
  )
}

.projr_dest_send_label_get_version_comp <- function(remote_pre,
                                                    type,
                                                    label,
                                                    structure,
                                                    strategy) {
  if (grepl("^upload", strategy)) {
    return(NULL)
  }
  .projr_remote_get_version_label(
    remote_pre, type, label, structure
  )
 }

.projr_dest_send_label_get_remotes_comp <- function(type,
                                                    id,
                                                    label,
                                                    structure,
                                                    path,
                                                    path_append_label,
                                                    version) {
  if (is.null(version_comparison)) {
    NULL
  } else {
    .projr_remote_get_final(
      type, id, label, structure, path, path_append_label, version_comparison
    )
  }
}
