# send to all remotes when they're the destination
# --------------------------

.projr_dest_send <- function(bump_component,
                             upload_github,
                             upload_force) {
  # consider early exit
  # ------------------
  dont_exit_early <- .projr_dest_send_check(bump_component, upload_github)
  if (!dont_exit_early) {
    return(invisible(FALSE))
  }

  # loop over types of remotes
  type_vec <- .projr_dest_send_get_type(upload_github)
  for (type in type_vec) {
    .projr_dest_send_type(type, bump_component, upload_github, upload_force)
  }
}

.projr_dest_send_check <- function(bump_component,
                                   upload_github) {
  # output_run
  output_run <- .projr_build_get_output_run(bump_component)
  if (!output_run) {
    return(invisible(FALSE))
  }
  isTRUE(upload_github) || (.is_chr(upload_github, TRUE))
}

.projr_dest_send_get_type <- function(upload_github) {
  type_vec_yml <- .projr_dest_send_get_type_yml()
  type_vec_param <- .projr_dest_send_get_type_param(upload_github)
  unique(c(type_vec_yml, type_vec_param))
}

.projr_dest_send_get_type_yml <- function() {
  .projr_dest_send_get_type_opt()[
    .projr_dest_send_get_type_opt() %in% names(.projr_yml_build_get(NULL))
  ]
}

.projr_dest_send_get_type_param <- function(upload_github) {
  if (upload_github) "github" else character(0L)
}

.projr_dest_send_get_type_opt <- function() {
  c("local", "github", "osf")
}

# send to one type of remote
# -----------------------------

.projr_dest_send_type <- function(type,
                                  bump_component,
                                  upload_github,
                                  upload_force) {
  # ensure that these are not NULL only if not
  # specified in _projr.yml. Reaason is that,
  # if they are specified in the `yml`, the settings 
  # in the `yml` will be used, so we want to make
  # that consistent that the `content` setting is
  # also specified in the `yml`.
  # so, the parameters are not overrides.
  upload_github <- .projr_dest_send_type_update_github(type, upload_github)
  upload_force <- .projr_dest_send_type_update_force(type, upload_force)
  title_vec <- .projr_dest_send_type_get_title(type, upload_github)
  for (x in title_vec) {
    .projr_dest_send_title(
      x, type, bump_component, upload_github, upload_force
    )
  }
}

.projr_dest_send_type_update_github <- function(type, upload_github) {
  is_github <- type == "github"
  archive_not_specified <- !(
    "archive" %in% names(.projr_yml_dest_get_type("github", NULL))
  )
  if (is_github && archive_not_specified) {
    upload_github
  } else {
    FALSE
  }
}

.projr_dest_send_type_update_force <- function(type, upload_force) {
  is_github <- type == "github"
  archive_not_specified <- !(
    "archive" %in% names(.projr_yml_dest_get_type("github", NULL))
  )
  if (is_github && archive_not_specified) {
    upload_github
  } else {
    NULL
  }
}

.projr_dest_send_type_get_title <- function(type,
                                            upload_github) {
  nm_vec_yml <- names(.projr_yml_dest_get_type(type, NULL))
  nm_vec_param <- .projr_dest_send_type_get_title_param(
    type, upload_github
  )
  unique(c(nm_vec_yml, nm_vec_param))
}

.projr_dest_send_type_get_title_yml <- function(type) {
  .projr_yml_dest_get_type(type, NULL) |>
    names()
}

.projr_dest_send_type_get_title_param <- function(type,
                                                  upload_github) {
  if (type != "github" || isFALSE(upload_github)) {
    return(character(0L))
  }
  "archive"
}

# send to one label of a remote
# -----------------------------

.projr_dest_send_title <- function(title,
                                   type,
                                   bump_component,
                                   upload_github,
                                   upload_force) {
  force(title)

  if (!.projr_dest_send_title_check(title, type, bump_component)) {
    return(invisible(FALSE))
  }

  content_vec <- .projr_dest_send_title_get_content(
    title, type, upload_github
  )

  for (x in content_vec) {
    .projr_dest_send_label(
      x, title, type, .projr_build_get_output_run(bump_component), 
      upload_github, upload_force
    )
  }
  invisible(TRUE)
}

.projr_dest_send_title_check <- function(title, type, bump_component) {
  force(title)
  .projr_yml_dest_get_title_complete(title, type, NULL)[["cue"]] |>
    .is_cue(bump_component)
}

.projr_dest_send_title_get_content <- function(title,
                                               type,
                                               upload_github) {
  force(title)
  is_yml_content <- .projr_dest_send_title_get_content_check_yml(
    type, title, upload_github
  )
  if (is_yml_content) {
    .projr_dest_send_title_get_content_yml(title, type)
  } else {
    .projr_dest_send_title_get_content_param(upload_github)
  }
}

.projr_dest_send_title_get_content_check_yml <- function(type,
                                                         title,
                                                         upload_github) {
  is_github <- type == "github"
  is_archive_param <- title == "archive" && !isFALSE(upload_github)
  is_github_param <- is_github && is_archive_param
  !is_github_param
}

.projr_dest_send_title_get_content_yml <- function(title, type) {
  force(title)
  .projr_yml_dest_get_title(title, type, NULL)[["content"]]
}

.projr_dest_send_title_get_content_param <- function(upload_github) {
  force(title)
  if (.is_chr(upload_github, TRUE)) {
    upload_github
  } else {
    yml_dir <- .projr_yml_dir_get(NULL)
    nm_vec <- names(yml_dir) |> c("docs") |> unique()
    nm_vec_output <- nm_vec[.projr_yml_dir_label_class_detect_output(nm_vec)]
    nm_vec_raw <- nm_vec[.projr_yml_dir_label_class_detect_raw(nm_vec)]
    nm_vec_docs <- nm_vec[.projr_yml_dir_label_class_detect_docs(nm_vec)]
    c(nm_vec_output, nm_vec_raw, nm_vec_docs)
  }
}

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
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]],
    label = label,
    structure = yml_title[["structure"]]
  )

  # get overall type of plan
  plan <- .projr_dest_send_get_plan(
    version_source = yml_title[["send"]][["version-source"]],
    sync_approach = yml_title[["send"]][["sync-approach"]],
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
    version_source = yml_title[["send"]][["version-source"]]
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
}

.projr_dest_send_label_get_remote_final_yml <- function(type,
                                                        label,
                                                        yml_title) {
  .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]],
    label = label,
    structure = yml_title[["structure"]]
  )
}

.projr_dest_send_label_get_remote_final_github_force <- function(type) {
  .projr_remote_get_final(
    type = "github",
    id = "archive",
    path = NULL,
  )
}

# ================================
# clear function
# ================================

# delete the remote if it's empty and it's versioned
.projr_dest_send_label_clear <- function(sync_approach,
                                         type,
                                         remote,
                                         structure) {
  if (!.is_opt(sync_approach, "sync-using-deletion")) {
    return(invisible(FALSE))
  }
  .projr_remote_rm_final_if_empty(type, remote, structure)
}
