# send to all remotes when they're the destination
# --------------------------

.dest_send <- function(bump_component,
                             upload_github,
                             upload_force) {
  # consider early exit
  # ------------------
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # loop over types of remotes
  type_vec <- .dest_send_get_type(upload_github)
  for (type in type_vec) {
    .dest_send_type(type, bump_component, upload_github, upload_force)
  }
}

.dest_send_check <- function(bump_component) {
  # output_run
  .build_get_output_run(bump_component) # nolint
}

.dest_send_get_type <- function(upload_github) {
  type_vec_yml <- .dest_send_get_type_yml()
  type_vec_param <- .dest_send_get_type_param(upload_github)
  unique(c(type_vec_yml, type_vec_param))
}

.dest_send_get_type_yml <- function() {
  .dest_send_get_type_opt()[
    .dest_send_get_type_opt() %in% names(.yml_build_get(NULL))
  ]
}

.dest_send_get_type_param <- function(upload_github) {
  if (upload_github) "github" else character(0L)
}

.dest_send_get_type_opt <- function() {
  c("local", "github", "osf")
}

# send to one type of remote
# -----------------------------

.dest_send_type <- function(type,
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
  upload_github <- .dest_send_type_update_github(type, upload_github)
  upload_force <- .dest_send_type_update_force(type, upload_force)
  title_vec <- .dest_send_type_get_title(type, upload_github)
  for (x in title_vec) {
    .dest_send_title(
      x, type, bump_component, upload_github, upload_force
    )
  }
}

.dest_send_type_update_github <- function(type, upload_github) {
  is_github <- type == "github"
  archive_not_specified <- !(
    "archive" %in% names(.yml_dest_get_type("github", NULL))
  )
  if (is_github && archive_not_specified) {
    upload_github
  } else {
    FALSE
  }
}

.dest_send_type_update_force <- function(type, upload_force) {
  is_github <- type == "github"
  archive_not_specified <- !(
    "archive" %in% names(.yml_dest_get_type("github", NULL))
  )
  if (is_github && archive_not_specified) {
    upload_force
  } else {
    NULL
  }
}

.dest_send_type_get_title <- function(type,
                                            upload_github) {
  nm_vec_yml <- names(.yml_dest_get_type(type, NULL))
  nm_vec_param <- .dest_send_type_get_title_param(
    type, upload_github
  )
  unique(c(nm_vec_yml, nm_vec_param))
}

.dest_send_type_get_title_yml <- function(type) {
  .yml_dest_get_type(type, NULL) |>
    names()
}

.dest_send_type_get_title_param <- function(type,
                                                  upload_github) {
  if (type != "github" || isFALSE(upload_github)) {
    return(character(0L))
  }
  "archive"
}

# send to one label of a remote
# -----------------------------

.dest_send_title <- function(title,
                                   type,
                                   bump_component,
                                   upload_github,
                                   upload_force) {
  force(title)
  may_send <- .dest_send_title_check(
    title, type, bump_component, upload_github, upload_force
  )

  if (!may_send) {
    return(invisible(FALSE))
  }

  content_vec <- .dest_send_title_get_content(
    title, type, upload_github
  )

  for (x in content_vec) {
    .dest_send_label(
      x, title, type, .build_get_output_run(bump_component),
      upload_github, upload_force
    )
  }
  invisible(TRUE)
}

.dest_send_title_check <- function(title,
                                         type,
                                         bump_component,
                                         upload_github,
                                         upload_force) {
  force(title)
  .yml_dest_get_title_complete(
    title, type, NULL, upload_github, upload_force
  )[["cue"]] |>
    .is_cue(bump_component)
}

.dest_send_title_get_content <- function(title,
                                               type,
                                               upload_github) {
  force(title)
  is_yml_content <- .dest_send_title_get_content_check_yml(
    type, title, upload_github
  )
  if (is_yml_content) {
    .dest_send_title_get_content_yml(title, type)
  } else {
    .dest_send_title_get_content_param(upload_github)
  }
}

.dest_send_title_get_content_check_yml <- function(type,
                                                         title,
                                                         upload_github) {
  is_github <- type == "github"
  is_archive_param <- title == "archive" && !isFALSE(upload_github)
  is_github_param <- is_github && is_archive_param
  !is_github_param
}

.dest_send_title_get_content_yml <- function(title, type) {
  force(title)
  .yml_dest_get_title(title, type, NULL)[["content"]]
}

.dest_send_title_get_content_param <- function(upload_github) {
  if (.is_chr(upload_github)) {
    upload_github
  } else {
    yml_dir <- .yml_dir_get(NULL)
    nm_vec <- names(yml_dir) |> c("docs") |> unique()
    nm_vec_output <- nm_vec[.yml_dir_label_class_detect_output(nm_vec)]
    nm_vec_raw <- nm_vec[.yml_dir_label_class_detect_raw(nm_vec)]
    nm_vec_docs <- nm_vec[.yml_dir_label_class_detect_docs(nm_vec)]
    c(nm_vec_output, nm_vec_raw, nm_vec_docs)
  }
}