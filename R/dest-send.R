# send to all remotes when they're the destination
# --------------------------

.dest_send <- function(bump_component,
                       archive_github,
                       archive_local,
                       always_archive) {
  # consider early exit
  # ------------------
  if (!.dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # loop over types of remotes
  type_vec <- .dest_send_get_type(archive_github, archive_local)
  for (type in type_vec) {
    archive_type <- .dest_send_get_archive_type(
      type, archive_github, archive_local
    )
    always_archive <- .dest_send_get_always_archive(
      type, archive_github, archive_local
    )
    .dest_send_type(
      type, bump_component, archive_type, always_archive
    )
  }
}

.dest_send_check <- function(bump_component) {
  # output_run
  .build_get_output_run(bump_component) # nolint
}

.dest_send_get_type <- function(archive_github, archive_local) {
  type_vec_yml <- .dest_send_get_type_yml()
  type_vec_param <- .dest_send_get_type_param(archive_github, archive_local)
  unique(c(type_vec_yml, type_vec_param))
}

.dest_send_get_type_yml <- function() {
  .dest_send_get_type_opt()[
    .dest_send_get_type_opt() %in% names(.yml_build_get(NULL))
  ]
}

.dest_send_get_type_param <- function(archive_github, archive_local) {
  out_vec <- character(0L)
  if (archive_github) {
    out_vec <- c(out_vec, "github")
  }
  if (archive_local) {
    out_vec <- c(out_vec, "local")
  }
  if (.is_len_0(out_vec)) {
    return(character(0L))
  }
  out_vec
}

.dest_send_get_type_opt <- function() {
  c("local", "github", "osf")
}

.dest_send_get_archive_type <- function(type,
                                        archive_github,
                                        archive_local) {
  if (type == "github") {
    # can be character, so just checking they're not FALSE
    !isFALSE(archive_github) && !is.null(archive_github) &&
      !"archive" %in% names(.yml_dest_get_type(type, NULL))
  } else if (type == "local") {
    !isFALSE(archive_local) && !is.null(archive_local) &&
      !"archive" %in% names(.yml_dest_get_type(type, NULL))
  } else {
    FALSE
  }
}

.dest_send_get_always_archive <- function(type, always_archive) {
  if ("archive" %in% names(.yml_dest_get_type(type, NULL))) {
    # if it was specified, then we don't want to
    # override it with the parameter
    NULL
  } else {
    always_archive
  }
}

# send to one type of remote
# -----------------------------

.dest_send_type <- function(type,
                            bump_component,
                            archive_type,
                            always_archive) {
  # ensure that these are not NULL only if not
  # specified in _projr.yml. Reaason is that,
  # if they are specified in the `yml`, the settings
  # in the `yml` will be used, so we want to make
  # that consistent that the `content` setting is
  # also specified in the `yml`.
  # so, the parameters are not overrides.
  title_vec <- .dest_send_type_get_title(type, archive_type)
  for (x in title_vec) {
    .dest_send_title(
      x, type, bump_component, archive_type, always_archive
    )
  }
}

.dest_send_type_get_title <- function(type,
                                      archive_github,
                                      archive_local) {
  nm_vec_yml <- names(.yml_dest_get_type(type, NULL))
  nm_vec_param <- .dest_send_type_get_title_param(
    type, archive_github, archive_local
  )
  unique(c(nm_vec_yml, nm_vec_param))
}

.dest_send_type_get_title_yml <- function(type) {
  .yml_dest_get_type(type, NULL) |>
    names()
}

.dest_send_type_get_title_param <- function(type,
                                            archive_github,
                                            archive_local) {

  is_title_param_and_true <- switch(
    type,
    github = !isFALSE(archive_github),
    local  = !isFALSE(archive_local),
    FALSE
  )
  if (is_title_param_and_true) "archive" else character(0L)
}

# send to one label of a remote
# -----------------------------

.dest_send_title <- function(title,
                             type,
                             bump_component,
                             archive_type,
                             always_archive) {
  force(title)
  may_send <- .dest_send_title_check(
    title, type, bump_component, archive_type, always_archive
  )

  if (!may_send) {
    return(invisible(FALSE))
  }

  content_vec <- .dest_send_title_get_content(
    title, type, archive_type
  )

  for (x in content_vec) {
    .dest_send_label(
      x, title, type, .build_get_output_run(bump_component),
      archive_type, always_archive, x == content_vec[length(content_vec)]
    )
  }

  .dest_send_type_changelog(
    title, type, .build_get_output_run(bump_component)
  )


  invisible(TRUE)
}

.dest_send_title_check <- function(title,
                                   type,
                                   bump_component,
                                   archive_type,
                                   always_archive) {
  force(title)
  .yml_dest_get_title_complete(
    title, type, NULL, archive_type, always_archive
  )[["cue"]] |>
    .is_cue(bump_component)
}

.dest_send_title_get_content <- function(title,
                                         type,
                                         archive_type) {
  force(title)
  is_yml_content <- .dest_send_title_get_content_check_yml(
    type, title, archive_type
  )
  if (is_yml_content) {
    .dest_send_title_get_content_yml(title, type)
  } else {
    .dest_send_title_get_content_param(archive_type)
  }
}

.dest_send_title_get_content_check_yml <- function(type,
                                                   title,
                                                   archive_local) {
  title == "archive" && !isFALSE(archive_local)
}

.dest_send_title_get_content_yml <- function(title, type) {
  force(title)
  .yml_dest_get_title(title, type, NULL)[["content"]]
}

.dest_send_title_get_content_param <- function(archive_type) {
  if (.is_chr(archive_type)) {
    archive_type
  } else {
    yml_dir <- .yml_dir_get(NULL)
    nm_vec <- names(yml_dir) |>
      c("docs") |>
      unique()
    nm_vec_output <- nm_vec[.yml_dir_label_class_detect_output(nm_vec)]
    nm_vec_raw <- nm_vec[.yml_dir_label_class_detect_raw(nm_vec)]
    nm_vec_docs <- nm_vec[.yml_dir_label_class_detect_docs(nm_vec)]
    c(nm_vec_output, nm_vec_raw, nm_vec_docs)
  }
}
