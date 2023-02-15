.projr_pb_upload <- function(output_run, bump_component) {
  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    browser()
  } else {
    return(invisible(FALSE))
  }
  yml_projr <- .projr_yml_get()
  # consider early exit
  # ------------------

  # either a dev run or else no github-release specified
  if ((!output_run) ||
    (!"github-release" %in% names(yml_projr[["build-output"]]))) {
    return(invisible(FALSE))
  }

  # if no item wants to be uploaded given the version bumped
  yml_projr_gh <- .projr_yml_get()[["build-output"]][["github-release"]]
  upload_vec_check <- vapply(yml_projr_gh, function(x) {
    .projr_version_comp_min_check(
      bump_component = bump_component,
      x[["version-component-min"]]
    )
  }, logical(1))
  if (!any(upload_vec_check)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    renv::install("piggyback")
  }

  # source code
  .projr_pb_upload_code(bump_component = bump_component)

  # doc
  .projr_pb_upload_doc(bump_component = bump_component)

  #
}

.projr_pb_upload_code <- function(bump_component) {
  yml_projr_gh <- projr_yml_get()[["github-release"]]
  if (!"source-code" %in% names(yml_projr_gh)) {
    return(invisible(FALSE))
  }

  gh_tbl_release <- suppressWarnings(suppressMessages(
    piggyback::pb_releases()
  ))

  # consider exiting
  # ------------------

  upload <- .projr_pb_upload_consider(
    yml_projr_gh_item = yml_projr_gh[[
      which(names(yml_projr_gh) == "source-code")
    ]],
    gh_tbl_release = gh_tbl_release,
    bump_component = bump_component
  )
  if (!upload) {
    return(invisible(FALSE))
  }

  # upload
  # ------------------

  tag <- paste0("v", projr_version_get())
  body <- "Project source code, inputs and/or outputs"

  # remove if already uploaded, as source is always latest
  if (tag %in% gh_tbl_release[["release_name"]]) {
    piggyback::pb_release_delete(tag = tag)
  }

  # always create new source code update
  # (and source code always first to be updated)
  piggyback::pb_release_create(tag = tag, body = body)
  invisible(TRUE)
}

.projr_pb_upload_consider <- function(yml_projr_gh_item,
                                      gh_tbl_release,
                                      bump_component) {
  # exit immediately if not to release
  .projr_version_comp_min_check(
    bump_component = bump_component,
    version_min =
    )
  if (!yml_projr_gh_item[["add"]]) {
    return(invisible(FALSE))
  }
  version_upload_min <- yml_projr_gh_item[["version-component-min"]]
  # upload if not wanted (at all or for this version component)
  version_vec_possible <- c("major", "minor", "patch")
  version_vec_upload <- switch(version_upload_min,
    "any" = version_vec_possible,
    version_vec_possible[
      seq_len(which(version_vec_possible == version_upload_min))
    ]
  )
  # exit immediately if not a high enough release level
  if (!bump_component %in% version_vec_upload) {
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

.projr_pb_upload_doc <- function(bump_component) {
  # get settings
  yml_projr_gh <- .projr_yml_get()[["build-output"]][["github-release"]]
  yml_projr_gh_doc <- yml_projr_gh[["bookdown"]]

  # consider not uploading
  # ----------------------------
  if (!yml_projr_gh_doc[["add"]]) {
    return(invisible(FALSE))
  }
  if (!.projr_version_comp_min_check(
    bump_component = bump_component,
    version_min = yml_projr_gh_doc[["version-component-min"]]
  )
  ) {
    return(invisible(FALSE))
  }

  # zip
  # --------------------------
  dir_doc <- projr_dir_get("bookdown")
  path_zip <- file.path(dirname(dir_doc), "doc.zip")
  if (file.exists(path_zip)) {
    file.remove(path_zip)
  }
  if (!dir.exists(dirname(path_zip))) {
    dir.create(dirname(path_zip))
  }
  setwd(dir_doc)
  path_zip <- paste0(basename(dir_doc), ".zip")
  utils::zip(
    path_zip,
    files = list.files(
      getwd(),
      recursive = TRUE, full.names = FALSE, all.files = TRUE
    ),
    flags = "-r9Xq"
  )

  # upload
  # --------------------------
  setwd(dir_proj)
  gh_tbl_release <- suppressWarnings(suppressMessages(
    piggyback::pb_releases()
  ))
  tag <- switch(item_list[["name"]],
    "@version" = paste0("v", version_current),
    item_list[["name"]]
  )
  body <- switch(item_list[["name"]],
    "@version" = paste0("Version-linked source code, project inputs and/or outputs"),
    "Project source code, inputs and/or outputs"
  )
  if (!tag %in% gh_tbl_releases[["release_name"]]) {
    piggyback::pb_release_create(tag = tag, body = body)
    piggyback::pb_upload(
      file = file.path(dir_doc, path_zip), overwrite = TRUE, tag = tag
    )
  } else {
    piggyback::pb_upload(
      file = file.path(dir_doc, path_zip), overwrite = TRUE, tag = tag
    )
  }
  invisible(TRUE)
}
