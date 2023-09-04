.projr_pb_upload <- function(output_run) {
  # consider early exit
  # ------------------

  if (!.projr_pb_check_run(output_run)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    renv::install("piggyback", prompt = FALSE)
    .projr_dep_add("piggyback")
  }
  for (i in seq_along(projr_yml_get()[["build"]][["github-release"]])) {
    gh_tbl_release <- try(
      suppressWarnings(suppressMessages(piggyback::pb_releases()))
    )
    if (identical(class(gh_tbl_release), "try-error")) {
      Sys.sleep(3)
      gh_tbl_release <- try(
        suppressWarnings(suppressMessages(piggyback::pb_releases()))
      )
      if (identical(class(gh_tbl_release), "try-error")) {
        warning("Could not upload to GitHub release.")
        break
      }
    }
    yml_projr_gh_ind <- projr_yml_get()[["build"]][["github-release"]][[i]]
    tag <- names(projr_yml_get()[["build"]][["github-release"]])[i]
    tag <- switch(tag,
      `@version` = paste0("v", projr_version_get()),
      tag
    )
    tag <- gsub("^ +", "", tag)
    tag < gsub(" +$", "", tag)
    tag <- gsub(" ", "-", tag)
    body <- yml_projr_gh_ind[["body"]]
    if (!tag %in% gh_tbl_release[["release_name"]]) {
      pb_release_create <- try(
        piggyback::pb_release_create(tag = tag, body = body)
      )
      if (identical(class(pb_release_create), "try-error")) {
        Sys.sleep(3)
        pb_release_create <- try(
          piggyback::pb_release_create(tag = tag, body = body)
        )
        if (identical(class(pb_release_create), "try-error")) {
          warning(paste0("Could not create a GitHub release with tag ", tag))
          next
        } else {
          Sys.sleep(1.5)
        }
      } else {
        Sys.sleep(1.5)
      }
    } else {
      if (FALSE) {
        pb_release_delete <- try(piggyback::pb_release_delete(tag = tag))
        if (identical(class(pb_release_delete), "try-error")) {
          Sys.sleep(2)
          pb_release_delete <- try(piggyback::pb_release_delete(tag = tag))
          if (identical(class(pb_release_delete), "try-error")) {
            warning(paste0(
              "Could not delete prior GitHub release with tag ", tag
            ))
            next
          }
        }
        pb_release_create <- try(
          piggyback::pb_release_create(tag = tag, body = body)
        )
        if (identical(class(pb_release_create), "try-error")) {
          Sys.sleep(2)
          pb_release_create <- try(
            piggyback::pb_release_create(tag = tag, body = body)
          )
          if (identical(class(pb_release_create), "try-error")) {
            warning(paste0("Could not re-create a GitHub release with tag ", tag))
            next
          }
        }
      }
    }
    # no need to upload anything more if a code-only release
    if (identical("code", yml_projr_gh_ind[["content"]])) {
      next
    }

    label_vec <- setdiff(yml_projr_gh_ind[["content"]], "code")

    for (j in seq_along(label_vec)) {
      label <- label_vec[j]
      path_zip <- .projr_zip_dir_pb(
        tag = tag, label = label,
        output_run = output_run
      )
      if (!file.exists(path_zip)) {
        next
      }
      pb_release_upload <- try(piggyback::pb_upload(file = path_zip, tag = tag))
      if (identical(class(pb_release_upload), "try-error")) {
        Sys.sleep(2)
        pb_release_upload <- try(
          piggyback::pb_upload(file = path_zip, tag = tag)
        )
        if (identical(class(pb_release_upload), "try-error")) {
          warning(paste0(
            "Could not upload ", label, " to GitHub release with tag ", tag
          ))
          next
        }
      }
    }
  }
  #
  invisible(TRUE)
}

.projr_pb_check_run <- function(output_run) {
  yml_projr <- projr_yml_get()
  # either a dev run or else no github-release specified
  if ((!output_run) ||
    (!"github-release" %in% names(yml_projr[["build"]]))) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_zip_dir_pb <- function(tag, label, output_run) {
  # paths
  # get path to copy from
  path_dir <- .projr_pb_path_get_dir(
    label = label, output_run = output_run
  )
  # path to zip to
  path_zip <- .projr_pb_path_get_zip(tag = tag, label = label)

  # exclude special folders
  # projr_output from cache folder(s)
  dir_exc <- .projr_pb_path_get_dir_exc(label = label)

  dir_inc <- NULL

  # zip
  zip_outcome <- .projr_zip_dir(
    path_dir = path_dir,
    path_zip = path_zip,
    dir_exc = dir_exc,
    dir_inc = dir_inc
  )
  switch(as.character(zip_outcome),
    "TRUE" = path_zip,
    "FALSE" = character(1)
  )
}

.projr_pb_path_get_dir <- function(label, output_run) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_dir <- projr_dir_get(label, output_safe = !output_run)
  if (!fs::is_absolute_path(path_dir)) {
    path_dir <- file.path(dir_proj, path_dir)
  }
  path_dir
}

.projr_pb_path_get_zip <- function(tag, label) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_zip <- projr_path_get(
    "cache", "projr", "gh_release", tag, paste0(label, ".zip")
  )
  if (!fs::is_absolute_path(path_zip)) {
    path_zip <- file.path(dir_proj, path_zip)
  }
  if (file.exists(path_zip)) {
    file.remove(path_zip)
  }
  path_zip
}

.projr_pb_path_get_dir_exc <- function(label) {
  if (!grepl("^cache", .projr_dir_label_strip(label))) {
    return("projr_gh_release")
  }
  yml_projr_dir <- projr_yml_get()[["directories"]]
  key_vec_match_cache <- .projr_dir_label_strip(names(yml_projr_dir))
  key_copy_vec_cache_ind <- which(grepl("^output", key_vec_match_cache))
  key_copy_vec_cache <- names(yml_projr_dir)[key_copy_vec_cache_ind]
  dir_exc <- paste0("projr-", key_copy_vec_cache)
  c(dir_exc, "projr_gh_release")
}
