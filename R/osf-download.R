.projr_osf_download_node_manifest <- function(osf_tbl) {
  osf_tbl_files <- osf_tbl |> osfr::osf_ls_files()
  osf_tbl_manifest <- osf_tbl_files[
    osf_tbl_files[["name"]] == "manifest.csv",
  ]
  if (nrow(osf_tbl_manifest) == 0L) {
    return(data.frame(
      label = character(0),
      fn = character(0),
      version = character(0),
      hash = character(0)
    ))
  }
  path_save <- file.path(tempdir(), "manifest.csv")
  osfr::osf_download(
    osf_tbl_manifest,
    path = tempdir(), conflicts = "overwrite"
  )
  utils::read.csv(path_save)
}

# so, we need functions to restore downloads,
# we also need functions to download
# outputs.
# should both be called restored?
.projr_restore_osf <- function(label = NULL) {
  if (is.null(label)) {
    label <- projr_yml_get_unchecked()[["directories"]]
  }
  label <- label[grepl("^cache|^dataraw", .projr_dir_label_strip(label))]
  for (i in seq_along(label)) {
    .projr_restore_osf_label(label[[i]])
  }
}

.projr_restore_osf_label <- function(label) {
  yml_projr_dir_lbl <-
    projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_projr_dir_lbl)) {
    return(invisible(FALSE))
  }
  title <- names(yml_projr_dir_lbl[["osf"]])
  yml_projr_osf <- yml_projr_dir_lbl[["osf"]][[1]]
  osf_tbl <- .projr_osf_get_node(
    title = title,
    yml_param = yml_projr_osf,
    parent_id = NULL
  )
  # set up subdirectories
  append_label <- (
    !"path_append_label" %in% names(yml_projr_osf)
  ) ||
    yml_projr_osf[["path_append_label"]]
  path_pre <- !is.null(yml_projr_osf[["path"]])
  if (append_label && path_pre) {
    path_dir_sub <- file.path(path_pre, label)
  } else if (append_label) {
    path_dir_sub <- label
  } else {
    path_dir_sub <- path_pre
  }
  if (append_label || path_pre) {
    osf_tbl <- osfr::osf_mkdir(
      x = osf_tbl, path = path_dir_sub
    )
  }
  # can try the fancy method stuff here
  # all about that `download_sync_approach`,
  # `download_version_source` and `download_conflict`.
  yml_projr_osf_dnld <- yml_projr_osf[["download"]]
  if (!is.null(yml_projr_osf_dnld)) {
    if (!is.null(yml_projr_osf_dnld[["conflict"]])) {
      conflict <- yml_projr_osf_dnld[["conflict"]]
    } else {
      conflict <- "overwrite"
    }
    if (!is.null(yml_projr_osf_dnld[["sync_approach"]])) {
      sync_approach <- yml_projr_osf_dnld[["sync_approach"]]
    } else {
      sync_approach <- "download_all"
    }
  } else {
    conflict <- "overwrite"
    sync_approach <- "download_all"
  }
  if (sync_approach == "delete_then_download_all") {
    unlink(yml_projr_dir_lbl[["path"]], recursive = TRUE)
  }
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  path_dir_save <- yml_projr_dir_lbl[["path"]]
  if (!dir.exists(path_dir_save)) {
    dir.create(path_dir_save, recursive = TRUE)
  }
  if (append_label || path_pre) {
    for (i in seq_len(nrow(osf_tbl_file))) {
      osfr::osf_download(
        x = osf_tbl_file[i, ],
        path = path_dir_save,
        recurse = TRUE,
        conflicts = conflict
      )
    }
  } else {
    osfr::osf_download(
      x = osf_tbl_file,
      path = path_dir_save,
      recurse = TRUE,
      conflicts = conflict
    )
  }
  osf_tbl
}

.projr_osf_download_dir <- function() {
  if (!.projr_osf_download_dir_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_dir <- projr_yml_get()[["directories"]]
  for (i in seq_along(yml_projr_dir)) {
    .projr_osf_download_dir_label(
      label = names(yml_projr_dir)[i],
    )
  }
}

.projr_osf_download_build <- function(output_safe) {
  if (!.projr_osf_download_build_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_build_osf <- projr_yml_get()[["build"]][["osf"]]
  for (i in seq_along(yml_projr_build_osf)) {
    .projr_osf_download_build_node(
      title = names(yml_projr_build_osf)[i],
      parent_id = NULL,
      output_safe = output_safe
    )
  }
}

.projr_osf_download_dir_check_run <- function() {
  yml_projr <- projr_yml_get()
  osf_is_source <- vapply(
    yml_projr[["directories"]], function(x) "osf" %in% names(x), logical(1)
  ) |>
    any()
  if (!osf_is_source) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_download_build_check_run <- function() {
  yml_projr <- projr_yml_get()
  osf_is_dest <- "osf" %in% names(yml_projr[["build"]])
  if (!osf_is_dest) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_download_dir_label <- function(label) {
  yml_param <- projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_param)) {
    return(invisible(FALSE))
  }
  yml_param_osf <- yml_param[["osf"]]
  # allowed to just specify id
  if (is.character(yml_param_osf)) {
    osf_tbl <- .projr_osf_get_node_id(yml_param_osf)
  } else {
    osf_tbl <- .projr_osf_get_node_id(yml_param_osf[["id"]])
  }
  if (is.null(osf_tbl)) {
    stop(paste0("osf node id not specified for label ", label))
  }
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  if (is.character(yml_param_osf)) {
    # use the label if it's a directory automatically
    if ("label" %in% osf_tbl_file[["name"]]) {
      osf_tbl_file_dir <- .projr_osf_filter_dir(osf_tbl_file)
      osf_tbl_file_dir_label <- osf_tbl_file_dir[
        osf_tbl_file_dir[["name"]] == label,
      ]
      if (nrow(osf_tbl_file_dir_label) == 1L) {
        osf_tbl_file <- osf_tbl_file_dir_label
      }
    }
    # specify conflict resolution
    conflicts <- "overwrite"
  } else {
    # use sub-directory if specified
    if ("sub-dir" %in% names(yml_param_osf)) {
      dir_vec <- fs::path_split(yml_param_osf[["sub-dir"]])
      osf_tbl_file_dir <- .projr_osf_filter_dir(osf_tbl_file)
      for (x in dir_vec) {
        osf_tbl_file_dir_sub <- osf_tbl_file_dir[
          osf_tbl_file_dir[["name"]] == x,
        ]
        osf_tbl_file_dir <- osf_tbl_file_dir_sub |>
          osfr::osf_ls_files() |>
          .projr_osf_filter_dir()
      }
      if (nrow(osf_tbl_file_dir) > 0L) {
        osf_tbl_file <- osf_tbl_file_dir
      } else {
        stop(paste0(
          "sub-dir ", yml_param_osf[["sub-dir"]],
          " not found for label ", label
        ))
      }
    }
    # specify conflict resolution
    if (is.null(yml_param_osf[["conflicts"]])) {
      conflicts <- "overwrite"
    } else {
      conflicts <- yml_param_osf[["conflicts"]]
    }
  }
  .projr_osf_download_osf_tbl(
    osf_tbl = osf_tbl_file,
    dir_save = projr_dir_get(label, output_safe = TRUE),
    is_file = TRUE,
    conflicts = "overwrite"
  )
  invisible(TRUE)
}

.projr_osf_download_build_node <- function(title,
                                           parent_id = NULL,
                                           output_safe) {
  yml_param <- projr_yml_get_unchecked()[["build"]][["osf"]][[title]]
  parent_id <- .projr_osf_get_parent_id(
    yml_param = yml_param, parent_id = parent_id
  )
  id <- yml_param[["id"]]
  if (!is.null(id)) {
    osf_tbl <- .projr_osf_get_node_id(id)
  } else {
    osf_tbl <- .projr_osf_get_node_id_parent(title, parent_id)
  }
  if (is.null(osf_tbl)) {
    stop(paste0("osf node id not found for title ", title))
  }
  if (is.null(yml_param[["conflicts"]])) {
    conflicts <- "overwrite"
  } else {
    conflicts <- yml_param[["conflicts"]]
  }
  for (i in seq_along(yml_param[["content"]])) {
    .projr_osf_download_osf_tbl(
      osf_tbl = osf_tbl,
      dir_save = projr_dir_get(yml_param[["content"]][i], output_safe = TRUE),
      file = FALSE,
      conflicts = conflicts
    )
  }
}

.projr_osf_download_osf_tbl <- function(osf_tbl,
                                        dir_save,
                                        is_file,
                                        conflicts = "overwrite") {
  if (!is_file) {
    osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
  }
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
      x = osf_tbl_file[i, ],
      path = dir_save,
      recurse = TRUE,
      conflicts = conflicts
    )
  }
}
