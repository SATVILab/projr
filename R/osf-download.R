.projr_osf_download_manifest <- function(osf_tbl) {
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
    path = file.path(tempdir(), "manifest.csv"),
    conflicts = "overwrite"
  )
  utils::read.csv(path_save)
}

.projr_osf_download <- function() {
  if (!.projr_osf_upload_check_run()) {
    return(invisible(FALSE))
  }
  yml_projr_dir <- projr_yml_get()[["directories"]]
  for (i in seq_along(yml_projr_dir)) {
    .projr_osf_download_label(
      label = names(yml_projr_dir)[i],
    )
  }
}

.projr_osf_download_check_run <- function() {
  yml_projr <- projr_yml_get()
  # either a dev run or else no osf upload specified
  osf_is_source <- vapply(
    yml_projr[["directories"]], function(x) "osf" %in% names(x), logical(1)
  ) |>
    any()
  if (!osf_is_source) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.projr_osf_download_label <- function(label) {
  yml_param <- projr_yml_get_unchecked()[["directories"]][[label]]
  if (!"osf" %in% names(yml_param)) {
    return(invisible(FALSE))
  }
  yml_param_osf <- yml_param[["osf"]]
  osf_tbl <- .projr_osf_get_node_id(yml_param_osf)
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
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
      x = osf_tbl_file[i, ],
      path = projr_dir_get(label, output_safe = TRUE),
      recurse = TRUE,
      conflicts = conflicts
    )
  }
  invisible(TRUE)
}

.projr_osf_download_osf_tbl <- function(osf_tbl,
                                        dir_save,
                                        conflicts = "overwrite") {
  osf_tbl <- .projr_osf_get_node_id(yml_param_osf)
  osf_tbl_file <- osf_tbl |> osfr::osf_ls_files()
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
