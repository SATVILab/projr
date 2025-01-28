# hashing
# ---------------------------

.projr_manifest_hash_label <- function(label,
                                       output_run) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .projr_hash_dir(
    path_dir = projr_path_get_dir(label, safe = !output_run),
    dir_exc = .projr_build_label_get_dir_exc(label)
  ) |>
    .projr_manifest_hash_cache_filter(label)
  cbind(
    data.frame(label = rep(label, nrow(hash_tbl))),
    hash_tbl
  )
}

.projr_manifest_hash_cache_filter <- function(hash_tbl, label) {
  if (!.projr_yml_dir_label_class_detect_cache(label)) {
    return(hash_tbl)
  }
  hash_tbl[!grepl("^projr/v\\d+", hash_tbl[["fn"]]), , drop = FALSE]
}

.projr_build_label_get_dir_exc <- function(label) {
  switch(.projr_yml_dir_label_class_get(label),
    "cache" = "projr"
  )
}

# misc operations
# ---------------------------

.projr_manifest_filter_label <- function(manifest, label) {
  manifest[manifest[["label"]] == label, ] %@@%
    .projr_zero_tbl_get_manifest()
}

.projr_manifest_filter_version <- function(manifest, version) {
  manifest[manifest[["version"]] == .projr_version_v_add(version), ] %@@%
    .projr_zero_tbl_get_manifest()
}

.projr_manifest_filter_out_version_label <- function(manifest,
                                                     version,
                                                     label) {
  label_vec_non <- manifest[["label"]] != label
  version_vec_non <- manifest[["version"]] != .projr_version_v_add(version)
  manifest[label_vec_non & version_vec_non, ] %@@%
    .projr_zero_tbl_get_manifest()

}

# get what would be added,
# based on the project
# ---------------------------

.projr_manifest_get_add_project <- function(label) {
  manifest_project <- .projr_manifest_read_project()
  manifest_project |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(projr::projr_version_get())
}

# writing, reading and merging
# ---------------------------

.projr_manifest_write <- function(manifest, path, overwrite = TRUE) {
  rownames(manifest) <- NULL
  manifest |>
    .projr_manifest_remove_duplicate() |>
    .projr_manifest_write_actual(path, overwrite)
}

.projr_manifest_append_previous <- function(manifest, append, path_previous) {
  if (!append) {
    return(manifest)
  }
  if (is.null(path_previous)) {
    path_previous <- .projr_manifest_get_path_file("NULL")
  }
  if (!file.exists(path_previous)) {
    return(manifest)
  }
  manifest_pre <- .projr_manifest_read(path_previous)
  manifest |> .projr_manifest_append_previous_actual(manifest_pre)
}

.projr_manifest_append_previous_actual <- function(manifest, manifest_pre) {
  rownames(manifest_pre) <- NULL
  manifest <- manifest_pre |> rbind(manifest)
  rownames(manifest) <- NULL
  manifest
}

.projr_manifest_remove_duplicate <- function(manifest) {
  manifest[["string"]] <- Reduce(paste0, manifest)
  manifest <- manifest[!duplicated(manifest[["string"]]), ]
  manifest[["string"]] <- NULL
  manifest
}

.projr_manifest_write_actual <- function(manifest, path, overwrite) {
  rownames(manifest) <- NULL
  if (file.exists(path)) {
    if (!overwrite) {
      stop("file '", path, "' already exists", call. = FALSE)
    }
    invisible(file.remove(path))
  }
  .dir_create(dirname(path))
  utils::write.csv(
    manifest, path,
    row.names = FALSE
  )
  invisible(path)
}

.projr_manifest_read <- function(path = NULL) {
  if (is.null(path) || !file.exists(path)) {
    return(.projr_zero_tbl_get_manifest())
  }
  out_tbl <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_manifest_read_project <- function() {
  .projr_manifest_read(.dir_proj_get("manifest.csv"))
}

.projr_manifest_get_path_dir <- function(path_dir) {
  if (is.null(path_dir)) {
    path_dir <- .dir_proj_get()
  }
  .dir_create(path_dir)
}

.projr_manifest_get_path_file <- function(path_dir) {
  path_dir |>
    .projr_manifest_get_path_dir() |>
    file.path("manifest.csv")
}

.projr_manifest_version_get_latest <- function(manifest) {
  manifest[["version"]] |> .projr_version_get_earliest()
}

.projr_zero_tbl_get_manifest <- function() {
  out_df <- data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}

.projr_version_file_update_project_version <- function(version_file) {
  if (.is_len_0(version_file)) {
    return(paste0("Project: ", projr_version_get()))
  }
  if (any(grepl("^Project: ", version_file))) {
    version_file <- version_file[!grepl("^Project: ", version_file)]
  } 
  c(paste0("Project: ", projr_version_get()), version_file)
}

.projr_version_file_update_label_version <- function(version_file, label, is_upload) {
  version_add <- if (is_upload) {
    projr_version_get() |> paste0("*")
  } else {
    projr_version_get()
  }
  if (.is_len_0(version_file)) {
    return(paste0(label, ": ", version_add))
  }
  label_ind <- which(grepl(paste0("^", label, ": "), version_file))
  line_add <- paste0(label, ": ", version_add)
  if (.is_len_0(label_ind)) {
    c(version_file, line_add)
  } else {
    version_file[label_ind[[1]]] <- line_add
    if (length(label_ind) > 1) {
      version_file[-label_ind[-1]]
    } else {
      version_file
    }
  }
}

.projr_version_file_check_update_label <- function(fn,
                                                   version_file,
                                                   label) {
  is_change <- .is_change(fn)
  is_label_present <- .projr_version_file_check_update_label_present(
    version_file, label
  )
  is_change || !is_label_present
}

.projr_version_file_check_update_label_present <- function(version_file,
                                                            label) {
  !.is_len_0(which(grepl(paste0("^", label, ": "), version_file)))
}
