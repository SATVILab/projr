# hashing
# ---------------------------

.manifest_hash_label <- function(label,
                                       output_run) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .hash_dir(
    path_dir =.path_get_dir(label, safe = !output_run),
    dir_exc = .build_label_get_dir_exc(label)
  ) |>
    .manifest_hash_cache_filter(label)
  if (nrow(hash_tbl) == 0L) {
    .empty_tbl_get_manifest(label, projr:.version_get())
  } else {
    cbind(
      data.frame(label = rep(label, nrow(hash_tbl))),
      hash_tbl
    )
  }
}

.manifest_hash_cache_filter <- function(hash_tbl, # nolint
                                              label) {
  if (!.yml_dir_label_class_detect_cache(label)) {
    return(hash_tbl)
  }
  hash_tbl[!grepl("^projr/v\\d+", hash_tbl[["fn"]]), , drop = FALSE]
}

.build_label_get_dir_exc <- function(label) {
  switch(.yml_dir_label_class_get(label),
    "cache" = "projr"
  )
}

# misc operations
# ---------------------------

.manifest_filter_label <- function(manifest, label) {
  manifest[manifest[["label"]] == label, ] %@@%
    .zero_tbl_get_manifest()
}

.manifest_filter_version <- function(manifest, version) {
  manifest[manifest[["version"]] == .version_v_add(version), ] %@@%
    .zero_tbl_get_manifest()
}

.manifest_filter_out_version_label <- function(manifest, # nolint
                                                     version,
                                                     label) {
  label_vec_non <- manifest[["label"]] != label
  version_vec_non <- manifest[["version"]] != .version_v_add(version)
  manifest[label_vec_non & version_vec_non, ] %@@%
    .zero_tbl_get_manifest()

}

# get what would be added,
# based on the project
# ---------------------------

.manifest_get_add_project <- function(manifest, label) { # nolint
  manifest_project <- .manifest_read_project()
  manifest_add <- manifest_project |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(projr:.version_get())
  if (nrow(manifest_add) == 0L) {
    return(.empty_tbl_get_manifest(label, projr:.version_get()))
  }
  manifest_add
}

# writing, reading and merging
# ---------------------------

.manifest_write <- function(manifest, path, overwrite = TRUE) {
  rownames(manifest) <- NULL
  manifest |>
    .manifest_remove_duplicate() |>
    .manifest_write_actual(path, overwrite)
}

.manifest_append_previous <- function(manifest, append, path_previous) { # nolint
  if (!append) {
    return(manifest)
  }
  if (is.null(path_previous)) {
    path_previous <- .manifest_get_path_file("NULL")
  }
  if (!file.exists(path_previous)) {
    return(manifest)
  }
  manifest_pre <- .manifest_read(path_previous)
  manifest |> .manifest_append_previous_actual(manifest_pre)
}

.manifest_append_previous_actual <- function(manifest, manifest_pre) { # nolint
  rownames(manifest_pre) <- NULL
  manifest <- manifest_pre |> rbind(manifest)
  rownames(manifest) <- NULL
  manifest
}

.manifest_remove_duplicate <- function(manifest) { # nolint: object_length_linter, line_length_linter.
  manifest[["string"]] <- Reduce(paste0, manifest)
  manifest <- manifest[!duplicated(manifest[["string"]]), ]
  manifest[["string"]] <- NULL
  manifest
}

.manifest_write_actual <- function(manifest, path, overwrite) {
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

# .manifest_read <- function(path = NULL) {
.manifest_read <- function(path = NULL) {
  if (!.is_string(path) || !file.exists(path)) {
    return(.zero_tbl_get_manifest())
  }
  out_tbl <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.manifest_read_project <- function() {
  .manifest_read(.path_get("manifest.csv"))
}

.manifest_get_path_dir <- function(path_dir) {
  if (is.null(path_dir)) {
    path_dir <- .path_get()
  }
  .dir_create(path_dir)
}

.manifest_get_path_file <- function(path_dir) {
  path_dir |>
    .manifest_get_path_dir() |>
    file.path("manifest.csv")
}

.manifest_version_get_latest <- function(manifest) { # nolint: object_length_linter, line_length_linter.
  manifest[["version"]] |> .version_get_earliest()
}

.empty_tbl_get_manifest <- function(label, version) {
  out_df <- data.frame(
    label = label,
    fn = character(1),
    version = version |> .version_v_add(),
    hash = character(1)
  )
  rownames(out_df) <- NULL
  out_df
}

.zero_tbl_get_manifest <- function() {
  out_df <- data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}

.version_file_update_project_version <- function(version_file) { # nolint: object_length_linter, line_length_linter.
  if (.is_len_0(version_file)) {
    return(paste0("Project: ",.version_get()))
  }
  if (any(grepl("^Project: ", version_file))) {
    version_file <- version_file[!grepl("^Project: ", version_file)]
  }
  c(paste0("Project: ",.version_get()), version_file)
}

.version_file_update_label_version <- function(version_file, # nolint
                                                     label,
                                                     add_asterisk) {
  version_add <- if (add_asterisk) {
   .version_get() |> paste0("*")
  } else {
   .version_get()
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

.version_file_check_update_label <- function(fn, # nolint: object_length_linter, line_length_linter.
                                                   version_file,
                                                   label) {
  is_change <- .is_change(fn) # nolint
  is_label_present <- .version_file_check_update_label_present(
    version_file, label
  )
  is_change || !is_label_present
}

.version_file_check_update_label_present <- function(version_file, # nolint
                                                           label) {
  !.is_len_0(which(grepl(paste0("^", label, ": "), version_file)))
}

# get minimum acceptable version
.manifest_get_version_earliest_match <- function(label, # nolint
                                                       version_comp) {
  # begin with latest version (most conservative)
  version_earliest_match <-.version_get() |>
    .version_v_rm() |>
    package_version()
  # get lowest version available, if version_comp not provided
  version_comp <-
    .manifest_get_version_earliest_match_get_version_comp(version_comp)
  manifest_project <- .remote_get_manifest_project() |>
    .manifest_filter_label(label)
  rownames(manifest_project) <- NULL
  manifest_latest <- manifest_project |>
    .manifest_filter_version.version_get())
  manifest_latest <- manifest_latest[, c("label", "fn", "hash")]
  version_vec <- manifest_project[["version"]] |>
    .version_v_rm() |>
    package_version() |>
    sort()
  version_vec <- version_vec[version_vec >= package_version(version_comp)]
  if (.is_len_0(version_vec)) {
    return(version_earliest_match)
  }
  version_vec <- version_vec |> rev()
  for (i in seq_along(version_vec)) {
    version_curr <- version_vec[[i]]
    manifest_curr <- manifest_project |>
      .manifest_filter_version(version_curr)
    manifest_curr <- manifest_curr[, c("label", "fn", "hash")]
    change_list <- .change_get_hash(manifest_curr, manifest_latest)
    change_vec <- change_list[-which(names(change_list) == "fn_same")] |>
      unlist()
    is_change <- .is_len_pos(change_vec)
    if (is_change) {
      return(version_earliest_match)
    }
    version_earliest_match <- version_curr
  }
  version_earliest_match
}

.manifest_get_version_earliest_match_get_version_comp <- # nolint
  function(version_comp = NULL) {
    if (!is.null(version_comp)) {
      return(version_comp)
    }
    version_format <- .yml_metadata_get_version_format(NULL)
    # remove dev
    version_format <- gsub(".dev$", "", version_format)
    # swop all but last component for 0, and last for 1,
    # to get earliest possible valid version
    version_format <- gsub("major|minor|patch", "0", version_format)
    gsub("0$", "1", version_format)
  }