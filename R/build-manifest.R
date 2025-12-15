# pre
# --------------------------------
.build_manifest_pre <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  .build_manifest_pre_get_manifest() |>
    .manifest_write(.build_manifest_pre_path_get())
}

.build_manifest_pre_path_get <- function() {
  .dir_get_cache_auto_version("manifest.csv", profile = NULL)
}

.build_manifest_pre_get_manifest <- function() {
  label_vec <- .build_manifest_pre_get_label()
  if (.is_len_0(label_vec)) {
    return(.zero_tbl_get_manifest())
  }
  out_tbl <- lapply(
    label_vec, .manifest_hash_label,
    output_run = FALSE
  ) |>
    .build_manifest_reduce()
  rownames(out_tbl) <- NULL
  out_tbl
}


.build_manifest_pre_get_label <- function() {
  # decide whether to hash or not - default is to hash,
  # except for cache directories
  label_vec <- c(
    .yml_dir_get_label_cache(NULL),
    .yml_dir_get_label_raw(NULL)
  )
  label_vec[
    vapply(
      label_vec, .yml_dir_get_hash_complete, logical(1),
      profile = NULL
    )
  ]
}

.build_manifest_pre_get_label_ind_check <- function(label) {
  if (grepl("^raw", label)) {
    return(TRUE)
  }
}

# post
# --------------------------------
.build_manifest_post <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }

  # Get current and previous manifests
  manifest_pre <- .build_manifest_pre_read()
  manifest_current <- .build_manifest_post_get_manifest(output_run)
  manifest_previous <- .build_manifest_post_get_manifest_previous_version()

  # Detect and mark removed files
  # Check both pre-build and post-build labels for removals
  all_current_labels <- c(.build_manifest_pre_get_label(), .build_manifest_post_get_label())
  manifest_removals <- .build_manifest_detect_removals(
    rbind(manifest_pre, manifest_current),  # Combine pre and post manifests
    manifest_previous,
    all_current_labels
  )

  # Combine all manifests
  manifest_pre |>
    rbind(manifest_current) |>
    rbind(manifest_removals) |>
    rbind(manifest_previous) |>
    .manifest_write(.build_manifest_post_get_path(output_run))
}

.build_manifest_detect_removals <- function(manifest_current,
                                           manifest_previous,
                                           current_labels) {
  # Detect files that existed in previous version but are missing now
  # Add entries with empty hash to mark removal

  if (nrow(manifest_previous) == 0) {
    return(.zero_tbl_get_manifest())
  }

  if (length(current_labels) == 0) {
    return(.zero_tbl_get_manifest())
  }

  # Get current version
  current_version <- projr::projr_version_get() |> .version_v_add()

  # Filter previous manifest to relevant labels and get latest state
  manifest_previous_filtered <- manifest_previous[
    manifest_previous[["label"]] %in% current_labels,
    , drop = FALSE
  ]

  if (nrow(manifest_previous_filtered) == 0) {
    return(.zero_tbl_get_manifest())
  }

  # Handle case where manifest_current has 0 rows (all files removed)
  if (nrow(manifest_current) == 0) {
    current_keys <- character(0)
  } else {
    current_keys <- paste(manifest_current[["label"]],
                         manifest_current[["fn"]],
                         sep = ":::")
  }

  # For each label+fn in previous, check if it exists in current
  previous_keys <- paste(manifest_previous_filtered[["label"]],
                        manifest_previous_filtered[["fn"]],
                        sep = ":::")

  # Find files that were in previous but not in current (removed)
  removed_keys <- setdiff(previous_keys, current_keys)

  if (length(removed_keys) == 0) {
    return(.zero_tbl_get_manifest())
  }

  # Create removal entries (empty hash to indicate removal)
  removal_indices <- which(previous_keys %in% removed_keys)

  # Get unique removed files (one entry per file)
  removed_files <- manifest_previous_filtered[removal_indices, , drop = FALSE]
  removed_files[["file_key"]] <- paste(removed_files[["label"]],
                                       removed_files[["fn"]],
                                       sep = ":::")

  # Keep only one entry per file (deduplicate by file_key)
  removed_files <- removed_files[!duplicated(removed_files[["file_key"]]), , drop = FALSE]
  removed_files[["file_key"]] <- NULL

  # Mark as removed by setting version to current and hash to empty
  # This creates a "tombstone" entry
  removed_files[["version"]] <- current_version
  removed_files[["hash"]] <- ""

  rownames(removed_files) <- NULL
  removed_files
}

.build_manifest_pre_read <- function() {
  path_manifest <- .build_manifest_pre_path_get()
  if (file.exists(path_manifest)) {
    return(.manifest_read(path_manifest))
  }
  .zero_tbl_get_manifest()
}

.build_manifest_post_get_manifest <- function(output_run) {
  label_vec <- .build_manifest_post_get_label()
  if (length(label_vec) == 0) {
    return(.zero_tbl_get_manifest())
  }
  out_tbl <- lapply(
    label_vec, .manifest_hash_label,
    output_run = output_run
  ) |>
    .build_manifest_reduce()
  rownames(out_tbl) <- NULL
  out_tbl
}

.build_manifest_reduce <- function(manifest_list) {
  if (.is_len(manifest_list, 1L)) {
    return(manifest_list[[1]])
  }
  Reduce(rbind, manifest_list)
}

.build_manifest_post_get_label <- function() {
  # decide whether to hash or not - default is to hash,
  # except for cache directories
  label_vec <- c(
    .yml_dir_get_label_docs(NULL),
    .yml_dir_get_label_output(NULL)
  )
  label_vec[
    vapply(
      label_vec, .yml_dir_get_hash_complete, logical(1),
      profile = NULL
    )
  ]
}

.build_manifest_post_get_path <- function(output_run) {
  if (output_run) {
    return(.path_get("manifest.csv"))
  }
  .dir_get_cache_auto_version("manifest.csv", profile = NULL)
}

.build_manifest_post_get_manifest_previous_version <- function() {
  path <- .path_get("manifest.csv")
  if (!file.exists(path)) {
    return(.zero_tbl_get_manifest())
  }
  .manifest_read(.path_get("manifest.csv"))
}
