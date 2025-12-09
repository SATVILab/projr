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
  # Combine all manifests at once for better performance
  manifest_parts <- list(
    .build_manifest_pre_read(),
    .build_manifest_post_get_manifest(output_run),
    .build_manifest_post_get_manifest_previous_version()
  )
  # Filter out empty manifests
  manifest_parts <- Filter(function(x) nrow(x) > 0, manifest_parts)
  if (length(manifest_parts) == 0) {
    combined_manifest <- .zero_tbl_get_manifest()
  } else if (length(manifest_parts) == 1) {
    combined_manifest <- manifest_parts[[1]]
  } else {
    combined_manifest <- do.call(rbind, manifest_parts)
  }
  .manifest_write(combined_manifest, .build_manifest_post_get_path(output_run))
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
  # Filter out empty manifests and use do.call for better performance
  manifest_list <- Filter(function(x) nrow(x) > 0, manifest_list)
  if (length(manifest_list) == 0) {
    return(.zero_tbl_get_manifest())
  } else if (length(manifest_list) == 1) {
    return(manifest_list[[1]])
  }
  do.call(rbind, manifest_list)
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
