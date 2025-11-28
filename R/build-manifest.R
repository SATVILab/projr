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

  # Build current version's manifest (pre + post)
  manifest_current <- .build_manifest_pre_read() |>
    rbind(.build_manifest_post_get_manifest(output_run))

  # Get current version
  version_current <- projr_version_get()

  # Write split manifest for current version
  # This merges with existing data if the file exists (for re-builds)
  .manifest_split_write_version(manifest_current, version_current)

  # For backward compatibility, also write consolidated manifest
  # Read all previous data and add current, then deduplicate
  manifest_all <- .build_manifest_post_get_manifest_previous_version() |>
    rbind(manifest_current) |>
    .manifest_remove_duplicate()

  # Write consolidated manifest (overwrite = TRUE, no dedup since we just did it)
  .manifest_write_impl(manifest_all, .build_manifest_post_get_path(output_run), overwrite = TRUE)
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
  # Simply read the consolidated manifest.csv
  # Don't exclude current version - let deduplication handle it
  path <- .path_get("manifest.csv")
  if (!file.exists(path)) {
    return(.zero_tbl_get_manifest())
  }
  .manifest_read(path)
}
