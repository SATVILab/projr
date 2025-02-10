# pre
# --------------------------------
.projr_build_manifest_pre <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  .projr_build_manifest_pre_get_manifest() |>
    .projr_manifest_write(.projr_build_manifest_pre_path_get())
}

.projr_build_manifest_pre_path_get <- function() {
  .projr_dir_get_cache_auto_version("manifest.csv", profile = NULL)
}

.projr_build_manifest_pre_get_manifest <- function() {
  label_vec <- .projr_build_manifest_pre_get_label()
  if (.is_len_0(label_vec)) {
    return(.projr_zero_tbl_get_manifest())
  }
  out_tbl <- lapply(
    label_vec, .projr_manifest_hash_label,
    output_run = FALSE
  ) |>
    .projr_build_manifest_reduce()
  rownames(out_tbl) <- NULL
  out_tbl
}


.projr_build_manifest_pre_get_label <- function() {
  # decide whether to hash or not - default is to hash,
  # except for cache directories
  label_vec <- c(
    .projr_yml_dir_get_label_cache(NULL),
    .projr_yml_dir_get_label_raw(NULL)
  )
  label_vec[
    vapply(
      label_vec, .projr_yml_dir_get_hash_complete, logical(1),
      profile = NULL
    )
  ]
}

.projr_build_manifest_pre_get_label_ind_check <- function(label) {
  if (grepl("^raw", label)) {
    return(TRUE)
  }
}

# post
# --------------------------------
.projr_build_manifest_post <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  .projr_build_manifest_pre_read() |>
    rbind(.projr_build_manifest_post_get_manifest(output_run)) |>
    rbind(.projr_build_manifest_post_get_manifest_previous_version()) |>
    .projr_manifest_write(.projr_build_manifest_post_get_path(output_run))
}

.projr_build_manifest_pre_read <- function() {
  path_manifest <- .projr_build_manifest_pre_path_get()
  if (file.exists(path_manifest)) {
    return(.projr_manifest_read(path_manifest))
  }
  .projr_zero_tbl_get_manifest()
}

.projr_build_manifest_post_get_manifest <- function(output_run) {
  label_vec <- .projr_build_manifest_post_get_label()
  if (length(label_vec) == 0) {
    return(.projr_zero_tbl_get_manifest())
  }
  out_tbl <- lapply(
    label_vec, .projr_manifest_hash_label,
    output_run = output_run
  ) |>
    .projr_build_manifest_reduce()
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_build_manifest_reduce <- function(manifest_list) {
  if (.is_len(manifest_list, 1L)) {
    return(manifest_list[[1]])
  }
  Reduce(rbind, manifest_list)
}

.projr_build_manifest_post_get_label <- function() {
  # decide whether to hash or not - default is to hash,
  # except for cache directories
  label_vec <- c(
    .projr_yml_dir_get_label_docs(NULL),
    .projr_yml_dir_get_label_output(NULL)
  )
  label_vec[
    vapply(
      label_vec, .projr_yml_dir_get_hash_complete, logical(1),
      profile = NULL
    )
  ]
}

.projr_build_manifest_post_get_path <- function(output_run) {
  if (output_run) {
    return(.path_get("manifest.csv"))
  }
  .projr_dir_get_cache_auto_version("manifest.csv", profile = NULL)
}

.projr_build_manifest_post_get_manifest_previous_version <- function() {
  path <- .path_get("manifest.csv")
  if (!file.exists(path)) {
    return(.projr_zero_tbl_get_manifest())
  }
  .projr_manifest_read(.path_get("manifest.csv"))
}
