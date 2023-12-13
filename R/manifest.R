.projr_hash_label <- function(label,
                              output_run = NULL) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .projr_hash_dir(
    path_dir = projr_dir_get(label, safe = !output_run)
  )
  cbind(
    data.frame(label = rep(label, nrow(hash_tbl))),
    hash_tbl
  )
}

.projr_hash_dir <- function(path_dir, version = NULL) {
  .projr_check_given(path_dir, "path_dir")
  fn_vec <- .projr_dir_ls(path_dir, full.names = TRUE)
  if (.projr_state_len_z(fn_vec)) {
    return(.projr_zero_tbl_get_hash())
  }
  out_tbl <- data.frame(
    fn = .projr_file_get_rel(fn_vec, path_dir),
    version = paste0("v", version %||% projr_version_get()),
    hash = .projr_hash_file(fn_vec)
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_hash_file <- function(fn) {
  vapply(fn, .projr_hash_file_single, character(1))
}

.projr_hash_file_single <- function(fn) {
  digest::digest(fn, serialize = FALSE, file = TRUE)
}


.projr_manifest_write <- function(manifest, output_run, append = TRUE) {
  path_dir_write <- .projr_manifest_write_get_path(output_run)
  rownames(manifest) <- NULL
  .projr_manifest_write_append_previous(manifest) |>
    .projr_manifest_write_append_previous(append) |>
    .projr_manifest_remove_duplicate() |>
    .projr_manifest_write_actual(path_dir_write)
}

.projr_manifest_write_get_path <- function(output_run) {
  if (output_run) {
    return(.projr_dir_proj_get())
  }
  .projr_dir_get_cache_auto_version() |>
    .projr_dir_create()
}

.projr_manifest_write_append_previous <- function(manifest, append) {
  if (!append) {
    return(manifest)
  }
  path_manifest <- .projr_dir_proj_get("manifest.csv")
  if (!file.exists(path_manifest)) {
    return(manifest)
  }
  manifest_orig <- .projr_manifest_read(NULL)
  rownames(manifest_orig) <- NULL
  manifest <- manifest_orig |> rbind(manifest)
  rownames(manifest) <- NULL
  manifest
}

.projr_manifest_remove_duplicate <- function(manifest) {
  manifest[["string"]] <- Reduce(paste0, manifest)
  manifest <- manifest[!duplicated(manifest[["string"]]), ]
  manifest[["string"]] <- NULL
  manifest
}

.projr_manifest_write_actual <- function(manifest, path_dir) {
  utils::write.csv(
    manifest, file.path(path_dir, "manifest.csv"),
    row.names = FALSE
  )
}

.projr_manifest_read <- function(path_dir = NULL) {
  if (.projr_state_null(path_dir)) {
    path_dir <- .projr_dir_proj_get()
  }
  out_tbl <- utils::read.csv(
    file.path(path_dir, "manifest.csv"),
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}


.projr_manifest_version_get_latest <- function(manifest) {
  manifest[["version"]] |>
    unique() |>
    sort() |>
    utils::tail(1)
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

.projr_zero_tbl_get_hash <- function() {
  out_df <- data.frame(
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}
