# hashing
# ---------------------------

.projr_manifest_hash_label <- function(label,
                                       output_run) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .projr_hash_dir(
    path_dir = projr_path_get_dir(label, safe = !output_run),
    dir_exc = .projr_build_label_get_dir_exc(label)
  )
  cbind(
    data.frame(label = rep(label, nrow(hash_tbl))),
    hash_tbl
  )
}

.projr_build_label_get_dir_exc <- function(label) {
  switch(.projr_yml_dir_label_class_get(label),
    "cache" = "projr"
  )
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
  .projr_dir_create(dirname(path))
  utils::write.csv(
    manifest, path,
    row.names = FALSE
  )
  invisible(path)
}

.projr_manifest_read <- function(path = NULL) {
  out_tbl <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_manifest_get_path_dir <- function(path_dir) {
  if (.projr_state_null(path_dir)) {
    path_dir <- .projr_dir_proj_get()
  }
  .projr_dir_create(path_dir)
}

.projr_manifest_get_path_file <- function(path_dir) {
  path_dir |>
    .projr_manifest_get_path_dir() |>
    .projr_file_get_full_dots("manifest.csv")
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
