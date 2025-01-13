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

# get manifests from various places
# ---------------------------

.projr_manifest_get <- function(type,
                                remote) {
  switch(type,
    "project" = .projr_manifest_get_project(),
    .projr_manifest_get_non_project(type, remote)
  )
}

.projr_manifest_get_project <- function() {
  # just the actual project
  .projr_manifest_read(.dir_proj_get("manifest.csv"))
}

.projr_manifest_get_non_project <- function(type,
                                            remote) {
  manifest_actual <- .projr_manifest_get_non_project_raw(type, remote)
  if (is.null(manifest_actual)) {
    .projr_manifest_get_project()
  } else {
    manifest_actual
  }
}

.projr_manifest_get_non_project_raw <- function(type, remote) {
  path_dir_save <- .dir_create_tmp_random()
  .projr_remote_file_get_all(type, remote, path_dir_save)
  manifest <- .projr_manifest_read(file.path(path_dir_save, "manifest.csv"))
  unlink(path_dir_save, recursive = TRUE)
  manifest
}

.projr_manifest_get_version <- function(type,
                                        remote,
                                        label) {
  remote <- 
  .projr_manifest_get(type, remote) |>
    .projr_manifest_version_get_latest()
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
