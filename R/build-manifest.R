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
  .manifest_hash_label(.build_manifest_pre_get_label(), output_run = FALSE)
}

.build_manifest_pre_get_label <- function() {
  .build_manifest_labels(c("cache", "raw"))
}

.build_manifest_labels <- function(types) {
  directories <- .yml_dir_get(NULL)
  labels <- names(directories)
  stripped <- .dir_label_strip(labels)
  matches <- lapply(types, function(type) {
    labels[startsWith(stripped, type)]
  })
  labels <- as.character(unlist(matches, use.names = FALSE))
  labels[vapply(labels, function(label) {
    .yml_dir_complete_hash(directories[[label]][["hash"]], label)
  }, logical(1))]
}

# post
# --------------------------------
.build_manifest_post <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  current <- .build_manifest_reduce(list(
    .build_manifest_pre_read(), .build_manifest_post_get_manifest(output_run)
  ))
  .manifest_store_write(current, .build_manifest_post_get_path(output_run))
}

.build_manifest_pre_read <- function() {
  path_manifest <- .build_manifest_pre_path_get()
  if (file.exists(path_manifest)) {
    return(.manifest_read(path_manifest))
  }
  .zero_tbl_get_manifest()
}

.build_manifest_post_get_manifest <- function(output_run) {
  .manifest_hash_label(.build_manifest_post_get_label(), output_run)
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
  .build_manifest_labels(c("docs", "output"))
}

.build_manifest_post_get_path <- function(output_run) {
  if (output_run) {
    return(.path_get("_projr", "manifests"))
  }
  .dir_get_cache_auto_version("manifest.csv", profile = NULL)
}
