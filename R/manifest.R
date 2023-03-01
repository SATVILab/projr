.projr_build_manifest_save <- function(manifest) {
  yml_projr_dir <- projr_yml_get()[["directories"]]
  label_vec <- names(yml_projr_dir)[
    grepl("^archive|^output", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  for (x in label_vec) {
    if ("manifest" %in% names(yml_projr_dir[[x]])) {
      if (!yml_projr_dir[[x]][["manifest"]]) {
        next
      }
    }
    saveRDS(
      manifest, projr_path_get(x, "manifest.rds", output_safe = FALSE)
    )
    write.csv(
      manifest, projr_path_get(x, "manifest.csv", output_safe = FALSE)
    )
  }
  invisible(TRUE)
}

# hashed beforehand
.projr_build_manifest_hash_pre <- function() {
  yml_projr_dir <- projr_yml_get()[["directories"]]
  cache_vec <- names(yml_projr_dir)[
    grepl("^cache", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  for (x in cache_vec) {
    if (!"hash" %in% names(yml_projr_dir[[x]])) {
      cache_vec <- cache_vec[!cache_vec == x]
      next
    }
    if (!yml_projr_dir[[x]][["hash"]]) {
      cache_vec <- cache_vec[!cache_vec == x]
    }
  }
  if (length(cache_vec) == 0) {
    return(
      data.frame(
        label = character(0), fn = character(0), hash = character(0)
      )
    )
  }
  hash_tbl_list <- lapply(cache_vec, .projr_manifest_hash_label)
  Reduce(rbind, hash_tbl_list)
}

.projr_build_manifest_hash_post <- function() {
  yml_projr_dir <- projr_yml_get()[["directories"]]
  label_vec <- names(yml_projr_dir)[
    grepl("^dataraw|^output|^docs", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  if (!"docs" %in% label_vec) {
    label_vec <- c(label_vec, "docs")
  }
  for (x in label_vec) {
    if (grepl("^docs", .projr_dir_label_strip(x))) {
      if (!"hash" %in% names(yml_projr_dir[[x]])) {
        label_vec <- label_vec[!label_vec == x]
        next
      }
      if (!yml_projr_dir[[x]][["hash"]]) {
        label_vec <- label_vec[!label_vec == x]
      }
    } else if ("hash" %in% names(yml_projr_dir[[x]])) {
      if (!yml_projr_dir[[x]][["hash"]]) {
        label_vec <- label_vec[!label_vec == x]
      }
    }
  }
  if (length(label_vec) == 0) {
    return(
      data.frame(
        label = character(0), fn = character(0), hash = character(0)
      )
    )
  }
  hash_tbl_list <- lapply(label_vec, .projr_manifest_hash_label)
  Reduce(rbind, hash_tbl_list)
}

.projr_manifest_hash_label <- function(label) {
  path_dir <- projr::projr_dir_get(label, output_safe = TRUE)
  fn_vec <- list.files(path_dir, full.names = TRUE, recursive = TRUE)
  fn_to_hash_vec <- vapply(fn_vec, function(fn) {
    digest::digest(fn, serialize = FALSE, file = TRUE)
  }, character(1))
  data.frame(
    label = label,
    fn = fs::path_rel(fn_vec, path_dir) |> as.character(),
    hash = fn_to_hash_vec
  )
}
