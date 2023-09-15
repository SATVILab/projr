# hashed beforehand
.projr_build_manifest_hash_pre <- function(output_run) {
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
  hash_tbl_list <- lapply(
    cache_vec, .projr_manifest_hash_label,
    output_run = output_run
  )
  Reduce(rbind, hash_tbl_list)
}

.projr_build_manifest_hash_post <- function(output_run) {
  yml_projr_dir <- projr_yml_get()[["directories"]]
  label_vec <- names(yml_projr_dir)[
    grepl(
      "^dataraw|^output|^docs",
      .projr_dir_label_strip(names(yml_projr_dir))
    )
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
  hash_tbl_list <- lapply(
    label_vec, .projr_manifest_hash_label,
    output_run = output_run
  )
  Reduce(rbind, hash_tbl_list)
}

.projr_manifest_hash_label <- function(label, output_run) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  path_dir <- projr::projr_dir_get(label, output_safe = !output_run)
  fn_vec <- list.files(path_dir, full.names = TRUE, recursive = TRUE)
  if (length(fn_vec) == 0) {
    return(data.frame(
      label = character(0),
      fn = character(0),
      hash = character(0)
    ))
  }
  fn_to_hash_vec <- vapply(fn_vec, function(fn) {
    digest::digest(fn, serialize = FALSE, file = TRUE)
  }, character(1))
  data.frame(
    label = label,
    fn = fs::path_rel(fn_vec, path_dir) |> as.character(),
    version = paste0("v", projr_version_get()),
    hash = fn_to_hash_vec
  )
}

.projr_manifest_write <- function(manifest, output_run) {
  if (!output_run) {
    dir_cache_auto <- .projr_dir_get_cache_auto()
    dir_write <- file.path(
      dir_cache_auto, "projr", paste0("v", projr_version_get())
    )
    if (!dir.exists(dir_write)) {
      dir.create(dir_write, recursive = TRUE)
    }
  } else {
    dir_write <- projr_dir_get("project")
  }
  utils::write.csv(
    manifest, file.path(dir_write, "manifest.csv"),
    append = TRUE
  )
}

.projr_manifest_read <- function() {
  utils::read.csv(
    projr_path_get("project", "manifest.csv"),
    stringsAsFactors = FALSE
  )
}

.projr_manifest_compare <- function(manifest_add, manifest2) {

}
