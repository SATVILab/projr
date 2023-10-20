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
        label = character(0), fn = character(0),
        version = character(0), hash = character(0)
      )
    )
  }
  hash_tbl_list <- lapply(
    cache_vec, .projr_manifest_hash_label,
    output_run = output_run
  )
  out_tbl <- Reduce(rbind, hash_tbl_list)
  rownames(out_tbl) <- NULL
  out_tbl
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
        label = character(0), fn = character(0),
        version = character(0), hash = character(0)
      )
    )
  }
  hash_tbl_list <- lapply(
    label_vec, .projr_manifest_hash_label,
    output_run = output_run
  )
  out_tbl <- Reduce(rbind, hash_tbl_list)
  rownames(out_tbl) <- NULL
  out_tbl
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
      version = character(0),
      hash = character(0)
    ))
  }
  fn_to_hash_vec <- vapply(fn_vec, function(fn) {
    digest::digest(fn, serialize = FALSE, file = TRUE)
  }, character(1))
  out_tbl <- data.frame(
    label = label,
    fn = fs::path_rel(fn_vec, path_dir) |> as.character(),
    version = paste0("v", projr_version_get()),
    hash = fn_to_hash_vec
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_manifest_write <- function(manifest, output_run, append = TRUE) {
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
  rownames(manifest) <- NULL
  if (append) {
    path_manifest <- file.path(dir_write, "manifest.csv")
    if (file.exists(path_manifest)) {
      manifest_orig <- .projr_manifest_read()
      rownames(manifest_orig) <- NULL
      manifest <- manifest_orig |> rbind(manifest)
      rownames(manifest) <- NULL
    }
  }
  utils::write.csv(
    manifest, file.path(dir_write, "manifest.csv"),
    row.names = FALSE
  )
}

.projr_manifest_read <- function() {
  out_tbl <- utils::read.csv(
    projr_path_get("project", "manifest.csv"),
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_manifest_compare <- function(manifest_pre, manifest_post) {
  zero_row_tbl <- data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  # get last version uploaded
  # get version that is being uploaded to
  # get list of whatever is in the OSF registry
  # get all files that have been added
  version_pre <- .projr_manifest_get_version_latest(manifest_pre)
  manifest_pre <- manifest_pre[
    manifest_pre[["version"]] == version_pre,
  ]
  manifest_post <- manifest_post[
    manifest_post[["version"]] == paste0("v", projr_version_get()),
  ]
  fn_vec_pre_lgl_removed <- !manifest_pre[["fn"]] %in% manifest_post[["fn"]]
  fn_vec_post_lgl_kept <- manifest_post[["fn"]] %in% manifest_pre[["fn"]]
  fn_vec_post_lgl_add <- !manifest_post[["fn"]] %in% manifest_pre[["fn"]]
  manifest_post_kept <- manifest_post[fn_vec_post_lgl_kept, ]
  hash_from_fn_pre <- setNames(
    manifest_pre[["hash"]], manifest_pre[["fn"]]
  )
  hash_vec_post_match <- manifest_post_kept[["hash"]] ==
    hash_from_fn_pre[manifest_post_kept[["fn"]]]
  if (nrow(manifest_post) == 0L) {
    manifest_post_kept_unchanged <- zero_row_tbl
    manifest_post_kept_changed <- zero_row_tbl
    manifest_post_add <- zero_row_tbl
  } else {
    manifest_post_add <- manifest_post[fn_vec_post_lgl_add, ]
    manifest_post_kept_unchanged <- manifest_post_kept[hash_vec_post_match, ]
    manifest_post_kept_changed <- manifest_post_kept[!hash_vec_post_match, ]
  }
  if (nrow(manifest_pre) == 0L) {
    manifest_removed <- zero_row_tbl
  } else {
    manifest_removed <- manifest_pre[fn_vec_pre_lgl_removed, ]
  }
  list(
    "removed" = manifest_removed,
    "kept_unchanged" = manifest_post_kept_unchanged,
    "kept_changed" = manifest_post_kept_changed,
    "added" = manifest_post_add
  )
}

.projr_manifest_get_version_latest <- function(manifest) {
  manifest[["version"]] |>
    unique() |>
    sort() |>
    utils::tail(1)
}

.projr_manifest_compare_version <- function(version_post = NULL,
                                            version_pre = NULL,
                                            label = NULL) {
  # get manifests from previous version and current version
  manifest <- .projr_manifest_read()

  if (nrow(manifest) == 0L) {
    return(.projr_manifest_compare(
      manifest_pre = manifest, manifest_post = manifest
    ))
  }

  if (is.null(version_post)) {
    version_post <- paste0("v", projr_version_get())
  } else {
    if (!grepl("^v", version_post)) {
      version_post <- paste0("v", version_post)
    }
  }

  if (is.null(version_pre)) {
    manifest_pre_all <- manifest[manifest[["version"]] < version_post, ]
    if (nrow(manifest_pre_all) == 0L) {
      version_pre <- "nothing_to_match"
    }
    version_pre <- .projr_manifest_get_version_latest(manifest_pre_all)
  } else {
    if (!grepl("^v", version_pre)) {
      version_pre <- paste0("v", version_pre)
    }
  }

  if (!is.null(label)) {
    manifest <- manifest[manifest[["label"]] == label, ]
  }

  manifest_pre <- manifest[manifest[["version"]] == version_pre, ]

  manifest_post <- manifest[manifest[["version"]] == version_post, ]

  # compare
  # -----------------
  .projr_manifest_compare(
    manifest_pre = manifest_pre, manifest_post = manifest_post
  )
}
