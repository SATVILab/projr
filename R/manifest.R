# hashed beforehand
.projr_build_hash_pre <- function(output_run) {
  # get labels
  label_vec <- .projr_build_hash_pre_get_label()

  # return early if need be
  if (length(label_vec) == 0) {
    return(.projr_zero_tbl_get_manifest())
  }

  # hash and return
  out_tbl <- lapply(
    label_vec, .projr_hash_label,
    output_run = output_run
  ) |>
    Reduce(f = rbind, x = _)
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_build_hash_pre_get_label <- function() {
  yml_projr_dir <- projr_yml_get()[["directories"]]
  cache_vec <- names(yml_projr_dir)[
    grepl("^cache", .projr_dir_label_strip(names(yml_projr_dir)))
  ]
  # decide whether to hash or not - default is to not
  for (x in cache_vec) {
    if (!"hash" %in% names(yml_projr_dir[[x]])) {
      cache_vec <- cache_vec[!cache_vec == x]
      next
    }
    if (!yml_projr_dir[[x]][["hash"]]) {
      cache_vec <- cache_vec[!cache_vec == x]
    }
  }
  cache_vec
}

.projr_build_hash_post <- function(output_run) {
  label_vec <- .projr_build_hash_post_get_label()
  if (length(label_vec) == 0) {
    return(.projr_zero_tbl_get_manifest())
  }
  hash_tbl_list <- lapply(
    label_vec, .projr_hash_label,
    output_run = output_run
  ) |>
    Reduce(f = rbind, x = _)
  out_tbl <- Reduce(rbind, hash_tbl_list)
  rownames(out_tbl) <- NULL
  out_tbl
}

.projr_build_hash_post_get_label <- function() {
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
  # decide whether to include for hashing or not
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
  label_vec
}

.projr_hash_label <- function(label,
                              output_run = NULL) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .projr_hash_dir(
    path_dir = projr_dir_get(label, output_safe = !output_run)
  )
  cbind(
    data.frame(label = rep(label, nrow(hash_tbl))),
    hash_tbl
  )
}

.projr_hash_dir <- function(path_dir, version = NULL) {
  fn_vec <- list.files(path_dir, full.names = TRUE, recursive = TRUE)
  version <- version %||% projr_version_get()
  if (length(fn_vec) == 0L) {
    return(.projr_zero_tbl_get_hash())
  }
  fn_to_hash_vec <- vapply(fn_vec, function(fn) {
    digest::digest(fn, serialize = FALSE, file = TRUE)
  }, character(1))
  out_tbl <- data.frame(
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
  manifest[["string"]] <- Reduce(paste0, manifest)
  manifest <- manifest[!duplicated(manifest[["string"]]), ]
  manifest[["string"]] <- NULL
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
