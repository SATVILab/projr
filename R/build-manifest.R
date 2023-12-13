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
  path_file <- .projr_dir_get_cache_auto_version("manifest.csv")
  saveRDS(out_tbl, path_file)
  invisible(path_file)
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
  .projr_build_hash_pre_read() |>
    rbind(.projr_build_hash_post_get(output_run)) |>
    .projr_manifest_write(output_run)
}

.projr_build_hash_pre_read <- function() {
  .projr_dir_get_cache_auto_version("manifest.csv") |>
    readRDS()
}

.projr_build_hash_post_get <- function(output_run) {
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
