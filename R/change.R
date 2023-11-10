# ------------------------
# overall function
# ------------------------

.projr_change_get <- function(label,
                              output_run,
                              remote_base,
                              remote_final,
                              path_remote_rel,
                              remote_type,
                              version_source) {
  switch(version_source,
    "manifest" = .projr_change_get_manifest(label = label),
    "file" = .projr_change_get_file(
      label = label,
      remote_base = remote_base,
      path_remote_rel = path_remote_rel
    ),
    stop(paste0("version_source '", version_source, "' not recognized"))
  )
}

# ------------------------
# manifest-based
# ------------------------
.projr_change_get_manifest <- function(version_post = NULL,
                                       version_pre = NULL,
                                       label = NULL) {
  # this differs from .projr_change_get_hash
  # as it will filter on version and does
  # not assume there is only one label
  # get manifests from previous version and current version
  manifest <- .projr_manifest_read()

  if (nrow(manifest) == 0L) {
    return(.projr_zero_list_manifest_get())
  }

  # get version to compare
  version_vec <- .projr_change_get_manifest_version_to_compare(
    version_post = version_post,
    version_pre = version_pre,
    manifest = manifest
  )

  # choose current label only,
  # done after comparing to ensure we get the right comparison
  if (!is.null(label)) {
    manifest <- manifest[manifest[["label"]] == label, ]
  }

  manifest_pre <- manifest[manifest[["version"]] == version_vec[["pre"]], ]

  manifest_post <- manifest[manifest[["version"]] == version_vec[["post"]], ]

  # compare
  # -----------------

  # can't assume there's only one label
  .projr_change_get_hash(hash_pre = manifest_pre, hash_post = manifest_post)
}

.projr_change_get_manifest_version_to_compare <- function(version_post = NULL,
                                                          version_pre = NULL,
                                                          manifest) {
  version_post <- .projr_change_get_manifest_version_post(
    version_post = version_post
  )
  version_pre <- .projr_change_get_manifest_version_pre(
    version_pre = version_pre, manifest = manifest
  )
  c("post" = version_post, "pre" = version_pre)
}

.projr_change_get_manifest_version_post <- function(version_post = NULL) {
  if (is.null(version_post)) {
    version_post <- paste0("v", projr_version_get())
  } else {
    if (!grepl("^v", version_post)) {
      version_post <- paste0("v", version_post)
    }
  }
  version_post
}
.projr_change_get_manifest_version_pre <- function(version_pre = NULL,
                                                   manifest) {
  if (is.null(version_pre)) {
    manifest_pre_all <- manifest[manifest[["version"]] < version_post, ]
    if (nrow(manifest_pre_all) == 0L) {
      version_pre <- "nothing_to_match"
    }
    version_pre <- .projr_manifest_version_get_latest(manifest_pre_all)
  } else {
    if (!grepl("^v", version_pre)) {
      version_pre <- paste0("v", version_pre)
    }
  }
  version_pre
}

.projr_zero_list_manifest_get <- function() {
  list(
    "removed" = character(),
    "kept_unchanged" = character(),
    "kept_changed" = character(),
    "added" = character()
  )
}

# ------------------------
# file-based
# ------------------------

.projr_change_get_file <- function(label_pre = NULL,
                                   output_run = NULL,
                                   path_dir_local_pre = NULL,
                                   remote_type_pre = NULL,
                                   remote_base_pre = NULL,
                                   remote_final_pre = NULL,
                                   path_remote_rel_pre = NULL,
                                   label_post = NULL,
                                   remote_type_post = NULL,
                                   remote_base_post = NULL,
                                   remote_final_post = NULL,
                                   path_remote_rel_post = NULL,
                                   path_dir_local_post = NULL) {
  # get directories where files are found or saved to,
  # downloading them to there if necessary
  path_dir_local_pre <- .projr_change_get_file_get(
    label = label_pre,
    output_run = output_run,
    path_dir_local = path_dir_local_pre,
    remote_type = remote_type_pre,
    remote_base = remote_base_pre,
    remote_final = remote_final_pre,
    path_remote_rel = path_remote_rel_pre
  )
  path_dir_local_post <- .projr_change_get_file_get(
    label = label_post,
    output_run = output_run,
    path_dir_local = path_dir_local_post,
    remote_type = remote_type_post,
    remote_base = remote_base_post,
    remote_final = remote_final_post,
    path_remote_rel = path_remote_rel_post
  )
  # get hashes
  .projr_change_get_dir(
    dir_pre = path_dir_local_pre, dir_post = path_dir_local_post
  )
}

.projr_change_get_file_get <- function(label,
                                       output_run,
                                       path_dir_local,
                                       remote_type,
                                       remote_base,
                                       remote_final,
                                       path_remote_rel) {
  switch(remote_type,
    "local" = .projr_change_get_file_get_local(
      label = label,
      output_run = output_run,
      path_dir_local = path_dir_local
    ),
    "osf" = .projr_change_get_file_get_osf(
      label = label,
      remote_base = remote_base,
      remote_final = remote_final,
      path_remote_rel = path_remote_rel
    )
  )
}

.projr_change_get_file_get_local <- function(label,
                                             output_run,
                                             path_dir_local = NULL) {
  if (!is.null(path_dir_local)) {
    return(path_dir_local)
  }
  projr_dir_get(label, output_safe = !output_run)
}

.projr_change_get_file_get_osf <- function(remote_base,
                                           remote_final = NULL,
                                           path_remote_rel) {
  if (is.null(remote_final)) {
    remote_final <- remote_base |> osfr::osf_mkdir(path = path_remote_rel)
  }
  path_dir_save_local <- file.path(tempdir(), "osf", signif(rnorm(1)))
  if (dir.exists(path_dir_save_local)) {
    unlink(path_dir_save_local, recursive = TRUE)
  }
  dir.create(path_dir_save_local, recursive = TRUE)

  path_dir_save_local <- .projr_osf_download(
    osf_tbl = remote_final,
    path_dir_save_local = path_dir_save_local,
    conflicts = "overwrite"
  )
  path_dir_save_local
}

# ------------------------
# just compare the hashes
# ------------------------

# between two directories
.projr_change_get_dir <- function(dir_pre,
                                  dir_post) {
  hash_tbl_pre <- .projr_hash_dir(path_dir = dir_pre)
  hash_tbl_post <- .projr_hash_dir(path_dir = dir_post)
  .projr_change_get_hash(hash_pre = hash_tbl_pre, hash_post = hash_tbl_post)
}

# of hash tables
.projr_change_get_hash <- function(hash_pre, hash_post) {
  fn_vec_pre_lgl_removed <- !hash_pre[["fn"]] %in% hash_post[["fn"]]
  fn_vec_post_lgl_kept <- hash_post[["fn"]] %in% hash_pre[["fn"]]
  fn_vec_post_lgl_add <- !hash_post[["fn"]] %in% hash_pre[["fn"]]
  hash_post_kept <- hash_post[fn_vec_post_lgl_kept, ]
  hash_from_fn_pre <- setNames(
    hash_pre[["hash"]], hash_pre[["fn"]]
  )
  hash_vec_post_match <- hash_post_kept[["hash"]] ==
    hash_from_fn_pre[hash_post_kept[["fn"]]]
  if (nrow(hash_post) == 0L) {
    hash_post_kept_unchanged <- .projr_zero_tbl_get_manifest()
    hash_post_kept_changed <- .projr_zero_tbl_get_manifest()
    hash_post_add <- .projr_zero_tbl_get_manifest()
  } else {
    hash_post_add <- hash_post[fn_vec_post_lgl_add, ]
    hash_post_kept_unchanged <- hash_post_kept[hash_vec_post_match, ]
    hash_post_kept_changed <- hash_post_kept[!hash_vec_post_match, ]
  }
  if (nrow(hash_pre) == 0L) {
    hash_removed <- .projr_zero_tbl_get_manifest()
  } else {
    hash_removed <- hash_pre[fn_vec_pre_lgl_removed, ]
  }
  list(
    "removed" = hash_removed,
    "kept_unchanged" = hash_post_kept_unchanged,
    "kept_changed" = hash_post_kept_changed,
    "added" = hash_post_add
  )
}
