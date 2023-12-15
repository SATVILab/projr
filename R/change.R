.projr_change_get <- function(label,
                              output_run,
                              version_source,
                              remote_type,
                              remote_final) {
  .projr_change_get_check(label, version_source)
  switch(version_source,
    "manifest" = .projr_change_get_manifest(label = label),
    "file" = .projr_change_get_file(
      remote_type_pre = remote_type,
      remote_final_pre = remote_final,
      remote_type_post = "local",
      remote_final_post = projr_dir_get(label, safe = !output_run)
    ),
    stop(paste0("version_source '", version_source, "' not recognized"))
  )
}

.projr_change_get_check <- function(label,
                                    version_source) {
  .projr_check_given(label, "label")
  .projr_check_chr_single(label, "label")
  .projr_check_nz(label, "label")
  .projr_check_given(version_source, "version_source")
  .projr_check_chr_single(version_source, "version_source")
}

# manifest-based
# ------------------------

.projr_change_get_manifest <- function(version_post = NULL,
                                       version_pre = NULL,
                                       label = NULL) {
  # this differs from .projr_change_get_hash
  # as it will filter on version and does
  # not assume there is only one label
  # get manifests from previous version and current version
  manifest <- .projr_manifest_read(.projr_dir_proj_get("manifest.csv"))

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

  # use zero table if version_pre not found
  manifest_pre <- manifest[manifest[["version"]] == version_vec[["pre"]], ] %@@%
    .projr_zero_tbl_get_manifest()

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
    version_pre, version_post, manifest
  )
  c("post" = version_post, "pre" = version_pre)
}

.projr_change_get_manifest_version_post <- function(version_post = NULL) {
  (version_post %||% projr_version_get()) |>
    .projr_version_v_add()
}

.projr_change_get_manifest_version_pre <- function(version_pre = NULL,
                                                   version_post,
                                                   manifest) {
  if (is.null(version_pre)) {
    return(.projr_change_get_manifest_version_pre_null(
      manifest = manifest,
      version_post = version_post
    ))
  }
  version_pre |> .projr_version_v_rm()
}

.projr_change_get_manifest_version_pre_null <- function(manifest, version_post) {
  version_vec_manifest <- manifest[["version"]] |> .projr_version_v_rm()
  version_post <- version_post |> .projr_version_v_rm()
  manifest_pre_all <- manifest[version_vec_manifest < version_post, , drop = FALSE]
  if (nrow(manifest_pre_all) == 0L) {
    return(character())
  }
  manifest_pre_all |>
    .projr_manifest_version_get_latest() |>
    .projr_version_v_add()
}

.projr_zero_list_manifest_get <- function() {
  list(
    "removed" = character(),
    "kept_unchanged" = character(),
    "kept_changed" = character(),
    "added" = character()
  )
}

# file-based
# ------------------------

.projr_change_get_file <- function(remote_type_pre = NULL,
                                   remote_final_pre = NULL,
                                   remote_type_post = NULL,
                                   remote_final_post = NULL) {
  # get directories where files are found or saved to,
  # downloading them to there if necessary
  # remote_type_: remote type (local, osf, github)
  # remote_final_: exactly where the data are on the remote
  path_dir_local_pre <- .projr_change_get_file_dir(
    remote_type = remote_type_pre,
    remote_final = remote_final_pre
  )
  path_dir_local_post <- .projr_change_get_file_dir(
    remote_type = remote_type_post,
    remote_final = remote_final_post
  )
  # get hashes
  .projr_change_get_dir(
    path_dir_pre = path_dir_local_pre, path_dir_post = path_dir_local_post
  )
}

.projr_change_get_file_dir <- function(remote_type,
                                       remote_final) {
  # to download the data to a local directory,
  # so that we can hash
  switch(remote_type,
    "local" = .projr_change_get_file_dir_local(remote_final),
    "osf" = .projr_change_get_file_dir_osf(remote_final),
    "github" = .projr_change_get_file_dir_github(remote_final)
  )
}

.projr_change_get_file_dir_local <- function(remote_final) {
  remote_final
}

.projr_change_get_file_dir_osf <- function(remote_final) {
  .projr_remote_file_get_all("osf", remote_final, .projr_dir_tmp_random_get())
}

.projr_change_get_file_dir_github <- function(remote_final) {
  .projr_remote_file_get_all(
    "github", remote_final, .projr_dir_tmp_random_get()
  )
}

# compare hashes
# ------------------------

# between two directories
.projr_change_get_dir <- function(path_dir_pre,
                                  path_dir_post) {
  hash_tbl_pre <- .projr_hash_dir(path_dir_pre)
  hash_tbl_post <- .projr_hash_dir(path_dir_post)
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
