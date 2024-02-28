.projr_change_get <- function(label,
                              output_run,
                              path_dir_local = NULL,
                              version_source,
                              type,
                              remote) {
  .projr_change_get_check(label, path_dir_local, version_source)
  switch(version_source,
    "manifest" = .projr_change_get_manifest(label = label),
    "file" = .projr_change_get_file(
      type_pre = type,
      remote_pre = remote,
      type_post = "local",
      remote_post = path_dir_local %||%
        projr_path_get_dir(label, safe = !output_run)
    ),
    stop(paste0("version_source '", version_source, "' not recognized"))
  )
}

.projr_change_get_check <- function(label,
                                    path_dir_local,
                                    version_source) {
  if (is.null(path_dir_local)) {
    .assert_string(label, required = TRUE)
  }
  .assert_given(version_source)
  .assert_string(version_source)
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
  manifest <- .projr_manifest_read(.dir_proj_get("manifest.csv"))

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
  version_pre |> .projr_version_v_add()
}

.projr_change_get_manifest_version_pre_null <- function(manifest, version_post) {
  if (nrow(manifest) == 0L) {
    return(character())
  }
  version_vec_manifest <- manifest[["version"]] |> .projr_version_v_rm()
  version_post <- version_post |> .projr_version_v_rm()
  # FIX THIS
  manifest_pre_all <- manifest[
    vapply(
      version_vec_manifest,
      function(x) {
        utils::compareVersion(x, version_post) == -1
      },
      logical(1)
    ), ,
    drop = FALSE
  ]
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

.projr_change_get_file <- function(type_pre = NULL,
                                   remote_pre = NULL,
                                   type_post = NULL,
                                   remote_post = NULL) {
  # get directories where files are found or saved to,
  # downloading them to there if necessary
  # type_: remote type (local, osf, github)
  # remote_: exactly where the data are on the remote
  path_dir_local_pre <- .projr_change_get_file_dir(
    type = type_pre,
    remote = remote_pre
  )
  path_dir_local_post <- .projr_change_get_file_dir(
    type = type_post,
    remote = remote_post
  )
  # get hashes
  .projr_change_get_dir(
    path_dir_pre = path_dir_local_pre, path_dir_post = path_dir_local_post
  )
}

.projr_change_get_file_dir <- function(type,
                                       remote) {
  # to download the data to a local directory,
  # so that we can hash
  switch(type,
    "local" = .projr_change_get_file_dir_local(remote),
    "osf" = .projr_change_get_file_dir_osf(remote),
    "github" = .projr_change_get_file_dir_github(remote)
  )
}

.projr_change_get_file_dir_local <- function(remote) {
  remote
}

.projr_change_get_file_dir_osf <- function(remote) {
  .projr_remote_file_get_all("osf", remote, .dir_create_tmp_random())
}

.projr_change_get_file_dir_github <- function(remote) {
  .projr_remote_file_get_all(
    "github", remote, .dir_create_tmp_random()
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
  hash_from_fn_pre <- stats::setNames(
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
