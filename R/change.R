.projr_change_get <- function(label,
                              output_run,
                              path_dir_local = NULL,
                              version_source,
                              type,
                              remote) {
  .projr_change_get_check(label, path_dir_local, version_source)
  switch(version_source,
    "manifest" = .projr_change_get_manifest(
      type_pre = type,
      remote_pre = remote,
      type_post = "project",
      remote_post = NULL,
      label = label
    ),
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

.projr_change_get_manifest <- function(type_pre,
                                       remote_pre,
                                       type_post,
                                       remote_post,
                                       label = NULL) {
  .assert_given_mid(label)
  version_pre_actual <- .projr_remote_get_version_label(
    remote_pre, type_pre, label
    )
  version_post_actual <- .projr_remote_get_version_label(
    remote_post, type_post, label
    )
  # this differs from .projr_change_get_hash
  # as it will filter on version
  # get manifests from previous version and current version
  manifest_pre_full <- .projr_remote_get_manifest(type_pre, remote_pre) |>
    .projr_manifest_filter_label(label)
  manifest_post_full <- .projr_remote_get_manifest(type_post, remote_post) |>
    .projr_manifest_filter_label(label)

  if (.projr_change_get_manifest_check_nothing(manifest_pre_full, manifest_post_full)) {
    # this is if no files are specified in either location
    return(.projr_zero_list_manifest_get())
  }

  # filter for actual version
  manifest_post <- manifest_post_full |>
    .projr_manifest_filter_version(version_post_actual)
  
  manifest_pre <- .projr_change_get_manifest_pre_final(
    version_pre_actual, version_post_actual, manifest_post,
    manifest_pre_full, manifest_post_full, type_pre, remote_pre
  )

  if (.projr_change_get_manifest_check_nothing(manifest_pre, manifest_post)) {
    # this is if no files are specified in either location
    return(.projr_zero_list_manifest_get())
  } 

  .projr_change_get_hash(hash_pre = manifest_pre, hash_post = manifest_post)
}

.projr_change_get_manifest_check_nothing <- function(manifest_pre, manifest_post) {
  nrow(manifest_pre) == 0L && nrow(manifest_post) == 0L
}

.projr_change_get_manifest_pre_final <- function(version_pre_actual,
                                                 version_post_actual,
                                                 manifest_post,
                                                 manifest_pre_full,
                                                 manifest_post_full,
                                                 type_pre,
                                                 remote_pre) {
  if (.is_len_0(version_pre_actual)) {
    # has it directly if not available
    path_dir_local_pre <- .projr_change_get_file_dir(
      type = type_pre,
      remote = remote_pre
    )
    .projr_hash_dir(path_dir_local_pre)

  } else {
    version_pre_final <- .projr_change_get_manifest_get_closest_mismatch(
      version_pre_actual, version_post_actual, manifest_post,
      manifest_pre_full, manifest_post_full
    )
    manifest_pre_full |>
      .projr_manifest_filter_version(version_pre_final)
  }
}


.projr_change_get_manifest_get_closest_mismatch <- function(version_pre,
                                                            version_post,
                                                            manifest_post,
                                                            manifest_pre_full,
                                                            manifest_post_full) {
  # get the closest mismatch to the latest version,
  # and if there are no mismatches just return the 
  # previous version (furthest away version)
  version_vec_pre <- manifest_pre_full[["version"]] |> .projr_version_v_rm()
  version_vec_post <- manifest_post_full[["version"]] |> .projr_version_v_rm()
  version_vec <- c(version_vec_pre, version_vec_post) |>
    unique() |>
    sort() |>
    rev()
  version_closest_mismatch <- version_pre
  version_vec_loop <- version_vec[version_vec >= version_pre &
    version_vec <= version_post]
  for (i in seq_along(version_vec_loop)) {
    version_curr <- version_vec_loop[[i]]
    manifest_curr <- 
      .projr_change_get_manifest_get_closest_mismatch_get_manifest_curr(
        manifest_pre_full, manifest_post_full, version_curr
      )
    if (!identical(manifest_curr, manifest_post)) {
      version_closest_mismatch <- version_curr
      break
    }
  }
  version_closest_mismatch
}

.projr_change_get_manifest_get_closest_mismatch_get_manifest_curr <-
  function(manifest_pre, manifest_post, version) {
    # prefer the manifest from the previous version,
    # as it was uploaded with it,
    # but otherwise use the manifest from the post version
    # (especially since the pre-manifest may not be up to date)
    if (version %in% manifest_pre[["version"]]) {
      manifest_pre |> .projr_manifest_filter_version(version)
    } else {
      # will return empty  manifest if not found
      manifest_post |> .projr_manifest_filter_version(version)
    }
  }

.projr_zero_list_manifest_get <- function() {
  lapply(1:4, function(x) character(0L)) |>
    stats::setNames(c("fn_dest_extra", "fn_same", "fn_diff", "fn_source_extra"))
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
  hash_tbl_pre <- stop("this should not happen like this")
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
    "fn_dest_extra" = hash_removed,
    "fn_same" = hash_post_kept_unchanged,
    "fn_diff" = hash_post_kept_changed,
    "fn_source_extra" = hash_post_add
  )
}
