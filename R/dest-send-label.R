.dest_send_label <- function(label,
                             title,
                             type,
                             output_run,
                             archive_type,
                             always_archive,
                             changelog) {
  force(title)
  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run) # nolint
  yml_title <- .yml_dest_get_title_complete( # nolint
    title, type, NULL, archive_type, always_archive
  )
  remote_list <- .dest_send_label_get_remotes(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    label, yml_title[["structure"]],
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    yml_title[["send"]][["cue"]]
  )

  plan <- .dest_send_label_get_plan(
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    remote_list[["version_comp"]], type, label,
    remote_list[["remote_pre"]], remote_list[["remote_dest"]],
    remote_list[["remote_comp"]], yml_title[["send"]][["cue"]],
    changelog
  )

  .dest_send_label_implement_plan(
    plan[["fn_add"]], plan[["fn_rm"]], plan[["version"]],
    plan[["manifest"]], plan[["create"]], plan[["purge"]],
    plan[["changelog"]],
    remote_list[["remote_dest"]], type, yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    path_dir_local, remote_list[["remote_pre"]]
  )
}

# ==========================================================================
# Get remotes
# ==========================================================================

.dest_send_label_get_remotes <- function(type,
                                         id,
                                         path,
                                         path_append_label,
                                         label,
                                         structure,
                                         strategy,
                                         inspect,
                                         cue) {
  remote_pre <- .remote_get_final(
    type, id, label, structure, path, path_append_label, NULL, TRUE
  )
  remote_dest <- .remote_get_final_if_exists(
    type, id, label, structure, path, path_append_label, NULL
  )
  version_comp <- .dest_send_label_get_remotes_get_version_comp(
    remote_pre, type, label, structure, strategy, inspect, cue
  )
  remote_comp <- .dest_send_label_get_remotes_comp(
    type, id, label, structure, path, path_append_label, version_comp,
    remote_dest
  )
  list(
    "remote_pre" = remote_pre,
    "remote_dest" = remote_dest,
    "remote_comp" = remote_comp,
    "version_comp" = version_comp
  )
}

.dest_send_label_get_remotes_get_version_comp <- function(remote_pre, # nolint
                                                          type,
                                                          label,
                                                          structure,
                                                          strategy,
                                                          inspect,
                                                          cue) {
  is_nothing <-
    .dest_send_label_get_remotes_get_version_comp_check_nothing(
      remote_pre, inspect, strategy
    )
  if (is_nothing) {
    return(NULL)
  }
  # in all cases here, we're just trying to find
  # the version on the remote that is acceptable to compare against.
  # the major difference between `latest` and `archive`
  # is that in `latest` we don't care about whether
  # there is a difference in the files in between now
  # and the last upload (even if the last upload is the
  # same as now), and we also don't care about
  # checking what the versioned uploaded file/folder
  # is
  if (structure == "latest") {
    return(.dest_send_label_get_remotes_get_version_comp_latest(
      inspect, remote_pre, type, label
    ))
  }
  # now, it's archive, but we compare
  # against the latest version to see what
  # we should upload, as now we're not interested
  # in old remotes, but the latest one
  # does upload-missing become upload-all here?
  # yes, essentially, with cue: always, as we're
  # not going to adjust old versions so it's basically
  # just upload all
  if (cue == "always" || strategy == "upload-missing") {
    return(projr_version_get() |> .version_v_rm())
  }
  # now we see if we can compare against an archived remote
  .dest_send_label_get_remotes_get_version_comp_archive(
    cue, strategy, label, inspect, remote_pre, type
  )
}

.dest_send_label_get_remotes_get_version_comp_check_nothing <- # nolint
  function(remote_pre,
           inspect,
           strategy) {
    # in this initial cases, we don't need to compare
    is_no_comp <- is.null(remote_pre) # cannot be a comparison remote
    is_no_inspect <- inspect == "none" # no comparison asked for
    is_upload_all <- strategy == "upload-all" # no comparison needed
    is_no_comp || is_no_inspect || is_upload_all
  }

.dest_send_label_get_remotes_get_version_comp_latest <-
  function(inspect,
           remote_pre,
           type,
           label) { # nolint
    version_project <- projr_version_get() |> .version_v_rm()
    version_comp_untrusted <- if (inspect == "file") version_project else NULL
    version_remote <- .remote_get_version_label(
      remote_pre, type, label, "latest"
    ) |>
      .version_v_rm()
    if (!.is_string(version_remote)) {
      version_comp_untrusted
    } else {
      version_remote
    }
  }

.dest_send_label_get_remotes_get_version_comp_archive <- function(cue, # nolint
                                                                  strategy, # nolint
                                                                  label,
                                                                  inspect,
                                                                  remote_pre, # nolint
                                                                  type) {
  # if `inspect` is `file`, `version_comp` can be returned as `version_project`,
  # as we are always going to hash it, so we don't need to
  # return `NULL` to indicate untrusted manifests.
  # otherwise return `NULL`, indicating we trust nothing,
  # if we would not consider the manifest
  version_project <- projr_version_get() |> .version_v_rm()
  version_comp_no_trusted_archive <- NULL
  version_min_acceptable <-
    .manifest_get_version_earliest_match(label, NULL) |>
    .version_v_rm() # nolint
  # if the archives are all out of date, then
  # we require the latest version
  if (version_min_acceptable == version_project) {
    return(version_comp_no_trusted_archive)
  }
  # now, we need to see if an earlier version might work
  version_remote <- .remote_get_version_label(
    remote_pre, type, label, "archive"
  ) |>
    .version_v_rm()
  # earliest version does not work if it's not trusted
  # or none are avaialble (version_remote is NULL),
  # or if the version is too old
  if (!.is_string(version_remote) || version_remote < version_min_acceptable) {
    return(version_comp_no_trusted_archive)
  }
  # at this point, this is simply the latest remote.
  # we trust its information, so we can use it
  version_remote
}

.dest_send_label_get_remotes_comp <- function(type,
                                              id,
                                              label,
                                              structure,
                                              path,
                                              path_append_label,
                                              version,
                                              remote_dest) {
  if (is.null(version)) {
    # if it's `NULL`, then we return remote_dest,
    # as that is the comparison remote, essentially.
    remote_dest
  } else {
    remote_comp <- .remote_get_final(
      type, id, label, structure, path, path_append_label, version
    )
    .dest_send_label_get_remotes_comp_empty(remote_comp, type)
  }
}

.dest_send_label_get_remotes_comp_empty <- function(remote_comp,
                                                    type) {
  if (type != "github") {
    return(remote_comp)
  }
  remote_comp_exists <- .remote_final_check_exists_github_direct(
    remote_comp[["tag"]], remote_comp[["fn"]]
  )
  if (!remote_comp_exists) {
    remote_comp["fn"] <- gsub("\\.zip$", "-empty.zip", remote_comp[["fn"]])
  }
  remote_comp
}

# ==========================================================================
# Get plan
# ==========================================================================

.dest_send_label_get_plan <- function(strategy,
                                      inspect,
                                      version_comp,
                                      type,
                                      label,
                                      remote_pre,
                                      remote_dest,
                                      remote_comp,
                                      cue,
                                      changelog) {
  plan_fn <- .dest_send_label_get_plan_fn(
    strategy, label, inspect, version_comp, remote_comp, type,
    remote_pre, remote_dest
  )

  .dest_send_label_get_plan_action(
    strategy, plan_fn[["fn_source"]], plan_fn[["fn_dest"]],
    plan_fn[["fn_source_extra"]], plan_fn[["fn_dest_extra"]],
    plan_fn[["fn_same"]], plan_fn[["fn_diff"]],
    remote_pre, remote_dest, type, label, version_comp, cue, changelog
  )
}

# --------------------------------------------------------------------------
# How files have changed
# --------------------------------------------------------------------------

.dest_send_label_get_plan_fn <- function(strategy,
                                         label,
                                         inspect,
                                         version_comp,
                                         remote_comp,
                                         type,
                                         remote_pre,
                                         remote_dest) {
  switch(strategy,
    "upload-all" = .dest_send_label_get_plan_fn_upload_all(label),
    "upload-missing" = .dest_send_label_get_plan_fn_upload_missing(
      inspect, version_comp, remote_comp, type, label
    ),
    .dest_send_label_get_plan_fn_sync(
      inspect, version_comp, remote_pre, remote_dest, remote_comp,
      type, label
    )
  )
}

.dest_send_label_get_plan_fn_upload_all <- function(label) {
  # will add whatever is in `fn_source`, nothing else needed
  list("fn_source" = .dest_send_label_get_fn_source(label))
}

.dest_send_label_get_fn_source <- function(label) {
  fn_vec <- .dest_send_label_get_manifest_source(label)[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

.dest_send_label_get_manifest_source <- function(label) {
  .remote_get_manifest("project") |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(projr_version_get())
}

.dest_send_label_get_plan_fn_upload_missing <- function(inspect,
                                                        version_comp,
                                                        remote_comp,
                                                        type,
                                                        label) {
  # add all in `fn_souce_extra`, so need `fn_source` and `fn_dest`,
  # and then we diff them.
  fn_source <- .dest_send_label_get_fn_source(label)
  fn_dest <- .dest_send_label_get_fn_dest(
    inspect, version_comp, remote_comp, type, label
  )
  fn_source_extra <- setdiff(fn_source, fn_dest)
  list("fn_source_extra" = fn_source_extra)
}

.dest_send_label_get_fn_dest <- function(inspect,
                                         version_comp,
                                         type,
                                         remote_comp,
                                         remote_pre,
                                         label) {
  # remote_comp does not exist or we ignore it
  if (is.null(remote_comp) || inspect == "none") {
    return(character(0L))
  }
  # rely on files, either due to untrusted version
  # or because `manifest: file`
  if (is.null(version_comp) || inspect == "file") {
    return(.remote_file_ls(type, remote_comp))
  }
  # we now trust the manifest, so we use that
  manifest_remote <- .remote_get_manifest(type, remote_pre) |>
    .manifest_filter_version(version_comp) |>
    .manifest_filter_label(label)
  fn_vec <- manifest_remote[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

.dest_send_label_get_plan_fn_sync <- function(inspect, # nolint
                                              version_comp,
                                              remote_pre,
                                              remote_dest,
                                              remote_comp,
                                              type,
                                              label) {
  if (inspect == "none") {
    # essentially, upload-all
    return(c("fn_source_extra" = .dest_send_label_get_fn_source(label)))
  }
  manifest_project <- .dest_send_label_get_manifest_source(label)
  manifest_remote <- .dest_send_label_get_manifest_remote(
    version_comp, inspect, remote_comp, type, label, remote_pre
  )
  if (nrow(manifest_remote) == 1 &&
    !is.na(manifest_remote[["fn"]]) &&
    manifest_remote[["fn"]] == "projr-empty") {
    manifest_remote[["fn"]] <- NA_character_
    manifest_remote[["hash"]] <- NA_character_
  }
  .change_get_hash(manifest_remote, manifest_project)
}

.dest_send_label_get_manifest_remote <-
  function(version_comp,
           inspect,
           remote_comp,
           type,
           label,
           remote_pre) {
    # if we don't have a version with a trusted manifest,
    # or we explicity don't want to use the manifest
    if (is.null(version_comp) || inspect == "file") {
      .dest_send_label_get_manifest_remote_hash(
        remote_comp, type, version_comp, label
      )
    } else {
      .dest_send_label_get_manifest_remote_manifest(
        type, remote_pre, label, version_comp
      )
    }
  }

.dest_send_label_get_manifest_remote_hash <-
  function(remote_comp,
           type,
           version_comp,
           label) {
    # if the remote does not exist
    if (is.null(remote_comp)) {
      .empty_tbl_get_manifest(label, version_comp)
    } else {
      .remote_hash(type, remote_comp, version_comp, label)
    }
  }

.dest_send_label_get_manifest_remote_manifest <- # nolint
  function(type, remote_pre, label, version = NULL) {
    version <- if (is.null(version)) projr_version_get() else version
    .remote_get_manifest(type, remote_pre) |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(version)
  }


# --------------------------------------------------------------------------
# Actions to take
# --------------------------------------------------------------------------

.dest_send_label_get_plan_action <- function(strategy,
                                             fn_source,
                                             fn_dest,
                                             fn_source_extra,
                                             fn_dest_extra,
                                             fn_same,
                                             fn_diff,
                                             remote_pre,
                                             remote_dest,
                                             type,
                                             label,
                                             version_comp,
                                             cue,
                                             changelog) {
  plan <- switch(strategy,
    "upload-all" = .dest_send_label_get_plan_action_upload_all(
      fn_source, remote_dest, type, remote_pre, label
    ),
    "upload-missing" = .dest_send_label_get_plan_action_upload_missing(
      fn_source_extra, remote_dest, type, remote_pre, label
    ),
    .dest_send_label_get_plan_action_sync(
      remote_dest, cue, fn_source_extra, type, remote_pre, label,
      version_comp, fn_dest_extra, fn_diff, fn_same, strategy
    )
  )
  plan |>
    append(list("changelog" = changelog))
}

# upload-all
# ---------------------------
.dest_send_label_get_plan_action_upload_all <- function(fn_source, # nolint
                                                        remote_dest,
                                                        type,
                                                        remote_pre,
                                                        label) {
  # will add whatever is in `fn_source`, nothing else needed
  create <- !is.null(remote_dest)
  asterisk_label <- !create # don't asterisk if creating it
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE,
    asterisk_force_add = asterisk_label
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )
  list(
    "fn_add" = fn_source,
    "fn_rm" = character(0L),
    "version" = version_file,
    "manifest" = manifest,
    "purge" = FALSE,
    "create" = create
  )
}

.dest_send_label_get_plan_action_version_file <- function(type, # nolint
                                                          remote_pre,
                                                          label,
                                                          update_label = FALSE, # nolint
                                                          asterisk_force_add = FALSE,
                                                          asterisk_force_rm = FALSE) { # nolint
  version_remote <- .remote_get_version_file(type, remote_pre)
  version_remote <- .version_file_update_project_version(
    version_remote
  )

  if (update_label) {
    # Check if the previous version had an asterisk (was untrusted)
    # Check if we need to force add or remove the asterisk
    use_asterisk <-
      .dest_send_label_get_plan_action_version_file_get_use_asterisk(
        asterisk_force_rm, asterisk_force_add, version_remote, label
      )

    version_remote <- .version_file_update_label_version(
      version_remote, label, use_asterisk
    )
  }
  version_remote
}

.dest_send_label_get_plan_action_version_file_get_use_asterisk <- function(asterisk_force_rm,
                                                                           asterisk_force_add,
                                                                           version_remote,
                                                                           label) {
  if (asterisk_force_rm) {
    # here we're forcibly removing it,
    # regardless of previous status.
    # so, we trust the remote now.
    return(FALSE)
  }
  if (asterisk_force_add) {
    return(TRUE)
  } 
  # here we check if the previous version was trusted
  .dest_send_label_get_plan_action_version_file_check_untrusted(
    version_remote, label
  )

}

.dest_send_label_get_plan_action_version_file_check_untrusted <- function(version_file, # nolint
                                                                          label) { # nolint
  if (length(version_file) == 0L) {
    return(FALSE)
  }
  match_str <- utils::glob2rx(label) |>
    gsub("\\$", "", x = _) |>
    paste0(": ")
  label_regex <- grep(match_str, version_file, value = TRUE)
  if (.is_len_0(label_regex)) {
    return(FALSE)
  }
  # Return TRUE if ends in an asterisk
  grepl("\\*$", label_regex)
}

.dest_send_label_get_plan_action_manifest <- function(type,
                                                      remote_pre,
                                                      label,
                                                      rm_existing = FALSE,
                                                      rm_existing_all = FALSE, # nolint
                                                      rm_adding = FALSE) { # nolint
  manifest_remote <- .remote_get_manifest(type, remote_pre)
  manifest_append <- .manifest_get_add_project(manifest_remote, label)
  # remove any entries in manifest_remote
  # that are in manifest_append (as we are going to overwrite them)
  if (rm_existing) {
    manifest_remote <-
      .dest_send_label_get_plan_action_manifest_rm_existing(
        manifest_remote, label, manifest_append
      )
  }
  if (rm_existing_all) {
    manifest_remote <- manifest_remote |>
      .manifest_filter_out_version_label(
        projr_version_get(), label
      )
  }
  if (rm_adding) {
    manifest_remote_existing <- manifest_remote |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(projr_version_get())
    manifest_append <-
      .dest_send_label_get_plan_action_manifest_rm_existing(
        manifest_append, label, manifest_remote_existing
      )
  }

  manifest_append |>
    .manifest_append_previous_impl(manifest_remote)
}

.dest_send_label_get_plan_action_manifest_rm_existing <-
  function(manifest_remote,
           label,
           manifest_append) {
    manifest_remote_version_label <- manifest_remote |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(projr_version_get())
    rn_vec_init <- rownames(manifest_remote_version_label)
    manifest_remote_version_label <- manifest_remote_version_label[
      !manifest_remote_version_label[["fn"]] %in% manifest_append[["fn"]],
    ]
    manifest_remote_rm <- setdiff(
      rownames(manifest_remote_version_label), rn_vec_init
    )
    manifest_remote <- manifest_remote[
      !rownames(manifest_remote) %in% manifest_remote_rm,
    ]
    manifest_remote
  }

# upload_missing
# ---------------------------
.dest_send_label_get_plan_action_upload_missing <- function(fn_source_extra, # nolint
                                                            remote_dest,
                                                            type,
                                                            remote_pre,
                                                            label) {
  # will add whatever is in `fn_source`, nothing else needed
  create <- !is.null(remote_dest)
  asterisk_label <- !create # don't asterisk if creating it
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE, asterisk_force_add = asterisk_label
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_adding = TRUE
  )
  list(
    "fn_add" = fn_source_extra,
    "fn_rm" = character(0L),
    "manifest" = manifest,
    "version" = version_file,
    create = create,
    purge = FALSE
  )
}

# sync
# ---------------------------

.dest_send_label_get_plan_action_sync <- function(remote_dest,
                                                  cue,
                                                  fn_source_extra,
                                                  type,
                                                  remote_pre,
                                                  label,
                                                  version_comp,
                                                  fn_dest_extra,
                                                  fn_diff,
                                                  fn_same,
                                                  strategy) {
  if (is.null(remote_dest)) {
    .dest_send_label_get_plan_action_no_remote(
      cue, fn_source_extra, type, remote_pre, label,
      version_comp, fn_dest_extra, fn_diff, fn_same, strategy
    )
  } else if (strategy == "sync-purge") {
    .dest_send_label_get_plan_action_purge(
      type, remote_pre, label, version_comp,
      fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy
    )
  } else {
    .assert_has(strategy, "sync-diff")
    .dest_send_label_get_plan_action_diff(
      fn_source_extra, type, remote_pre, label,
      fn_dest_extra, fn_diff, fn_same
    )
  }
}

.dest_send_label_get_plan_action_purge <- function(type, # nolint
                                                   remote_pre,
                                                   label,
                                                   version_comp,
                                                   fn_source_extra,
                                                   fn_dest_extra,
                                                   fn_diff,
                                                   fn_same,
                                                   strategy) {
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE,
    asterisk_force_rm = TRUE # Purge operations should be trusted
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing_all = TRUE
  )
  list(
    fn_add = c(fn_source_extra, fn_diff, fn_same), # nolint
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = FALSE,
    purge = TRUE
  )
}

.dest_send_label_get_plan_action_diff <- function(fn_source_extra, # nolint
                                                  type,
                                                  remote_pre,
                                                  label,
                                                  fn_dest_extra,
                                                  fn_diff,
                                                  fn_same) {
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  if (!is_change) {
    .dest_send_label_get_plan_action_diff_no_change(
      type, remote_pre, label
    )
  } else {
    .dest_send_label_get_plan_action_diff_change(
      type, remote_pre, label, fn_source_extra, fn_diff, fn_same, fn_dest_extra
    )
  }
}

.dest_send_label_get_plan_action_diff_no_change <- function(type, # nolint
                                                            remote_pre,
                                                            label) {
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = FALSE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_adding = TRUE
  )
  list(
    fn_add = character(0L),
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = FALSE,
    purge = FALSE
  )
}

.dest_send_label_get_plan_action_diff_change <- function(type,
                                                         remote_pre,
                                                         label,
                                                         fn_source_extra,
                                                         fn_diff,
                                                         fn_same,
                                                         fn_dest_extra) {
  # For diff operations, preserve untrusted status
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )
  list(
    fn_add = c(fn_source_extra, fn_diff),
    fn_rm = fn_dest_extra,
    version = version_file,
    manifest = manifest,
    create = FALSE,
    purge = FALSE
  )
}

.dest_send_label_get_plan_action_no_remote <- # nolint
  function(cue,
           fn_source_extra,
           type,
           remote_pre,
           label,
           version_comp,
           fn_dest_extra,
           fn_diff,
           fn_same,
           strategy) {
    switch(cue,
      "always" = .dest_send_label_get_plan_action_no_remote_always(
        fn_source_extra, type, remote_pre, label
      ),
      "if-change" = .dest_send_label_get_plan_action_no_remote_if_change(
        version_comp, type, remote_pre, label,
        fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy
      ),
      stop("Unknown cue: ", cue)
    )
  }

.dest_send_label_get_plan_action_no_remote_always <- function(fn_source_extra, # nolint
                                                              type,
                                                              remote_pre,
                                                              label) {
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )
  list(
    fn_add = fn_source_extra,
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = TRUE,
    purge = FALSE
  )
}

.dest_send_label_get_plan_action_no_remote_if_change <- function(version_comp, # nolint
                                                                 type, # nolint
                                                                 remote_pre, # nolint
                                                                 label, # nolint
                                                                 fn_source_extra, # nolint
                                                                 fn_dest_extra, # nolint
                                                                 fn_diff, # nolint
                                                                 fn_same, # nolint
                                                                 strategy) { # nolint
  if (is.null(version_comp)) {
    .dest_send_label_get_plan_action_no_remote_if_change_null(
      type, remote_pre, label, fn_source_extra
    )
  } else {
    .dest_send_label_get_plan_action_no_remote_if_change_non_null(
      fn_source_extra, fn_dest_extra, fn_diff, fn_same,
      type, remote_pre, label, strategy
    )
  }
}

.dest_send_label_get_plan_action_no_remote_if_change_null <- function(type, # nolint
                                                                      remote_pre, # nolint
                                                                      label, # nolint
                                                                      fn_source_extra) { # nolint
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )
  list(
    fn_add = fn_source_extra,
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = TRUE,
    purge = FALSE
  )
}

.dest_send_label_get_plan_action_no_remote_if_change_non_null <- function(fn_source_extra, # nolint
                                                                          fn_dest_extra, # nolint
                                                                          fn_diff, # nolint
                                                                          fn_same, # nolint
                                                                          type, # nolint
                                                                          remote_pre, # nolint
                                                                          label, # nolint
                                                                          strategy) { # nolint
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  if (!is_change) {
    .dest_send_label_get_plan_action_no_remote_if_change_non_null_no_change( # nolint
      type, remote_pre, label, fn_source_extra
    )
  } else {
    .dest_send_label_get_plan_action_no_remote_if_change_non_null_change(
      type, remote_pre, label, fn_source_extra, fn_diff, fn_same, strategy
    )
  }
}

.dest_send_label_get_plan_action_no_remote_if_change_non_null_no_change <- function(type, # nolint
                                                                                    remote_pre, # nolint
                                                                                    label, # nolint
                                                                                    fn_source_extra) { # nolint
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = FALSE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing = FALSE
  )
  list(
    fn_add = character(0L),
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = FALSE,
    purge = FALSE
  )
}

.dest_send_label_get_plan_action_no_remote_if_change_non_null_change <- function(type, # nolint
                                                                                 remote_pre, # nolint
                                                                                 label, # nolint
                                                                                 fn_source_extra, # nolint
                                                                                 fn_diff, # nolint
                                                                                 fn_same, # nolint
                                                                                 strategy) { # nolint
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing_all = TRUE
  )
  list(
    fn_add = c(fn_source_extra, fn_diff, fn_same),
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    create = TRUE,
    purge = strategy == "sync-purge"
  )
}

# ==========================================================================
# Implement plan
# ==========================================================================

.dest_send_label_implement_plan <- function(fn_add,
                                            fn_rm,
                                            version_file,
                                            manifest,
                                            create,
                                            purge,
                                            changelog,
                                            remote_dest,
                                            type,
                                            id,
                                            label,
                                            structure,
                                            path,
                                            path_append_label,
                                            path_dir_local,
                                            remote_pre) {
  if (purge) {
    .remote_file_rm_all(type, remote_dest)
  }
  if (create || type == "github") {
    # will create for OSF and local,
    # but not GitHub, so GitHub remote
    # is only created if there is a file to add
    # this means if the create is TRUE
    # and .is_len_pos(fn_add) is FALSE,
    # then we need to create an empty
    # GitHub remote. Could do that later, I guess?
    remote_dest <- .remote_get_final(
      type, id, label, structure, path, path_append_label, NULL
    )
  }

  if (.is_len_pos(fn_rm)) {
    .remote_file_rm(type, fn_rm, remote_dest)
  }

  if (.is_len_pos(fn_add)) {
    .remote_file_add(type, remote_dest, path_dir_local, fn_add)
    if (type == "github") {
      # ensure that we remove the empty one
      remote_dest_empty <- remote_dest
      remote_dest_empty["fn"] <- gsub(
        "\\.zip$", "-empty.zip", remote_dest_empty[["fn"]]
      )
      if (.remote_final_check_exists_github_direct(
        remote_dest_empty[["tag"]], remote_dest_empty[["fn"]]
      )) {
        .remote_file_rm_all_github(remote_dest_empty)
      }
    }
  }

  # ensure that there is an empty one, if
  # there is non-empty one
  if (type == "github") {
    if (structure == "latest" || create) {
      .dest_send_label_implement_plan_github_empty(
        remote_dest
      )
    }
  }

  # need to use remote_pre and just add an individual file
  .remote_write_manifest(type, remote_pre, manifest)
  .remote_write_version_file(type, remote_pre, version_file)
  if (changelog) {
    .remote_write_changelog(type, remote_pre)
  }
  invisible(TRUE)
}

.dest_send_label_implement_plan_github_empty <- function(remote_dest) {
  # this only happens if the remote doesn't exist,
  # what if we in the end end up with no remote, what then?
  # shouldn't we always have some sort of remote?
  if (!.remote_final_check_exists_github_direct(
    remote_dest[["tag"]], remote_dest[["fn"]]
  )) {
    # create release if it doesn't exist
    if (!.remote_check_exists("github", remote_dest[["tag"]])) {
      .remote_create_github(remote_dest[["tag"]])
    }
    # upload empty asset, if that doesn't already exist
    remote_dest_empty <- remote_dest
    remote_dest_empty[["fn"]] <- gsub(
      "\\.zip$", "-empty.zip", remote_dest_empty[["fn"]]
    )
    path_tmp <- tempdir()
    file.create(file.path(path_tmp, "projr-empty"))
    .remote_file_add(
      "github", remote_dest_empty, path_tmp, "projr-empty"
    )
  }
}
