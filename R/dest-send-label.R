.projr_dest_send_label <- function(label,
                                   title,
                                   type,
                                   output_run,
                                   upload_github,
                                   upload_force) {
  force(title)
  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run) # nolint
  yml_title <- .projr_yml_dest_get_title_complete( # nolint
    title, type, NULL, upload_github, upload_force
  )
  remote_list <- .projr_dest_send_label_get_remotes(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    label, yml_title[["structure"]],
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    yml_title[["send"]][["cue"]]
  )

  plan <- .projr_dest_send_label_get_plan(
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    remote_list[["version_comp"]], type, label,
    remote_list[["remote_pre"]], remote_list[["remote_dest"]],
    remote_list[["remote_comp"]], yml_title[["send"]][["cue"]]
  )

  .projr_dest_send_label_implement_plan(
    plan[["fn_add"]], plan[["fn_rm"]], plan[["version"]],
    plan[["manifest"]], plan[["create"]], plan[["purge"]],
    remote_list[["remote_dest"]], type, yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    path_dir_local, remote_list[["remote_pre"]]
  )

}

# ==========================================================================
# Get remotes
# ==========================================================================

.projr_dest_send_label_get_remotes <- function(type,
                                               id,
                                               path,
                                               path_append_label,
                                               label,
                                               structure,
                                               strategy,
                                               inspect,
                                               cue) {
  remote_pre <- .projr_remote_get_final(
    type, id, label, structure, path, path_append_label, NULL, TRUE
  )
  remote_dest <- .projr_remote_get_final_if_exists(
    type, id, label, structure, path, path_append_label, NULL
  )
  version_comp <- .projr_dest_send_label_get_remotes_get_version_comp(
    remote_pre, type, label, structure, strategy, inspect, cue
  )
  remote_comp <- .projr_dest_send_label_get_remotes_comp(
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

.projr_dest_send_label_get_remotes_get_version_comp <- function(remote_pre, # nolint
                                                                type,
                                                                label,
                                                                structure,
                                                                strategy,
                                                                inspect,
                                                                cue) {

  is_nothing <-
    .projr_dest_send_label_get_remotes_get_version_comp_check_nothing(
      remote_pre, inspect, strategy
    )
  if (is_nothing) {
    return(.projr_dest_send_label_get_remotes_get_version_comp_nothing())
  }
  # here we always compare against the latest remote
  is_latest <-
    .projr_dest_send_label_get_remotes_get_version_comp_check_latest(
      cue, strategy, structure
    )
  if (is_latest) {
    return(.projr_dest_send_label_get_remotes_get_version_comp_latest())
  }
  # now we see if we can compare against an archived remote
  .projr_dest_send_label_get_remotes_get_version_comp_archive(
    cue, strategy, label, inspect, remote_pre, type
  )
}

.projr_dest_send_label_get_remotes_get_version_comp_check_nothing <- # nolint
  function(remote_pre,
           inspect,
           strategy) {
    # in this initial cases, we don't need to compare
    is_no_comp <- is.null(remote_pre) # cannot be a comparison remote
    is_no_inspect <- inspect == "none" # no comparison asked for
    is_upload_all <- strategy == "upload-all" # no comparison needed
    is_no_comp || is_no_inspect || is_upload_all
  }

.projr_dest_send_label_get_remotes_get_version_comp_nothing <- function() { # nolint
  # nothing to compare against, so return `NULL`
  NULL
}

.projr_dest_send_label_get_remotes_get_version_comp_check_latest <- # nolint
  function(cue,
           strategy,
           structure) {
    # if we are always uploading, then we don't need to compare
    # against anything, as we are always going to upload
    # to the latest version
    cue == "always" || strategy == "upload-missing" || structure == "latest"
  }

.projr_dest_send_label_get_remotes_get_version_comp_latest <- function() { # nolint
  projr_version_get() |> .projr_version_v_rm()
}

.projr_dest_send_label_get_remotes_get_version_comp_archive <- function(cue, # nolint
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
  version_project <- projr_version_get() |> .projr_version_v_rm()
  version_comp_no_trusted_archive <- # nolint
    if (inspect == "file") version_project else NULL
  version_min_acceptable <-
    .projr_manifest_get_version_earliest_match(label, NULL) |>
    .projr_version_v_rm() # nolint
  # if the archives are all out of date, then
  # we require the latest version
  if (version_min_acceptable == version_project) {
    return(version_comp_no_trusted_archive)
  }
  # now, we need to see if an earlier version might work
  version_remote <- .projr_remote_get_version_label(
    remote_pre, type, label, "archive"
  ) |>
    .projr_version_v_rm()
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

.projr_dest_send_label_get_remotes_comp <- function(type,
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
    .projr_remote_get_final(
      type, id, label, structure, path, path_append_label, version
    )
  }
}

# ==========================================================================
# Get plan
# ==========================================================================

.projr_dest_send_label_get_plan <- function(strategy,
                                            inspect,
                                            version_comp,
                                            type,
                                            label,
                                            remote_pre,
                                            remote_dest,
                                            remote_comp,
                                            cue) {
  plan_fn <- .projr_dest_send_label_get_plan_fn(
    strategy, label, inspect, version_comp, remote_comp, type,
    remote_pre, remote_dest
  )

  .projr_dest_send_label_get_plan_action(
    strategy, plan_fn[["fn_source"]], plan_fn[["fn_dest"]],
    plan_fn[["fn_source_extra"]], plan_fn[["fn_dest_extra"]],
    plan_fn[["fn_same"]], plan_fn[["fn_diff"]],
    remote_pre, remote_dest, type, label, version_comp, cue
  )

}

# --------------------------------------------------------------------------
# How files have changed
# --------------------------------------------------------------------------

.projr_dest_send_label_get_plan_fn <- function(strategy,
                                               label,
                                               inspect,
                                               version_comp,
                                               remote_comp,
                                               type,
                                               remote_pre,
                                               remote_dest) {
  switch(strategy,
    "upload-all" = .projr_dest_send_label_get_plan_fn_upload_all(label),
    "upload-missing" = .projr_dest_send_label_get_plan_fn_upload_missing(
      inspect, version_comp, remote_comp, type, label
    ),
    .projr_dest_send_label_get_plan_fn_sync(
      inspect, version_comp, remote_pre, remote_dest, remote_comp,
      type, label
    )
  )
}

.projr_dest_send_label_get_plan_fn_upload_all <- function(label) {
  # will add whatever is in `fn_source`, nothing else needed
  list("fn_source" = .projr_dest_send_label_get_fn_source(label))
}

.projr_dest_send_label_get_fn_source <- function(label) {
  fn_vec <- .projr_dest_send_label_get_manifest_source(label)[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

.projr_dest_send_label_get_manifest_source <- function(label) {
  .projr_remote_get_manifest("project") |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(projr_version_get())
}

.projr_dest_send_label_get_plan_fn_upload_missing <- function(inspect,
                                                              version_comp,
                                                              remote_comp,
                                                              type,
                                                              label) {
  # add all in `fn_souce_extra`, so need `fn_source` and `fn_dest`,
  # and then we diff them.
  fn_source <- .projr_dest_send_label_get_fn_source(label)
  fn_dest <- .projr_dest_send_label_get_fn_dest(
    inspect, version_comp, remote_comp, type, label
  )
  fn_source_extra <- setdiff(fn_source, fn_dest)
  list("fn_source_extra" = fn_source_extra)
}

.projr_dest_send_label_get_fn_dest <- function(inspect,
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
    return(.projr_remote_file_ls(type, remote_comp))
  }
  # we now trust the manifest, so we use that
  manifest_remote <- .projr_remote_get_manifest(type, remote_pre) |>
    .projr_manifest_filter_version(version_comp) |>
    .projr_manifest_filter_label(label)
  fn_vec <- manifest_remote[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

.projr_dest_send_label_get_plan_fn_sync <- function(inspect, # nolint
                                                    version_comp,
                                                    remote_pre,
                                                    remote_dest,
                                                    remote_comp,
                                                    type,
                                                    label) {
  if (inspect == "none") {
    # essentially, upload-all
    return(c("fn_source_extra" = .projr_dest_send_label_get_fn_source(label)))
  }
  manifest_project <- .projr_dest_send_label_get_manifest_source(label)
  manifest_remote <- .projr_dest_send_label_get_manifest_remote(
    version_comp, inspect, remote_comp, type, label, remote_pre
  )
  .projr_change_get_hash(manifest_remote, manifest_project)
}

.projr_dest_send_label_get_manifest_remote <-
  function(version_comp,
           inspect,
           remote_comp,
           type,
           label,
           remote_pre) {
    # if we don't have a version with a trusted manifest,
    # or we explicity don't want to use the manifest
    if (is.null(version_comp) || inspect == "file") {
      .projr_dest_send_label_get_manifest_remote_hash(
        remote_comp, type, version_comp, label
      )
    } else {
      .projr_dest_send_label_get_manifest_remote_manifest(
        type, remote_pre, label, version_comp
      )
    }
  }

.projr_dest_send_label_get_manifest_remote_hash <-
  function(remote_comp,
           type,
           version_comp,
           label) {
    # if the remote does not exist
    if (is.null(remote_comp)) {
      .projr_empty_tbl_get_manifest(label, version_comp)
    } else {
      .projr_remote_hash(type, remote_comp, version_comp, label)
    }
  }

.projr_dest_send_label_get_manifest_remote_manifest <- # nolint
  function(type, remote_pre, label, version = NULL) {
    version <- if (is.null(version)) projr_version_get() else version
    .projr_remote_get_manifest(type, remote_pre) |>
      .projr_manifest_filter_label(label) |>
      .projr_manifest_filter_version(version)
  }


# --------------------------------------------------------------------------
# Actions to take
# --------------------------------------------------------------------------

.projr_dest_send_label_get_plan_action <- function(strategy,
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
                                                   cue) {
  switch(strategy,
    "upload-all" = .projr_dest_send_label_get_plan_action_upload_all(
      fn_source, remote_dest, type, remote_pre, label
    ),
    "upload-missing" = .projr_dest_send_label_get_plan_action_upload_missing(
      fn_source_extra, remote_dest, type, remote_pre, label
    ),
    .projr_dest_send_label_get_plan_action_sync(
      remote_dest, cue, fn_source_extra, type, remote_pre, label,
      version_comp, fn_dest_extra, fn_diff, fn_same, strategy
    )
  )
}

# upload-all
# ---------------------------
.projr_dest_send_label_get_plan_action_upload_all <- function(fn_source, # nolint
                                                              remote_dest,
                                                              type,
                                                              remote_pre,
                                                              label) {
  # will add whatever is in `fn_source`, nothing else needed
  create <- !is.null(remote_dest)
  asterisk_label <- !create # don't asterisk if creating it
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE,
    asterisk_label = asterisk_label
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_version_file <- function(type, # nolint
                                                                remote_pre,
                                                                label,
                                                                update_label = FALSE, # nolint
                                                                asterisk_label = FALSE) { # nolint
  version_remote <- .projr_remote_get_version_file(type, remote_pre)
  version_remote <- .projr_version_file_update_project_version(
    version_remote
  )
  if (update_label || asterisk_label) {
    version_remote <- .projr_version_file_update_label_version(
      version_remote, label, asterisk_label
    )
  }
  version_remote
}

.projr_dest_send_label_get_plan_action_manifest <- function(type,
                                                            remote_pre,
                                                            label,
                                                            rm_existing = FALSE,
                                                            rm_existing_all = FALSE, # nolint
                                                            rm_adding = FALSE) { # nolint
  manifest_remote <- .projr_remote_get_manifest(type, remote_pre)
  manifest_append <- .projr_manifest_get_add_project(manifest_remote, label)
  # remove any entries in manifest_remote
  # that are in manifest_append (as we are going to overwrite them)
  if (rm_existing) {
    manifest_remote <-
      .projr_dest_send_label_get_plan_action_manifest_rm_existing(
        manifest_remote, label, manifest_append
      )
  }
  if (rm_existing_all) {
    manifest_remote <- manifest_remote |>
      .projr_manifest_filter_out_version_label(
        projr_version_get(), label
      )
  }
  if (rm_adding) {
    manifest_remote_existing <- manifest_remote |>
      .projr_manifest_filter_label(label) |>
      .projr_manifest_filter_version(projr_version_get())
    manifest_append <-
      .projr_dest_send_label_get_plan_action_manifest_rm_existing(
        manifest_append, label, manifest_remote_existing
      )
  }

  manifest_append |>
    .projr_manifest_append_previous_actual(manifest_remote)

}

.projr_dest_send_label_get_plan_action_manifest_rm_existing <-
  function(manifest_remote,
           label,
           manifest_append) {
    manifest_remote_version_label <- manifest_remote |>
      .projr_manifest_filter_label(label) |>
      .projr_manifest_filter_version(projr_version_get())
    rn_vec_init <- rownames(manifest_remote_version_label)
    manifest_remote_version_label <- manifest_remote_version_label[
      !manifest_remote_version_label[["fn"]] %in% manifest_append[["fn"]]
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
.projr_dest_send_label_get_plan_action_upload_missing <- function(fn_source_extra, # nolint
                                                                  remote_dest,
                                                                  type,
                                                                  remote_pre,
                                                                  label) {
  # will add whatever is in `fn_source`, nothing else needed
  create <- !is.null(remote_dest)
  asterisk_label <- !create # don't asterisk if creating it
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE, asterisk_label = asterisk_label
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_sync <- function(remote_dest,
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
    .projr_dest_send_label_get_plan_action_no_remote(
      cue, fn_source_extra, type, remote_pre, label,
      version_comp, fn_dest_extra, fn_diff, fn_same, strategy
    )
  } else if (strategy == "sync-purge") {
    .projr_dest_send_label_get_plan_action_purge(
      type, remote_pre, label, version_comp,
      fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy
    )
  } else {
    .assert_has(strategy, "sync-diff")
    .projr_dest_send_label_get_plan_action_diff(
      fn_source_extra, type, remote_pre, label,
      fn_dest_extra, fn_diff, fn_same
    )
  }
}

.projr_dest_send_label_get_plan_action_purge <- function(type, # nolint
                                                         remote_pre,
                                                         label,
                                                         version_comp,
                                                         fn_source_extra,
                                                         fn_dest_extra,
                                                         fn_diff,
                                                         fn_same,
                                                         strategy) {
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_diff <- function(fn_source_extra, # nolint
                                                        type,
                                                        remote_pre,
                                                        label,
                                                        fn_dest_extra,
                                                        fn_diff,
                                                        fn_same) {
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  if (!is_change) {
    .projr_dest_send_label_get_plan_action_diff_no_change(
      type, remote_pre, label
    )
  } else {
    .projr_dest_send_label_get_plan_action_diff_change(
      type, remote_pre, label, fn_source_extra, fn_diff, fn_same, fn_dest_extra
    )
  }
}

.projr_dest_send_label_get_plan_action_diff_no_change <- function(type, # nolint
                                                                  remote_pre,
                                                                  label) {
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = FALSE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_diff_change <- function(type,
                                                               remote_pre,
                                                               label,
                                                               fn_source_extra,
                                                               fn_diff,
                                                               fn_same,
                                                               fn_dest_extra) {
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_no_remote <- # nolint
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
      "always" = .projr_dest_send_label_get_plan_action_no_remote_always(
        fn_source_extra, type, remote_pre, label
      ),
      "if-change" = .projr_dest_send_label_get_plan_action_no_remote_if_change(
        version_comp, type, remote_pre, label,
        fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy
      ),
      stop("Unknown cue: ", cue)
    )
  }

.projr_dest_send_label_get_plan_action_no_remote_always <- function(fn_source_extra, # nolint
                                                                    type,
                                                                    remote_pre,
                                                                    label) {
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_no_remote_if_change <- function(version_comp, # nolint
                                                                       type, # nolint
                                                                       remote_pre, # nolint
                                                                       label, # nolint
                                                                       fn_source_extra, # nolint
                                                                       fn_dest_extra, # nolint
                                                                       fn_diff, # nolint
                                                                       fn_same, # nolint
                                                                       strategy) { # nolint
  if (is.null(version_comp)) {
    .projr_dest_send_label_get_plan_action_no_remote_if_change_null(
      type, remote_pre, label, fn_source_extra
    )
  } else {
    .projr_dest_send_label_get_plan_action_no_remote_if_change_non_null(
      fn_source_extra, fn_dest_extra, fn_diff, fn_same,
      type, remote_pre, label, strategy
    )
  }
}

.projr_dest_send_label_get_plan_action_no_remote_if_change_null <- function(type, # nolint
                                                                            remote_pre, # nolint
                                                                            label, # nolint
                                                                            fn_source_extra) { # nolint
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_no_remote_if_change_non_null <- function(fn_source_extra, # nolint
                                                                                fn_dest_extra, # nolint
                                                                                fn_diff, # nolint
                                                                                fn_same, # nolint
                                                                                type, # nolint
                                                                                remote_pre, # nolint
                                                                                label, # nolint
                                                                                strategy) { # nolint
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  if (!is_change) {
    .projr_dest_send_label_get_plan_action_no_remote_if_change_non_null_no_change( # nolint
      type, remote_pre, label, fn_source_extra
    )
  } else {
    .projr_dest_send_label_get_plan_action_no_remote_if_change_non_null_change(
      type, remote_pre, label, fn_source_extra, fn_diff, fn_same, strategy
    )
  }
}

.projr_dest_send_label_get_plan_action_no_remote_if_change_non_null_no_change <- function(type,# nolint
                                                                                          remote_pre,# nolint
                                                                                          label,# nolint
                                                                                          fn_source_extra) {# nolint
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = FALSE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_get_plan_action_no_remote_if_change_non_null_change <- function(type, # nolint
                                                                                       remote_pre, # nolint
                                                                                       label, # nolint
                                                                                       fn_source_extra, # nolint
                                                                                       fn_diff, # nolint
                                                                                       fn_same, # nolint
                                                                                       strategy) { # nolint
  version_file <- .projr_dest_send_label_get_plan_action_version_file(
    type, remote_pre, label, update_label = TRUE
  )
  manifest <- .projr_dest_send_label_get_plan_action_manifest(
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

.projr_dest_send_label_implement_plan <- function(fn_add,
                                                  fn_rm,
                                                  version_file,
                                                  manifest,
                                                  create,
                                                  purge,
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
    .projr_remote_file_rm_all(type, remote_dest)
  }
  if (create) {
    remote_dest <- .projr_remote_get_final(
      type, id, label, structure, path, path_append_label, NULL
    )
  }

  if (.is_len_pos(fn_rm)) {
    .projr_remote_file_rm(type, fn_rm, remote_dest)
  }

  if (.is_len_pos(fn_add)) {
    .projr_remote_file_add(type, remote_dest, path_dir_local, fn_add)
  }

  # need to use remote_pre and just add an individual file
  .projr_remote_write_manifest(type, remote_pre, manifest)
  .projr_remote_write_version_file(type, remote_pre, version_file)
  invisible(TRUE)
}