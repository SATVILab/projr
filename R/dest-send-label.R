#' @title Process a single destination label
#' @description Coordinates planning and execution logic required to send a
#'   single content label to a destination title for the specified remote type.
#'   This function resolves remote metadata, builds the upload plan, and applies
#'   the plan while emitting detailed debug output when requested.
#' @param label Character scalar giving the directory label (e.g. output, docs)
#'   whose files should be uploaded.
#' @param title Destination title pulled from `_projr.yml` that describes the
#'   remote block currently being processed.
#' @param type Remote type identifier (`local`, `github`, or `osf`).
#' @param output_run Logical flag indicating whether the content directory is in
#'   the safe cache or the unsafe project root (controls path selection).
#' @param archive_type Logical/character directive returned by
#'   `.dest_send_get_archive_type()` describing whether this title is an archive
#'   destination and, if so, which content labels are relevant.
#' @param always_archive Logical flag propagated from `.dest_send_get_always_archive()`
#'   that forces archive creation even when `_projr.yml` does not request it.
#' @param changelog Logical flag indicating whether a changelog should be
#'   written back to the remote after uploads.
#' @param output_level Character scalar controlling CLI verbosity ("none",
#'   "std", or "debug").
#' @return Invisibly returns `TRUE` when an upload plan is executed or `FALSE`
#'   when the cue prevents sending.
#' @keywords internal
#' @noRd
.dest_send_label <- function(label,
                             title,
                             type,
                             output_run,
                             archive_type,
                             always_archive,
                             changelog,
                             output_level = "std") {
  force(title)

  .cli_debug(
    "Content '{label}': Starting processing for destination '{title}' (type: {type})", # nolint
    output_level = output_level
  )

  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run) # nolint

  .cli_debug(
    "Content '{label}': Local path is {path_dir_local}",
    output_level = output_level
  )

  yml_title <- .yml_dest_get_title_complete( # nolint
    title, type, NULL, archive_type, always_archive
  )
  remote_list <- .dsl_get_remotes(
    type, yml_title[["id"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    label, yml_title[["structure"]],
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    yml_title[["send"]][["cue"]]
  )

  .cli_debug(
    "Content '{label}': Remote configuration - id: {yml_title[['id']]}, structure: {yml_title[['structure']]}, strategy: {yml_title[['send']][['strategy']]}, inspect: {yml_title[['send']][['inspect']]}", # nolint
    output_level = output_level
  )

  if (!is.null(remote_list[["remote_dest"]])) {
    .cli_debug(
      "Content '{label}': Remote destination exists at path: {remote_list[['remote_dest']][['path']]}", # nolint
      output_level = output_level
    )
  } else {
    .cli_debug(
      "Content '{label}': Remote destination does not exist yet (will be created)", # nolint
      output_level = output_level
    )
  }

  plan <- .dsl_get_plan(
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    remote_list[["version_comp"]], type, label,
    remote_list[["remote_pre"]],
    remote_list[["remote_dest_full"]],
    remote_list[["remote_dest_empty"]],
    remote_list[["remote_comp"]], yml_title[["send"]][["cue"]],
    changelog
  )

  .cli_debug(
    "Content '{label}': Upload plan - {length(plan[['fn_add']])} file(s) to add, {length(plan[['fn_rm']])} file(s) to remove, create: {plan[['create']]}, purge: {plan[['purge']]}", # nolint
    output_level = output_level
  )

  if (length(plan[["fn_add"]]) > 0) {
    .cli_debug(
      "Content '{label}': Files to add: {paste(head(plan[['fn_add']], 10), collapse = ', ')}{if (length(plan[['fn_add']]) > 10) '...' else ''}", # nolint
      output_level = output_level
    )
  } else {
    .cli_debug(
      "Content '{label}': No files to add",
      output_level = output_level
    )
  }

  if (length(plan[["fn_rm"]]) > 0) {
    .cli_debug(
      "Content '{label}': Files to remove: {paste(head(plan[['fn_rm']], 10), collapse = ', ')}{if (length(plan[['fn_rm']]) > 10) '...' else ''}", # nolint
      output_level = output_level
    )
  } else {
    .cli_debug(
      "Content '{label}': No files to remove",
      output_level = output_level
    )
  }

  .dsl_implement_plan(
    plan[["fn_add"]], plan[["fn_rm"]], plan[["version"]],
    plan[["manifest"]], plan[["purge"]],
    plan[["is_remote_dest_empty"]], plan[["ensure_remote_dest_exists"]],
    plan[["changelog"]],
    remote_list[["remote_dest_full"]],
    remote_list[["remote_dest_empty"]],
    type, yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    path_dir_local, remote_list[["remote_pre"]],
    yml_title[["send"]][["cue"]],
    output_level
  )

  .cli_debug(
    "Content '{label}': Processing completed successfully",
    output_level = output_level
  )
}

# ==========================================================================
# Get remotes
# ==========================================================================

#' @title Resolve remotes for a destination label
#'
#' @description Builds the remote objects needed to evaluate send plans by
#'   retrieving the pre-remote, destination remote, comparison target, and
#'   comparison version identifier.
#'
#' @param type Remote type identifier (`local`, `github`, or `osf`).
#' @param id Remote id from `_projr.yml` (e.g. filesystem path or GitHub tag).
#' @param path Optional relative path within the remote definition.
#' @param path_append_label Logical flag indicating whether the label should be
#'   appended to the path when composing remote structures.
#' @param label Directory label being processed.
#' @param structure Remote structure (`latest` or `archive`).
#' @param strategy Send strategy requested for the destination (sync, upload,
#'   etc.).
#' @param inspect Inspection mode describing how remote state should be
#'   inspected (`manifest`, `file`, or `none`).
#' @param cue Cue describing when to send (always, if-change, etc.).
#'
#' @details
#' - remote_dest uses the latest version but assumes non-empty, and so
#'   is tweaked later on if it needs to be empty.
#' - remote_dest is NULL if it does not exist, which happens
#'   for `local` structure remotes if this is the first time
#'   we're uploading to there or the previous remote was empty
#'   (and it's GitHub),
#' - `remote_dest` is where we would upload to, if it already existed.
#'   So, it does often for latest remotes but not usually for `archive`
#'   remotes.
#'   or usually always for `archive` remotes.
#' - `version_comp` is critical, in that it tells us whether there is
#'   a trusted remote. If it's NULL, then there is no trusted remote, either
#'   because it does not eixst or because we don't trust its manifest if
#'   we're using manifest-based uploads.
#'   What this means is that if we are not inspecting, then
#'   `version_comp` is NULL, and we will upload everything.
#'   What this means then is that if the `version_comp` is NULL, then
#'   for `archive` remotes we will have to upload and the version will
#'   be trusted.
#'   For latest remotes, we will lose trusted remote status if the
#'   remote already exists, but if it's being created for the first time
#'   the remote will be trusted.
#' -
#' @return List containing `remote_pre`, `remote_dest`, `remote_comp`, and
#'   `version_comp` entries.
#' @keywords internal
#' @noRd
.dsl_get_remotes <- function(type,
                             id,
                             path,
                             path_append_label,
                             label,
                             structure,
                             strategy,
                             inspect,
                             cue) {
  # Useful for finding out information about final remotes,
  # such as their contents or version (via manifest.csv
  # or VERSION files) or, in the case of archive remotes,
  # what versions are available.
  remote_pre <- .remote_final_get(
    type, id, label, structure, path, path_append_label,
    NULL, TRUE, FALSE
  )
  # Mainly a check on whether the remote exists or not,
  # but also useful for investigating the remote.
  remote_dest_full <- .remote_final_get_if_exists(
    type, id, label, structure, path, path_append_label,
    projr_version_get(), FALSE, FALSE
  )
  remote_dest_empty <- .remote_final_get_if_exists(
    type, id, label, structure, path, path_append_label,
    projr_version_get(), FALSE, TRUE
  )
  # Version on the remote to be compared against.
  # NULL if there is no sufficiently up-to-date and
  # trusted version. By sufficiently up-to-date we mean
  # that locally we know that there were no changes
  # since that version.
  # By trusted, we mean that the contents of the
  # version to be compared against are trusted.
  # For file-inspection, they're always trusted.
  # For manifest remotes, they're only trusted
  # if the uploads have been performed in a trusted manner
  # (typically, either sync-purge or sync-diff with
  # inspect set to either manifest or file).
  version_comp <- .dsl_gr_get_version_comp(
    remote_pre, remote_dest_full, remote_dest_empty,
    type, label, structure, strategy, inspect, cue
  )
  # if version_comp is not NULL, this is the remote to
  # compare against.
  remote_comp <- .dsl_gr_comp(
    type, id, label, structure, path, path_append_label, version_comp,
    remote_dest_full, remote_dest_empty
  )
  list(
    "remote_pre" = remote_pre,
    "remote_dest_full" = remote_dest_full,
    "remote_dest_empty" = remote_dest_empty,
    "remote_comp" = remote_comp,
    "version_comp" = version_comp
  )
}

#' @title Determine comparison version for a destination
#' @description Calculates which remote version (if any) can be trusted for
#'   comparisons based on structure, inspection mode, strategy, and cue.
#' @inheritParams .dsl_get_remotes
#' @param remote_pre Remote pre-object produced by `.remote_final_get()`.
#' @return Character version string (without suffix) or `NULL` when no trusted
#'   comparison exists.
#'
#' @details
#'
#' If no inspection is done, then there is no remote to compare against,
#' and so we return NULL.
#'
#' If the structure is `latest`, then for `file` inspections
#' we always compare against the latest verison of the remote,
#' and for `manifest` inspections we need the manifest to
#' be trustworthy to use them.
#'
#' If the structure is `archive`, then for `file` inspections
#' we accept whatever the most recent remote is that is
#' above the minimum accepted version.
#' For `manifest` inpsections, however, the remote needs
#' to be sufficiently frequent and have valid manifest.
#'
#' If
#'
#' @keywords internal
#' @noRd
.dsl_gr_get_version_comp <- function(remote_pre,
                                     remote_dest_full,
                                     remote_dest_empty,
                                     type,
                                     label,
                                     structure,
                                     strategy,
                                     inspect,
                                     cue) {
  # exit early with NULL when no comparison remote is needed,
  # usually because the upload strategy does not involve
  # inspecting the remote at all
  is_nothing <-
    .dsl_gr_gvc_check_nothing(
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
  switch(structure,
    "latest" = .dsl_gr_gvc_latest(
      inspect, remote_pre, remote_dest_full, remote_dest_empty,
      type, label
    ),
    "archive" = .dsl_gr_gvc_archive(
      cue, strategy, label, inspect, remote_pre, remote_dest_full,
      remote_dest_empty, type
    )
  )
}

#' @title Check whether comparison logic can be skipped
#' @description Evaluates early-exit conditions that make remote comparisons
#'   unnecessary (no remote, no inspection, or upload-all strategy).
#' @inheritParams .dsl_gr_get_version_comp
#' @return Logical flag indicating whether comparison work should be skipped.
#' @keywords internal
#' @noRd
.dsl_gr_gvc_check_nothing <- function(remote_pre,
                                      inspect,
                                      strategy) {
  # in this initial cases, we don't need to compare
  is_no_comp <- is.null(remote_pre) # cannot be a comparison remote
  is_no_inspect <- inspect == "none" # no comparison asked for
  is_upload_all <- strategy == "upload-all" # no comparison needed
  is_no_comp || is_no_inspect || is_upload_all
}

#' @title Resolve comparison version for latest remotes
#' @description Determines which version string should be used when comparing
#'   against a `latest` remote structure, honoring inspection mode and manifest
#'   trust rules.
#' @inheritParams .dsl_gr_get_version_comp
#'
#' @details
#' The idea is that when `NULL` is returned, no comparison
#' (either file-based or manifest-based) will take place,
#' whereas if a version string is returned, that version
#' is to be used for comparisons.
#'
#' Note that this flow assumes that the `remote_dest` does not exist
#' for `archive` structure remotes.
#'
#' @return Character version string or `NULL` when no trusted version is
#'   available.
#'
#' @keywords internal
#' @noRd
.dsl_gr_gvc_latest <- function(inspect,
                               remote_pre,
                               remote_dest_full,
                               remote_dest_empty,
                               type,
                               label) {
  # if the remote dest does not exist,
  # then there is nothing to compare against.
  remote_exists_either <- !is.null(remote_dest_full) ||
    !is.null(remote_dest_empty)
  if (!remote_exists_either) {
    return(NULL)
  }
  switch(inspect,
    "file" = .dsl_gr_gvc_latest_file(),
    .dsl_gr_gvc_latest_manifest(
      remote_pre, type, label
    )
  )
}

.dsl_gr_gvc_latest_file <- function() {
  # final remotes for `latest` structure do not have the version embedded,
  # so the version to compare against is irrelevant.
  # But we don't return NULL because we do want to compare.
  projr_version_get() |> .version_v_rm()
}

.dsl_gr_gvc_latest_manifest <- function(remote_pre,
                                        type,
                                        label) {
  # will be character(0L) if:
  # - VERSION file does not exist or is corrupt
  # - manifest on remote does not match manifest locally
  #   for the relevant versions
  version_remote_raw <- .remote_get_version_latest_label(
    remote_pre, type, label, "latest"
  )
  # not valid if no string returned
  if (!.is_string(version_remote_raw)) {
    return(NULL)
  }
  version_remote_raw |>
    .version_v_rm()
}

#' @title Resolve comparison version for archive remotes
#' @description Selects the remote archive version that can be trusted during
#'   comparisons, accounting for cues, strategies, manifests, and acceptable
#'   version ranges.
#' @inheritParams .dsl_gr_get_version_comp
#' @return Trusted version string or `NULL` if no archive is suitable.
#' @keywords internal
#' @noRd
.dsl_gr_gvc_archive <- function(cue, # nolint
                                strategy, # nolint
                                label,
                                inspect,
                                remote_pre, # nolint
                                remote_dest_full,
                                remote_dest_empty,
                                type) {
  # against the latest version to see what
  # we should upload, as now we're not interested
  # in old remotes, but the latest one
  # does upload-missing become upload-all here?
  # yes, essentially, with cue: always, as we're
  # not going to adjust old versions so it's basically
  # just upload all.
  # unless it exists already, in which case
  # we're obviously uploading against it again and so we need
  # to check its contents in the usual manner.
  remote_exists_either <- !is.null(remote_dest_full) ||
    !is.null(remote_dest_empty)
  if (cue == "always" && !remote_exists_either) {
    return(NULL)
  }
  # if `inspect` is `file`, `version_comp` can be returned as `version_project`,
  # as we are always going to hash it, so we don't need to
  # return `NULL` to indicate untrusted manifests.
  # otherwise return `NULL`, indicating we trust nothing,
  # if we would not consider the manifest
  version_project <- projr_version_get() |> .version_v_rm()
  # what version we return if we don't have a
  # sufficiently up-to-date and trusted version.
  version_comp_no_trusted_archive <- NULL
  version_min_acceptable <-
    .manifest_get_version_earliest_match(label, NULL) |>
    .version_v_rm() # nolint
  # if the archives are all out of date
  # (there has been a change in the last build), then
  # we require the latest version
  if (version_min_acceptable == version_project) {
    # if the latest remote doesn't exist,
    # then we have nothing to compare against
    # this seems impossible for archive remotes, but okay.
    if (!remote_exists_either) {
      return(version_comp_no_trusted_archive)
      # if it does exist, and we're comparing against files,
      # then we compare against the latest version
      # (which we know exists already).
    } else if (inspect == "file") {
      return(version_project)
    } else if (inspect == "manifest") {
      # for manifest remotes, we carry on,
      # as we need to see whether the manifest
      # is trustworthy or not.
    }
  }
  # now, we need to see if an earlier version might work
  version_remote_raw <- .remote_get_version_latest_label(
    remote_pre, type, label, "archive"
  )
  # Check if version_remote_raw is empty before calling .version_v_rm()
  # we return NULL as we don't have a trusted archive.
  if (.is_len_0(version_remote_raw)) {
    return(version_comp_no_trusted_archive)
  }
  version_remote <- version_remote_raw |> .version_v_rm()
  # earliest version does not work if it's not trusted
  # or none are available (version_remote is NULL),
  # or if the version is too old
  if (!.is_string(version_remote) ||
        .version_is_earlier(version_remote, version_min_acceptable)) {
    return(version_comp_no_trusted_archive)
  }
  # now we've uploaded past the version we're at now.
  # this shouldn't happen (I should really check in advance),
  # but let's check.
  if (.version_is_earlier(projr_version_get(), version_remote)) {
    # should not happen, but just in case. We could force
    # the remote_get_version_label to return the
    # latest remote earlier than the current one, but
    # this is something that shouldn't happen, so
    # we'll leave it for now.
    return(version_comp_no_trusted_archive)
  }
  # at this point, we have the latest
  # version of the project on the remote.
  version_remote
}

#' @title Build comparison remote handle
#' @description Returns the remote that should be used for comparisons, falling
#'   back to the destination remote if no explicit version is supplied.
#' @inheritParams .dsl_get_remotes
#' @param version Optional version string to retrieve when comparing archives.
#' @param remote_dest Destination remote returned earlier (may be `NULL`).
#' @return Remote object suitable for comparison.
#' @keywords internal
#' @noRd
.dsl_gr_comp <- function(type,
                         id,
                         label,
                         structure,
                         path,
                         path_append_label,
                         version,
                         remote_dest_full,
                         remote_dest_empty) {
  remote_dest_full_priority <- if (!is.null(remote_dest_full)) {
    remote_dest_full
  } else {
    remote_dest_empty
  }
  if (is.null(version)) {
    # if it's `NULL`, then we return remote_dest,
    # as that is the comparison remote, essentially.
    # only matters when inspecting.
    return(remote_dest_full_priority)
  }

  switch(structure,
    # always the same regardless of version for `latest`
    "latest" = remote_dest_full_priority,
    # need to get the specific version remote
    # for the archive version request
    "archive" = {
      remote_comp_full <- .remote_final_get_if_exists(
        type, id, label, structure, path, path_append_label,
        version, FALSE, FALSE
      )
      if (!is.null(remote_comp_full)) {
        return(remote_comp_full)
      }
      .remote_final_get_if_exists(
        type, id, label, structure, path, path_append_label,
        version, TRUE, FALSE
      )
    }
  )
}

# ==========================================================================
# Get plan
# ==========================================================================

#' @title Construct send plan for a destination label
#' @description Determines which files must be added or removed and what metadata
#'   updates are required by combining strategy, inspection mode, and comparison
#'   context.
#' @param strategy Send strategy string.
#' @param inspect Inspection mode.
#' @param version_comp Comparison version string or `NULL`.
#' @param type Remote type identifier.
#' @param label Directory label.
#' @param remote_pre Remote pre-object.
#' @param remote_dest Existing destination remote (if any).
#' @param remote_comp Remote comparison target.
#' @param cue Cue describing when the destination should send.
#' @param changelog Logical indicating whether a changelog needs to be written.
#' @return List describing filenames to add/remove plus metadata actions.
#' @keywords internal
#' @noRd
.dsl_get_plan <- function(strategy,
                          inspect,
                          version_comp,
                          type,
                          label,
                          remote_pre,
                          remote_dest_full,
                          remote_dest_empty,
                          remote_comp,
                          cue,
                          changelog) {
  plan_fn <- .dsl_get_plan_fn(
    strategy, label, inspect, version_comp, remote_comp, type,
    remote_pre, remote_dest_full, remote_dest_empty
  )

  .dsl_get_plan_action(
    strategy, plan_fn[["fn_source"]], plan_fn[["fn_dest"]],
    plan_fn[["fn_source_extra"]], plan_fn[["fn_dest_extra"]],
    plan_fn[["fn_same"]], plan_fn[["fn_diff"]],
    remote_pre, remote_dest_full, remote_dest_empty,
    type, label, version_comp, cue, inspect, changelog
  )
}

# --------------------------------------------------------------------------
# How files have changed
# --------------------------------------------------------------------------

#' @title Determine file state for send plan
#' @description Builds filename vectors used by plan execution based on the
#'   chosen strategy (upload-all, upload-missing, sync).
#' @inheritParams .dsl_get_plan
#' @return Named list containing source/destination filename vectors and diff
#'   sets required for subsequent planning.
#' @keywords internal
#' @noRd
.dsl_get_plan_fn <- function(strategy,
                             label,
                             inspect,
                             version_comp,
                             remote_comp,
                             type,
                             remote_pre,
                             remote_dest_full,
                             remote_dest_empty) {
  switch(strategy,
    "upload-all" = .dsl_gpfn_upload_all(label),
    "upload-missing" = .dsl_gpfn_upload_missing(
      inspect, version_comp, remote_comp, remote_pre, type, label
    ),
    .dsl_gpfn_sync(
      inspect, version_comp, remote_pre, remote_dest_full,
      remote_dest_empty, remote_comp,
      type, label
    )
  )
}

#' @title File listing helper for upload-all
#' @description Returns every file tracked for the label so they can all be
#'   added to the destination regardless of remote state.
#' @inheritParams .dsl_get_plan_fn
#' @return Character vector of filenames sourced from the project manifest.
#' @keywords internal
#' @noRd
.dsl_gpfn_upload_all <- function(label) {
  # will add whatever is in `fn_source`, nothing else needed
  list("fn_source" = .dsl_get_fn_source(label))
}

#' @title Retrieve project-side filenames for a label
#' @description Pulls the file list for the current project version from the
#'   local manifest for the supplied label.
#' @inheritParams .dsl_get_plan_fn
#' @return Character vector of filenames present locally.
#' @keywords internal
#' @noRd
.dsl_get_fn_source <- function(label) {
  fn_vec <- .dsl_get_manifest_source(label)[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

#' @title Retrieve manifest rows for a label
#' @description Filters the project manifest down to the current version and the
#'   requested label to support downstream comparisons.
#' @inheritParams .dsl_get_plan_fn
#' @return Tibble/data.frame of manifest entries.
#' @keywords internal
#' @noRd
.dsl_get_manifest_source <- function(label) {
  .remote_get_manifest("project") |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(projr_version_get())
}

#' @title File listing helper for upload-missing
#' @description Determines which files exist locally but not remotely so they
#'   can be uploaded without touching existing remote content.
#' @inheritParams .dsl_get_plan_fn
#' @return Named list containing `fn_source_extra` with files missing remotely.
#' @keywords internal
#' @noRd
.dsl_gpfn_upload_missing <- function(inspect,
                                     version_comp,
                                     remote_comp,
                                     remote_pre,
                                     type,
                                     label) {
  # add all in `fn_souce_extra`, so need `fn_source` and `fn_dest`,
  # and then we diff them.
  fn_source <- .dsl_get_fn_source(label)
  fn_dest <- .dsl_get_fn_dest(
    inspect, version_comp, type, remote_comp, remote_pre, label
  )
  fn_source_extra <- setdiff(fn_source, fn_dest)
  list("fn_source_extra" = fn_source_extra)
}

#' @title Retrieve destination filenames for comparison
#' @description Returns the file listing that represents remote state, using
#'   hashes, manifests, or direct listings based on inspection choices.
#' @inheritParams .dsl_get_plan_fn
#' @return Character vector of filenames representing the remote state.
#' @keywords internal
#' @noRd
.dsl_get_fn_dest <- function(inspect,
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

#' @title File comparison helper for sync strategies
#' @description Builds full change tables between local and remote manifests to
#'   support `sync-diff` and `sync-purge` strategies.
#' @inheritParams .dsl_get_plan_fn
#' @return Change table as produced by `.change_get_hash()` describing adds,
#'   deletes, and matches.
#' @keywords internal
#' @noRd
.dsl_gpfn_sync <- function(inspect, # nolint
                           version_comp,
                           remote_pre,
                           remote_dest_full,
                           remote_dest_empty,
                           remote_comp,
                           type,
                           label) {
  if (inspect == "none") {
    # essentially, upload-all
    return(list("fn_source_extra" = .dsl_get_fn_source(label)))
  }
  manifest_project <- .dsl_get_manifest_source(label)
  manifest_remote <- .dsl_get_manifest_remote(
    version_comp, inspect, remote_comp, type, label, remote_pre
  )
  # manifest is empty, so we use a placeholder
  # to avoid issues with later processing, as
  # e.g. we don't want projr-empty to be
  # considered a real file.
  if (nrow(manifest_remote) == 1 &&
    !is.na(manifest_remote[["fn"]]) &&
    manifest_remote[["fn"]] == "projr-empty") {
    manifest_remote[["fn"]] <- NA_character_
    manifest_remote[["hash"]] <- NA_character_
  }
  .change_get_hash(manifest_remote, manifest_project)
}

#' @title Retrieve remote manifest data
#' @description Pulls either hashes or recorded manifest entries for the remote
#'   side depending on whether manifests are trusted for the comparison version.
#' @inheritParams .dsl_get_plan_fn
#' @return Manifest-like tibble describing remote state.
#' @keywords internal
#' @noRd
.dsl_get_manifest_remote <- function(version_comp,
                                     inspect,
                                     remote_comp,
                                     type,
                                     label,
                                     remote_pre) {
  # if we don't have a version with a trusted manifest,
  # or we explicity don't want to use the manifest
  if (is.null(version_comp) || inspect == "file") {
    .dsl_gmr_hash(
      remote_comp, type, version_comp, label
    )
  } else {
    .dsl_gmr_manifest(
      type, remote_pre, label, version_comp
    )
  }
}

#' @title Hash-based remote manifest helper
#' @description Computes manifest rows by hashing the remote files when a trusted
#'   manifest is unavailable.
#' @inheritParams .dsl_get_manifest_remote
#' @return Manifest-like tibble built from remote hashes.
#' @keywords internal
#' @noRd
.dsl_gmr_hash <- function(remote_comp,
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

#' @title Manifest-based remote helper
#' @description Retrieves the remote manifest file (if available) and filters it
#'   to the supplied label and version.
#' @inheritParams .dsl_get_manifest_remote
#' @param version Optional version override (defaults to current project
#'   version).
#' @return Tibble/data.frame of manifest entries from the remote metadata.
#' @keywords internal
#' @noRd
.dsl_gmr_manifest <- function(type,
                              remote_pre,
                              label,
                              version = NULL) {
  version <- if (is.null(version)) projr_version_get() else version
  .remote_get_manifest(type, remote_pre) |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(version)
}


# --------------------------------------------------------------------------
# Actions to take
# --------------------------------------------------------------------------

#' @title Translate file state into actionable plan
#' @description Converts filename sets into the concrete operations required for
#'   the chosen strategy, including manifest/version updates and changelog
#'   handling.
#' @param strategy Send strategy string.
#' @param fn_source Character vector of local filenames.
#' @param fn_dest Character vector describing remote filenames.
#' @param fn_source_extra Files only on the source side.
#' @param fn_dest_extra Files only on the destination side.
#' @param fn_same Files present and identical on both sides.
#' @param fn_diff Files present on both sides but with differing hashes.
#' @param remote_pre Remote pre-object.
#' @param remote_dest Remote destination object.
#' @param type Remote type identifier.
#' @param label Directory label.
#' @param version_comp Comparison version string or `NULL`.
#' @param cue Cue describing send conditions.
#' @param inspect Inspection mode string.
#' @param changelog Logical flag controlling changelog writes.
#' @return Named list describing adds/removals plus metadata work.
#' @keywords internal
#' @noRd
.dsl_get_plan_action <- function(strategy,
                                 fn_source,
                                 fn_dest,
                                 fn_source_extra,
                                 fn_dest_extra,
                                 fn_same,
                                 fn_diff,
                                 remote_pre,
                                 remote_dest_full,
                                 remote_dest_empty,
                                 type,
                                 label,
                                 version_comp,
                                 cue,
                                 inspect,
                                 changelog) {
  plan <- switch(strategy,
    "upload-all" = .dsl_gpa_upload_all(
      fn_source, remote_dest_full, remote_dest_empty,
      type, remote_pre, label, cue, version_comp
    ),
    "upload-missing" = .dsl_gpa_upload_missing(
      fn_source_extra, remote_dest_full, remote_dest_empty,
      type, remote_pre, label, cue, version_comp
    ),
    .dsl_gpa_sync(
      cue, fn_source_extra, type,
      remote_pre, remote_dest_full, remote_dest_empty, label,
      version_comp, fn_dest_extra, fn_diff, fn_same, strategy,
      inspect
    )
  )
  plan |>
    append(list("changelog" = changelog))
}

# upload-all
# ---------------------------
#' @title Build action plan for upload-all strategy
#' @description Prepares a plan that blindly uploads every tracked file and
#'   refreshes metadata regardless of existing remote contents.
#' @inheritParams .dsl_get_plan_action
#' @return Named list describing adds, manifest updates, and version metadata.
#' @keywords internal
#' @noRd
.dsl_gpa_upload_all <- function(fn_source, # nolint
                                remote_dest_full,
                                remote_dest_empty,
                                type,
                                remote_pre,
                                label,
                                cue,
                                version_comp) {
  # get what to add and remove
  fn_add <- fn_source
  fn_rm <- character(0L)

  # we only need to make sure it
  # exists if we are uploading to it.
  ensure_remote_dest_exists <-
    .dsl_gpa_ua_ensure_exists(
      fn_add, version_comp, cue
    )

  version_file <- .dsl_gpa_ua_version_file(
    fn_add, remote_dest_full, remote_dest_empty,
    ensure_remote_dest_exists, type, remote_pre, label
  )

  manifest <- .dsl_gpa_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )

  is_remote_dest_empty <-
    .dsl_gpa_get_is_remote_dest_empty(
      fn_add, fn_rm, label, version_file, manifest, remote_dest_full,
      remote_dest_empty
    )

  list(
    "fn_add" = fn_add,
    "fn_rm" = fn_rm,
    "version" = version_file,
    "manifest" = manifest,
    "purge" = FALSE,
    "is_remote_dest_empty" = is_remote_dest_empty,
    "ensure_remote_dest_exists" = ensure_remote_dest_exists
  )
}

.dsl_gpa_ua_ensure_exists <- function(fn_add,
                                      version_comp,
                                      cue) {
  # if we're adding anything, we need to ensure it exists
  if (.is_len_pos(fn_add)) {
    return(TRUE)
  }
  # but actually, even if it's empty, if we're uploading at all,
  # we need to make sure it exists.
  # if there was nothing to compare against, we need to ensure it exists
  # but `version_comp` is always NULL for `upload-all`, so this
  # means we would always upload...
  # that seems wrong to me.
  # well, I guess this is okay, because
  # either we do upload stuff, or we have an `empty`,
  # which is fine.
  # Well, unless people want to not just have empties? I imagine
  # they would. But why have `upload-all` then?
  # Okay, let's actually just make it that it's always TRUE,
  # which it will be if `version_comp` is `NULL`.
  # we can leave the checks for `fn_add`'s positivity
  # and `cue` being always in case we want to revisit this in future
  # (and to make it clear that those cases are handled correctly).
  if (is.null(version_comp)) {
    return(TRUE)
  }
  # if the cue is always, we need to ensure it exists
  if (cue == "always") {
    return(TRUE)
  }
  FALSE
}

.dsl_gpa_ua_version_file <- function(fn_add,
                                     remote_dest_full,
                                     remote_dest_empty,
                                     ensure_remote_dest_exists,
                                     type,
                                     remote_pre,
                                     label) {
  # check whether the remote exists in either form
  remote_exists_either <- !is.null(remote_dest_full) || !is.null(remote_dest_empty)

  # update label if we actually change files, or are creating the remote
  update_label <- .is_len_pos(fn_add) ||
    (!remote_exists_either && ensure_remote_dest_exists)

  # remove the asterisk if we're uploading and
  # it didn't exist
  asterisk_force_rm <- !remote_exists_either && ensure_remote_dest_exists

  # add the asterisk if it existed before
  asterisk_force_add <- remote_exists_either

  .dsl_gpa_version_file(
    type, remote_pre, label,
    update_label = update_label,
    asterisk_force_rm = asterisk_force_rm,
    asterisk_force_add = asterisk_force_add
  )
}

#' @title Update version file metadata for a destination
#' @description Loads the remote version file, updates the project version, and
#'   optionally refreshes the label entry while applying trust (asterisk)
#'   overrides.
#' @inheritParams .dsl_gpa_upload_all
#' @param update_label Logical flag indicating whether the label entry should be
#'   rewritten.
#' @param asterisk_force_add Logical flag forcing the untrusted indicator.
#' @param asterisk_force_rm Logical flag forcing removal of the untrusted
#'   indicator.
#' @return Character vector content of the updated version file.
#' @keywords internal
#' @noRd
.dsl_gpa_version_file <- function(type, # nolint
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
      .dsl_gpa_version_file_get_use_asterisk(
        asterisk_force_rm, asterisk_force_add, version_remote, label
      )

    version_remote <- .version_file_update_label_version(
      version_remote, label, use_asterisk
    )
  }
  version_remote
}

#' @title Decide whether the version entry should be marked untrusted
#' @description Combines forced overrides with prior version file state to
#'   determine whether an asterisk should appear next to the label entry.
#' @inheritParams .dsl_gpa_version_file
#' @param asterisk_force_rm Logical flag forcing removal.
#' @param asterisk_force_add Logical flag forcing addition.
#' @param version_remote Character vector representation of the version file.
#' @return Logical flag indicating whether the entry should include an asterisk.
#' @keywords internal
#' @noRd
.dsl_gpa_version_file_get_use_asterisk <- function(asterisk_force_rm,
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
  .dsl_gpa_version_file_check_untrusted(
    version_remote, label
  )
}

#' @title Inspect version file for trust markers
#' @description Checks whether the existing version file already marks the label
#'   as untrusted.
#' @inheritParams .dsl_gpa_version_file
#' @param version_file Character vector contents of the version file.
#' @return Logical flag indicating whether the label entry currently ends with
#'   an asterisk.
#' @keywords internal
#' @noRd
.dsl_gpa_version_file_check_untrusted <- function(version_file, # nolint
                                                  label) { # nolint
  if (length(version_file) == 0L) {
    return(FALSE)
  }
  match_str <- utils::glob2rx(label) |>
    (\(x) gsub("\\$", "", x))() |>
    paste0(": ")
  label_regex <- grep(match_str, version_file, value = TRUE)
  if (.is_len_0(label_regex)) {
    return(FALSE)
  }
  # Return TRUE if ends in an asterisk
  grepl("\\*$", label_regex)
}

#' @title Build manifest updates for a destination
#' @description Combines existing remote manifest entries with the current
#'   project manifest to reflect the new upload plan.
#' @inheritParams .dsl_gpa_upload_all
#' @param rm_existing Logical flag to remove any entries that will be overwritten.
#' @param rm_existing_all Logical flag removing all current rows for the version
#'   and label before appending.
#' @param rm_adding Logical flag removing files from the append set if they are
#'   unchanged (used by upload-missing).
#' @return Tibble/data.frame representing the manifest that should be written to
#'   the remote.
#' @keywords internal
#' @noRd
.dsl_gpa_manifest <- function(type,
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
      .dsl_gpa_manifest_rm_existing(
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
      .dsl_gpa_manifest_rm_existing(
        manifest_append, label, manifest_remote_existing
      )
  }

  manifest_append |>
    .manifest_append_previous_impl(manifest_remote)
}

#' @title Remove manifest rows that will be replaced
#' @description Helper that strips any remote manifest rows overlapping with the
#'   current append set, ensuring the resulting manifest has unique filenames.
#' @param manifest_remote Existing remote manifest tibble.
#' @inheritParams .dsl_gpa_upload_all
#' @param manifest_append Manifest rows that will be appended.
#' @return Remote manifest tibble with overlapping rows removed.
#' @keywords internal
#' @noRd
.dsl_gpa_manifest_rm_existing <-
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
#' @title Build action plan for upload-missing strategy
#' @description Generates an upload plan that only adds files missing remotely
#'   while preserving existing remote contents.
#' @inheritParams .dsl_get_plan_action
#' @return Named list describing files to add plus manifest/version updates.
#' @keywords internal
#' @noRd
.dsl_gpa_upload_missing <- function(fn_source_extra, # nolint
                                    remote_dest_full,
                                    remote_dest_empty,
                                    type,
                                    remote_pre,
                                    label,
                                    cue,
                                    version_comp) {
  # get what to add and remove
  fn_add <- fn_source_extra
  fn_rm <- character(0L)

  # we only need to make sure it
  # exists if we are uploading to it.
  ensure_remote_dest_exists <-
    .dsl_gpa_um_ensure_exists(
      fn_add, version_comp, cue
    )

  #
  version_file <- .dsl_gpa_um_version_file(
    fn_add, remote_dest_full, remote_dest_empty,
    ensure_remote_dest_exists, type, remote_pre, label
  )

  manifest <- .dsl_gpa_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )

  is_remote_dest_empty <-
    .dsl_gpa_get_is_remote_dest_empty(
      fn_add, fn_rm, label, version_file, manifest, remote_dest_full,
      remote_dest_empty
    )

  list(
    "fn_add" = fn_add,
    "fn_rm" = fn_rm,
    "manifest" = manifest,
    "version" = version_file,
    "purge" = FALSE,
    "is_remote_dest_empty" = is_remote_dest_empty,
    "ensure_remote_dest_exists" = ensure_remote_dest_exists
  )
}

.dsl_gpa_um_ensure_exists <- function(fn_add,
                                      version_comp,
                                      cue) {
  # same as for upload all
  # if we're adding anything, we need to ensure it exists
  if (.is_len_pos(fn_add)) {
    return(TRUE)
  }
  if (is.null(version_comp)) {
    return(TRUE)
  }
  # if the cue is always, we need to ensure it exists
  if (cue == "always") {
    return(TRUE)
  }
  FALSE
}

.dsl_gpa_um_version_file <- function(fn_add,
                                     remote_dest_full,
                                     remote_dest_empty,
                                     ensure_remote_dest_exists,
                                     type,
                                     remote_pre,
                                     label) {
  # same as for upload_all

  # check whether the remote exists in either form
  remote_exists_either <- !is.null(remote_dest_full) || !is.null(remote_dest_empty)

  # update label if we actually change files, or are creating the remote
  update_label <- .is_len_pos(fn_add) ||
    (!remote_exists_either && ensure_remote_dest_exists)

  # remove the asterisk if we're uploading and
  # it didn't exist
  asterisk_force_rm <- !remote_exists_either && ensure_remote_dest_exists

  # add the asterisk if it existed before
  asterisk_force_add <- remote_exists_either

  .dsl_gpa_version_file(
    type, remote_pre, label,
    update_label = update_label,
    asterisk_force_rm = asterisk_force_rm,
    asterisk_force_add = asterisk_force_add
  )
}

# sync
# ---------------------------

#' @title Dispatch sync strategy planning
#' @description Chooses between purge and diff implementations for sync
#'   strategies and forwards the required parameters.
#' @inheritParams .dsl_get_plan_action
#' @return Named list describing the resulting action plan.
#' @keywords internal
#' @noRd
.dsl_gpa_sync <- function(cue,
                          fn_source_extra,
                          type,
                          remote_pre,
                          remote_dest_full,
                          remote_dest_empty,
                          label,
                          version_comp,
                          fn_dest_extra,
                          fn_diff,
                          fn_same,
                          strategy,
                          inspect) {
  if (strategy == "sync-purge") {
    .dsl_gpa_purge(
      type, remote_pre,
      remote_dest_full, remote_dest_empty,
      label, version_comp,
      fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy,
      inspect, cue
    )
  } else {
    .assert_has(strategy, "sync-diff")
    .dsl_gpa_diff(
      fn_source_extra, type,
      remote_pre, remote_dest_full, remote_dest_empty,
      label, version_comp,
      fn_dest_extra, fn_diff, fn_same,
      cue, inspect
    )
  }
}

# --- purge ---
#' @title Build purge variant of sync plan
#' @description Generates an action plan that mirrors the local state by
#'   removing and re-uploading files as required for `sync-purge`.
#' @inheritParams .dsl_get_plan_action
#' @return Named list describing purge operations and metadata changes.
#' @keywords internal
#' @noRd
.dsl_gpa_purge <- function(type, # nolint
                           remote_pre,
                           remote_dest_full,
                           remote_dest_empty,
                           label,
                           version_comp,
                           fn_source_extra,
                           fn_dest_extra,
                           fn_diff,
                           fn_same,
                           strategy,
                           inspect,
                           cue) {
  # get what to add
  fn_add <- c(fn_source_extra, fn_diff, fn_same)
  fn_rm <- character(0L)

  ensure_remote_dest_exists <-
    .dsl_gpa_p_ensure_exists(
      fn_add, fn_dest_extra, cue, version_comp
    )

  version_file <- .dsl_gpa_p_version_file(
    fn_add, fn_dest_extra,
    remote_dest_full, remote_dest_empty,
    ensure_remote_dest_exists,
    type, remote_pre, label
  )

  manifest <- .dsl_gpa_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )

  # don't know if we're not adding anything,
  # but we do know it's not empty if we're adding
  # and we know it's empty if we're not
  is_remote_dest_empty <- if (ensure_remote_dest_exists) {
    !.is_len_pos(fn_add)
  } else {
    NULL
  }

  # only purge if the one of the remote dests exist and
  # if we are adding or removing files
  purge <- (!is.null(remote_dest_full) || !is.null(remote_dest_empty)) &&
    ensure_remote_dest_exists &&
    (.is_len_pos(fn_add) || .is_len_pos(fn_dest_extra))

  list(
    fn_add = fn_add,
    fn_rm = fn_rm,
    version = version_file,
    manifest = manifest,
    purge = purge,
    is_remote_dest_empty = is_remote_dest_empty,
    ensure_remote_dest_exists = ensure_remote_dest_exists
  )
}

.dsl_gpa_p_ensure_exists <- function(fn_add,
                                     fn_dest_extra,
                                     cue,
                                     version_comp) {
  # will ensure it exists if we are adding files (fn_add)
  # or if the remote has extra files (fn_dest_extra),
  # if there is a cue to always do so,
  # or if there is a change from before (which
  # would not have triggered fn_add if we're only removing files).
  .is_len_pos(fn_add) || .is_len_pos(fn_dest_extra) ||
    cue == "always" || is.null(version_comp)
}

.dsl_gpa_p_version_file <- function(fn_add,
                                    fn_dest_extra,
                                    remote_dest_full,
                                    remote_dest_empty,
                                    ensure_remote_dest_exists,
                                    type,
                                    remote_pre,
                                    label) {
  # check whether the remote exists in either form
  remote_exists_either <- !is.null(remote_dest_full) ||
    !is.null(remote_dest_empty)

  # update label if we actually do something, or are creating the remote
  update_label <- .is_len_pos(c(fn_add, fn_dest_extra)) ||
    (!remote_exists_either && ensure_remote_dest_exists)

  # we trust it if we are creating it or if
  # we are purging and uploading
  asterisk_force_rm <- !remote_exists_either ||
    (.is_len_pos(fn_add) || .is_len_pos(fn_dest_extra))

  .dsl_gpa_version_file(
    type, remote_pre, label,
    update_label = update_label,
    asterisk_force_rm = asterisk_force_rm,
    asterisk_force_add = FALSE
  )
}

# --- diff ---
#' @title Build diff variant of sync plan
#' @description Generates an action plan that applies only the detected diffs
#'   between local and remote manifests.
#' @inheritParams .dsl_get_plan_action
#' @return Named list describing diff-based uploads/removals plus metadata.
#' @keywords internal
#' @noRd
.dsl_gpa_diff <- function(fn_source_extra, # nolint
                          type,
                          remote_pre,
                          remote_dest_full,
                          remote_dest_empty,
                          label,
                          version_comp,
                          fn_dest_extra,
                          fn_diff,
                          fn_same,
                          cue,
                          inspect) {
  # get what to add and remove
  fn_add <- c(fn_source_extra, fn_diff)
  fn_rm <- fn_dest_extra

  # we only need to make sure it
  # exists if we are uploading to it.
  ensure_remote_dest_exists <-
    .dsl_gpa_d_ensure_exists(
      fn_add, fn_rm, version_comp, cue
    )

  version_file <- .dsl_gpa_d_version_file(
    fn_add, fn_rm,
    remote_dest_full, remote_dest_empty,
    inspect, version_comp,
    ensure_remote_dest_exists,
    type, remote_pre, label
  )

  manifest <- .dsl_gpa_manifest(
    type, remote_pre, label,
    rm_existing = TRUE
  )

  is_remote_dest_empty <-
    .dsl_gpa_get_is_remote_dest_empty(
      fn_add, fn_rm, label, version_file, manifest, remote_dest_full,
      remote_dest_empty
    )

  list(
    fn_add = fn_add,
    fn_rm = fn_rm,
    version = version_file,
    manifest = manifest,
    purge = FALSE,
    is_remote_dest_empty = is_remote_dest_empty,
    ensure_remote_dest_exists = ensure_remote_dest_exists
  )
}

.dsl_gpa_d_ensure_exists <- function(fn_add,
                                     fn_rm,
                                     version_comp,
                                     cue) {
  # check if there is a change
  is_change <- .is_len_pos(c(fn_add, fn_rm))

  # will upload if:
  # - there is a difference (by definition),
  # - the previous version is not trusted or does not exist
  #   (version_comp is NULL), or
  # - we are supposed to always upload (cue is "always")
  is_change ||
    is.null(version_comp) ||
    cue == "always"
}

.dsl_gpa_d_version_file <- function(fn_add,
                                    fn_rm,
                                    remote_dest_full,
                                    remote_dest_empty,
                                    inspect,
                                    version_comp,
                                    ensure_remote_dest_exists,
                                    type,
                                    remote_pre,
                                    label) {
  # check whether the remote exists in either form
  remote_exists_either <- !is.null(remote_dest_full) ||
    !is.null(remote_dest_empty)

  # update label if we actually do something, or are creating the remote
  update_label <- .is_len_pos(c(fn_add, fn_rm)) ||
    (remote_exists_either && ensure_remote_dest_exists)

  asterisk_force_rm <- !remote_exists_either ||
    inspect == "file" ||
    # if the version to compare against is NULL,
    # then we would have hashed the files,
    # so we can trust again.
    (inspect == "manifest" && is.null(version_comp))

  # add it if it was not inspected and it's not new
  asterisk_force_add <- remote_exists_either && inspect == "none"

  .dsl_gpa_version_file(
    type, remote_pre, label,
    update_label = update_label,
    asterisk_force_rm = asterisk_force_rm,
    asterisk_force_add = asterisk_force_add
  )
}


.dsl_gpa_get_is_remote_dest_empty <- function(fn_add,
                                              fn_rm,
                                              label,
                                              version_file,
                                              manifest,
                                              remote_dest_full,
                                              remote_dest_empty) {
  # if we're adding, then it cannot be empty
  if (.is_len_pos(fn_add)) {
    return(FALSE)
  }
  # if we're removing, then if we trust the manifest, we
  # can check whether it is empty based on the manifest
  is_trusted <- .version_file_check_label_trusted(version_file, label)
  if (is_trusted) {
    manifest_version_label <- manifest |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(projr_version_get())
    manifest_remaining <- manifest_version_label[
      !manifest_version_label[["fn"]] %in% fn_rm,
    ]
    is_zero_row <- nrow(manifest_remaining) == 0L
    is_empty_placeholder <- nrow(manifest_remaining) == 1 &&
      !is.na(manifest_remaining[["fn"]]) &&
      manifest_remaining[["fn"]] == "projr-empty"
    is_empty <- is_zero_row || is_empty_placeholder
    return(is_empty)
  }

  # well, at this stage, if the full remote doesn't exist and we'd
  # only remove files, then it is empty
  if (is.null(remote_dest_full)) {
    return(TRUE)
  }

  # return NULL if we haven't been able to tell quickly by now,
  # so we can just see what we have up there and then decide
  # if there is anything there or not.
  NULL
}

# ==========================================================================
# Implement plan
# ==========================================================================

#' @title Execute a prepared send plan
#' @description Applies purge/create operations, performs file uploads/removals,
#'   manages GitHub empty placeholders, and writes manifest/version files back to
#'   the remote.
#' @inheritParams .dsl_get_plan_action
#' @inheritParams .dest_send_label
#' @param fn_add Files to upload.
#' @param fn_rm Files to remove from the remote.
#' @param version_file Character vector describing the updated version file.
#' @param manifest Manifest tibble ready to write.
#' @param create Logical flag requesting remote creation.
#' @param purge Logical flag requesting existing content removal prior to upload.
#' @param id Remote id (path/tag/node) for the destination.
#' @param structure Remote structure (`latest` or `archive`).
#' @param path Optional relative path from `_projr.yml`.
#' @param path_append_label Logical flag controlling label path appending.
#' @param path_dir_local Local directory path containing files to upload.
#' @return Invisibly returns `TRUE` upon success.
#' @keywords internal
#' @noRd
.dsl_implement_plan <- function(fn_add,
                                fn_rm,
                                version_file,
                                manifest,
                                purge,
                                is_remote_dest_empty,
                                ensure_remote_dest_exists,
                                changelog,
                                remote_dest_full,
                                remote_dest_empty,
                                type,
                                id,
                                label,
                                structure,
                                path,
                                path_append_label,
                                path_dir_local,
                                remote_pre,
                                cue,
                                output_level = "std") {
  .cli_debug(
    "Content '{label}': Implementing upload plan",
    output_level = output_level
  )

  # purge
  remote_dest_full <- .dsl_ip_purge(
    purge, type, remote_dest_full, output_level
  )

  # add files
  remote_dest_full <- .dsl_ip_add(
    fn_add, remote_dest_full,
    type, id, label,
    structure, path, path_append_label,
    projr_version_get(), path_dir_local, output_level
  )

  # remove files
  remote_dest_full <- .dsl_ip_rm(
    fn_rm, remote_dest_full,
    type, id, label,
    structure, path, path_append_label,
    projr_version_get(),
    output_level
  )

  # remove unncecessary empty remote
  # or ensure at least an empty remote exists
  # if required
  remote_dest_empty <- .dsl_ip_finalise_remotes(
    remote_dest_full,
    remote_dest_empty,
    remote_pre,
    type, id, label,
    structure, path, path_append_label,
    fn_add, fn_rm, cue,
    output_level
  )

  # add log files
  .dsl_ip_log(
    version_file, manifest, type,
    remote_pre, changelog, output_level
  )
}

.dsl_ip_purge <- function(purge,
                          type,
                          remote_dest_full,
                          output_level = "std") {
  dont_purge <- is.null(remote_dest_full) || !purge
  if (dont_purge) {
    .cli_debug(
      "Not purging remote destination",
      output_level = output_level
    )
    return(remote_dest_full)
  }
  .cli_debug(
    "Purging remote destination",
    output_level = output_level
  )
  .remote_final_empty(type, remote_dest_full)
  # remote_dest_full becomes this now,
  # as it will no longer exist
  NULL
}

.dsl_ip_add <- function(fn_add,
                        remote_dest_full,
                        type,
                        id,
                        label,
                        structure,
                        path,
                        path_append_label,
                        version,
                        path_dir_local,
                        output_level = "std") {
  if (!.is_len_pos(fn_add)) {
    .cli_debug(
      "Content '{label}': No files to add to remote",
      output_level = output_level
    )
    return(remote_dest_full)
  }
  .cli_debug(
    "Content '{label}': Adding {length(fn_add)} file(s) to remote",
    output_level = output_level
  )
  remote_add <- .dsl_ip_a_get_remote_add(
    remote_dest_full, type, id, label, structure, path,
    path_append_label, version
  )
  .remote_file_add(type, remote_add, path_dir_local, fn_add, output_level)
  remote_add
}

.dsl_ip_a_get_remote_add <- function(remote_dest_full,
                                     type,
                                     id,
                                     label,
                                     structure,
                                     path,
                                     path_append_label,
                                     version) {
  if (!is.null(remote_dest_full)) {
    return(remote_dest_full)
  }
  .remote_final_get(
    type, id, label, structure, path,
    path_append_label, version, FALSE, FALSE
  )
}

.dsl_ip_rm <- function(fn_rm,
                       remote_dest_full,
                       type,
                       id,
                       label,
                       structure,
                       path,
                       path_append_label,
                       version,
                       output_level = "std") {
  if (!.is_len_pos(fn_rm)) {
    .cli_debug(
      "Content '{label}': No files to remove from remote",
      output_level = output_level
    )
    return(remote_dest_full)
  }
  remote_rm <- .dsl_ip_r_get_remote_rm(
    remote_dest_full, type, id, label,
    structure, path, path_append_label, version
  )
  if (is.null(remote_rm)) {
    .cli_debug(
      "Content '{label}': Remote destination does not exist; skipping file removals", # nolint
      output_level = output_level
    )
    return(remote_dest_full)
  }
  .cli_debug(
    "Content '{label}': Removing {length(fn_rm)} file(s) from remote",
    output_level = output_level
  )
  .remote_file_rm(type, fn_rm, remote_rm, output_level)
  Sys.sleep(1) # ensure remote consistency

  # Check if remote still exists after removal
  # (may have been automatically deleted if it became empty)
  .remote_final_get_if_exists(
    type, id, label, structure, path,
    path_append_label, version, FALSE, FALSE
  )
}

.dsl_ip_r_get_remote_rm <- function(remote_dest_full,
                                    type,
                                    id,
                                    label,
                                    structure,
                                    path,
                                    path_append_label,
                                    version) {
  if (!is.null(remote_dest_full)) {
    return(remote_dest_full)
  }

  .remote_final_get_if_exists(
    type, id, label, structure, path,
    path_append_label, version, FALSE, FALSE
  )
}

.dsl_ip_log <- function(version_file,
                        manifest,
                        type,
                        remote_pre,
                        changelog,
                        output_level = "std") {
  .cli_debug(
    "Uploading manifest",
    output_level = output_level
  )
  .remote_write_manifest(type, remote_pre, manifest)
  .cli_debug(
    "Uploading version file",
    output_level = output_level
  )
  .remote_write_version_file(type, remote_pre, version_file)
  if (changelog) {
    .cli_debug(
      "Writing changelog to remote",
      output_level = output_level
    )
    .remote_write_changelog(type, remote_pre)
  }
  invisible(TRUE)
}

.dsl_ip_finalise_remotes <- function(remote_dest_full,
                                     remote_dest_empty,
                                     remote_pre,
                                     type,
                                     id,
                                     label,
                                     structure,
                                     path,
                                     path_append_label,
                                     fn_add,
                                     fn_rm,
                                     cue,
                                     output_level = "std") {
  # Re-check if empty remote exists (might have been created in previous build)
  # even if remote_dest_empty was NULL from initial check.
  # For archive structures, we need to check for the empty variant of the
  # version that was just uploaded (remote_dest_full), not the current version
  # which may have already been bumped to dev.
  if (is.null(remote_dest_empty) && !is.null(remote_dest_full) && structure == "archive") {
    .cli_debug(
      "Re-checking for empty remote variant after adding files",
      output_level = output_level
    )
    # Extract version from remote_dest_full path
    # For local: path ends with v<version>, for github: it's in the "fn" component
    version_from_full <- .dsl_ip_fr_extract_version(remote_dest_full, type)
    .cli_debug(
      "Extracted version from full remote: {version_from_full}",
      output_level = output_level
    )
    if (!is.null(version_from_full)) {
      remote_dest_empty <- .remote_final_get_if_exists(
        type, id, label, structure, path, path_append_label,
        version_from_full, FALSE, TRUE
      )
      if (!is.null(remote_dest_empty)) {
        .cli_debug(
          "Found empty remote variant: {remote_dest_empty}",
          output_level = output_level
        )
      } else {
        .cli_debug(
          "No empty remote variant found",
          output_level = output_level
        )
      }
    }
  }

  # if both exist, then we remove the empty remote
  remote_dest_empty <- .dsl_ip_fr_remove_empty(
    remote_dest_full, remote_dest_empty,
    type,
    output_level = output_level
  )

  # ensure at least the empty remote exists,
  # if a remote is required
  remote_dest_empty <- .dsl_ip_fr_ensure_exists(
    remote_dest_full,
    remote_dest_empty,
    remote_pre,
    type, id, label, structure,
    path, path_append_label,
    fn_add, fn_rm,
    cue,
    output_level = output_level
  )
  remote_dest_empty
}

.dsl_ip_fr_extract_version <- function(remote, type) {
  # Extract version string from remote object
  if (type == "github") {
    # GitHub: remote is c("tag" = "v0.0.1", "fn" = "output-v0.0.1.zip")
    # Extract from fn component, strip .zip
    fn <- remote[["fn"]]
    if (is.null(fn)) {
      return(NULL)
    }
    # Remove .zip extension and extract version
    fn_no_zip <- sub("\\.zip$", "", fn)
    # Extract v<version> pattern from end
    version_match <- regmatches(fn_no_zip, regexpr("v[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9]+)?$", fn_no_zip))
    if (length(version_match) == 0) {
      return(NULL)
    }
    sub("^v", "", version_match)
  } else if (type %in% c("local", "osf")) {
    # Local/OSF: remote is a path ending with v<version>
    # Extract v<version> from the basename
    basename_remote <- basename(remote)
    version_match <- regmatches(basename_remote, regexpr("v[0-9]+\\.[0-9]+\\.[0-9]+(-[0-9]+)?$", basename_remote))
    if (length(version_match) == 0) {
      return(NULL)
    }
    sub("^v", "", version_match)
  } else {
    NULL
  }
}

.dsl_ip_fr_remove_empty <- function(remote_dest_full,
                                    remote_dest_empty,
                                    type,
                                    output_level = "std") {
  .cli_debug(
    "Removing empty remote destination if full destination exists",
    output_level = output_level
  )
  full_exists <- !is.null(remote_dest_full)
  empty_exists <- !is.null(remote_dest_empty)
  remove_empty <- full_exists && empty_exists
  if (!remove_empty) {
    return(remote_dest_empty)
  }
  .cli_debug(
    "Full remote exists; removing empty remote",
    output_level = output_level
  )
  .remote_final_rm(
    type, remote_dest_empty
  )
  NULL
}

.dsl_ip_fr_ensure_exists <- function(remote_dest_full,
                                     remote_dest_empty,
                                     remote_pre,
                                     type,
                                     id,
                                     label,
                                     structure,
                                     path,
                                     path_append_label,
                                     fn_add,
                                     fn_rm,
                                     cue,
                                     output_level = "std") {
  .cli_debug(
    "Ensuring a remote exists if required",
    output_level = output_level
  )
  # If we just added files, we definitely have a full remote (don't create empty)
  added_files <- .is_len_pos(fn_add)
  full_exists <- !is.null(remote_dest_full)
  empty_exists <- !is.null(remote_dest_empty)
  .cli_debug(
    "added_files: {added}, remote_dest_full: {full}, remote_dest_empty: {empty}",
    added = added_files,
    full = if (is.null(remote_dest_full)) "NULL" else as.character(remote_dest_full),
    empty = if (is.null(remote_dest_empty)) "NULL" else as.character(remote_dest_empty),
    output_level = output_level
  )
  .cli_debug(
    "full_exists: {full_exists}, empty_exists: {empty_exists}",
    output_level = output_level
  )
  if (added_files || full_exists || empty_exists) {
    .cli_debug(
      "Remote destination already exists (or files were just added); no need to create empty",
      output_level = output_level
    )
    return(remote_dest_empty)
  }

  # Check conditions for creating empty remote
  cond_latest <- structure == "latest"
  cond_fn_rm <- .is_len_pos(fn_rm)
  cond_always <- cue == "always"
  final_remotes <- .remote_ls_final(type, remote_pre)
  cond_no_remotes <- .is_len_0(final_remotes)

  .cli_debug(
    "Conditions: latest={latest}, fn_rm_len={fn_rm_len}, cue={cue}, final_remotes={num_final}",
    latest = cond_latest,
    fn_rm_len = length(fn_rm),
    cue = cue,
    num_final = length(final_remotes),
    output_level = output_level
  )

  create_empty <- cond_latest || cond_fn_rm || cond_always || cond_no_remotes

  if (!create_empty) {
    .cli_debug(
      "No need to create empty remote destination",
      output_level = output_level
    )
    return(NULL)
  }

  # create empty remote now, as clearly
  # we cannot upload anything at this point
  .cli_debug(
    "Creating empty remote destination for version={version}",
    version = projr_version_get(),
    output_level = output_level
  )
  .remote_final_empty_get(
    type, id, label, structure,
    path, path_append_label,
    projr_version_get()
  )
}
