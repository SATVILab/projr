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
  remote_list <- .dest_send_label_get_remotes(
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
      "Content '{label}': Remote destination exists at path: {remote_list[['remote_dest']][['path']]}",  # nolint
      output_level = output_level
    )
  } else {
    .cli_debug(
      "Content '{label}': Remote destination does not exist yet (will be created)", # nolint
      output_level = output_level
    )
  }

  plan <- .dest_send_label_get_plan(
    yml_title[["send"]][["strategy"]], yml_title[["send"]][["inspect"]],
    remote_list[["version_comp"]], type, label,
    remote_list[["remote_pre"]], remote_list[["remote_dest"]],
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

  .dest_send_label_implement_plan(
    plan[["fn_add"]], plan[["fn_rm"]], plan[["version"]],
    plan[["manifest"]], plan[["create"]], plan[["purge"]],
    plan[["changelog"]],
    remote_list[["remote_dest"]], type, yml_title[["id"]], label,
    yml_title[["structure"]], yml_title[["path"]],
    yml_title[["path-append-label"]],
    path_dir_local, remote_list[["remote_pre"]],
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
.dest_send_label_get_remotes <- function(type,
                                         id,
                                         path,
                                         path_append_label,
                                         label,
                                         structure,
                                         strategy,
                                         inspect,
                                         cue) {
  remote_pre <- .remote_final_get(
    type, id, label, structure, path, path_append_label, NULL, TRUE, FALSE
  )
  remote_dest <- .dest_send_label_get_remotes_get_remote_dest(
    type, id, label, structure, path, path_append_label,
    projr_version_get(), FALSE, FALSE
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

#' @title Retrieve an existing destination remote
#' @description Wrapper around `.remote_final_get_if_exists()` that returns the
#'   concrete destination remote if it already exists, otherwise `NULL`.
#' @inheritParams .dest_send_label_get_remotes
#' @param version Optional version override when composing the remote.
#' @param pre Logical flag requesting the parent (pre) remote rather than the
#'   final remote handle.
#' @return Remote definition or `NULL` when the remote does not yet exist.
#' @keywords internal
#' @noRd
.dest_send_label_get_remotes_get_remote_dest <- function(type,
                                                         id,
                                                         label,
                                                         structure,
                                                         path,
                                                         path_append_label,
                                                         version,
                                                         pre) {
  # always work with non-empty remote, and adapt it to be empty
  # remote if that is what is needed
  .remote_final_get_if_exists(
    type, id, label, structure, path, path_append_label,
    version, pre, FALSE
  )
}

#' @title Determine comparison version for a destination
#' @description Calculates which remote version (if any) can be trusted for
#'   comparisons based on structure, inspection mode, strategy, and cue.
#' @inheritParams .dest_send_label_get_remotes
#' @param remote_pre Remote pre-object produced by `.remote_final_get()`.
#' @return Character version string (without suffix) or `NULL` when no trusted
#'   comparison exists.
#'
#' @details
#'
#' If no inspection is done, then there is no remote to compare against,
#' and so we return NULL.
#'
#' If
#'
#' @keywords internal
#' @noRd
.dest_send_label_get_remotes_get_version_comp <- function(remote_pre, # nolint
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
  if (cue == "always" || strategy %in% c("upload-missing", "upload-all")) {
    return(projr_version_get() |> .version_v_rm())
  }
  # now we see if we can compare against an archived remote
  .dest_send_label_get_remotes_get_version_comp_archive(
    cue, strategy, label, inspect, remote_pre, type
  )
}

#' @title Check whether comparison logic can be skipped
#' @description Evaluates early-exit conditions that make remote comparisons
#'   unnecessary (no remote, no inspection, or upload-all strategy).
#' @inheritParams .dest_send_label_get_remotes_get_version_comp
#' @return Logical flag indicating whether comparison work should be skipped.
#' @keywords internal
#' @noRd
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

#' @title Resolve comparison version for latest remotes
#' @description Determines which version string should be used when comparing
#'   against a `latest` remote structure, honoring inspection mode and manifest
#'   trust rules.
#' @inheritParams .dest_send_label_get_remotes_get_version_comp
#'
#' @details
#'
#'
#' @return Character version string or `NULL` when no trusted version is
#'   available.
#'
#' @keywords internal
#' @noRd
.dest_send_label_get_remotes_get_version_comp_latest <-
  function(inspect,
           remote_pre,
           type,
           label) { # nolint
    # project version
    version_project <- projr_version_get() |> .version_v_rm()
    # if we inspect by "file" (and not "manifest"; we already know it's not "none"),
    # then the final remote we compare against is independent
    # of the version (as the versoin does not from a part of 
    # "latest" final remotes), so the point of returning version_project
    # for latest is that we are saying that we have something to compare against.
    # The only reason we do not return straight away is that
    # we have to check that this remote exists first,
    # which we do via `.remote_get_version_label`.
    version_comp_untrusted <- if (inspect == "file") version_project else NULL
    version_remote_raw <- .remote_get_version_label(
      remote_pre, type, label, "latest"
    )
    # Only call .version_v_rm if we have a valid value
    version_remote <- if (.is_string(version_remote_raw)) {
      version_remote_raw |> .version_v_rm()
    } else {
      NULL
    }
    if (!.is_string(version_remote)) {
      version_comp_untrusted
    } else {
      version_remote
    }
  }

#' @title Resolve comparison version for archive remotes
#' @description Selects the remote archive version that can be trusted during
#'   comparisons, accounting for cues, strategies, manifests, and acceptable
#'   version ranges.
#' @inheritParams .dest_send_label_get_remotes_get_version_comp
#' @return Trusted version string or `NULL` if no archive is suitable.
#' @keywords internal
#' @noRd
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
  version_remote_raw <- .remote_get_version_label(
    remote_pre, type, label, "archive"
  )
  # Check if version_remote_raw is empty before calling .version_v_rm()
  if (.is_len_0(version_remote_raw)) {
    return(version_comp_no_trusted_archive)
  }
  version_remote <- version_remote_raw |> .version_v_rm()
  # earliest version does not work if it's not trusted
  # or none are available (version_remote is NULL),
  # or if the version is too old
  if (!.is_string(version_remote) || version_remote < version_min_acceptable) {
    return(version_comp_no_trusted_archive)
  }
  # at this point, this is simply the latest remote.
  # we trust its information, so we can use it
  version_remote
}

#' @title Build comparison remote handle
#' @description Returns the remote that should be used for comparisons, falling
#'   back to the destination remote if no explicit version is supplied.
#' @inheritParams .dest_send_label_get_remotes
#' @param version Optional version string to retrieve when comparing archives.
#' @param remote_dest Destination remote returned earlier (may be `NULL`).
#' @return Remote object suitable for comparison.
#' @keywords internal
#' @noRd
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
    remote_comp <- .remote_final_get(
      type, id, label, structure, path, path_append_label, version,
      FALSE, TRUE
    )
    .dest_send_label_get_remotes_comp_empty(remote_comp, type)
  }
}

#' @title Normalize GitHub comparison remotes
#' @description Chooses between the normal and "-empty" GitHub asset variants
#'   so comparisons use the correct object without causing ambiguous results.
#' @inheritParams .dest_send_label_get_remotes_comp
#' @param remote_comp Remote comparison object created earlier.
#' @return Possibly modified remote comparison object appropriate for the
#'   remote type.
#' @keywords internal
#' @noRd
.dest_send_label_get_remotes_comp_empty <- function(remote_comp,
                                                    type) {
  if (type != "github") {
    return(remote_comp)
  }
  remote_comp_exists <- .remote_final_check_exists_direct(
    "github", remote_comp
  )
  remote_comp_empty <- remote_comp
  remote_comp_empty[["fn"]] <- gsub("\\.zip$", "-empty.zip", remote_comp[["fn"]])
  remote_comp_empty_exists <- .remote_final_check_exists_direct(
    "github", remote_comp_empty
  )
  if (remote_comp_exists && remote_comp_empty_exists) {
    .cli_debug(
      "Both normal and empty remote comparison exist on GitHub for remote: {remote_comp[['fn']]}."
    )
    stop("Both normal and empty remote comparison exist on GitHub, cannot proceed.")
  } else if (remote_comp_empty_exists) {
    .cli_debug(
      "Using empty remote comparison on GitHub for remote: {remote_comp_empty[['fn']]}."
    )
    remote_comp_empty
  } else {
    # assuming it exists, we'll see how this goes
    .cli_debug(
      "Using normal remote comparison on GitHub for remote: {remote_comp[['fn']]}."
    )
    remote_comp
  }
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
    remote_pre, remote_dest, type, label, version_comp, cue, inspect, changelog
  )
}

# --------------------------------------------------------------------------
# How files have changed
# --------------------------------------------------------------------------

#' @title Determine file state for send plan
#' @description Builds filename vectors used by plan execution based on the
#'   chosen strategy (upload-all, upload-missing, sync).
#' @inheritParams .dest_send_label_get_plan
#' @return Named list containing source/destination filename vectors and diff
#'   sets required for subsequent planning.
#' @keywords internal
#' @noRd
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
      inspect, version_comp, remote_comp, remote_pre, type, label
    ),
    .dest_send_label_get_plan_fn_sync(
      inspect, version_comp, remote_pre, remote_dest, remote_comp,
      type, label
    )
  )
}

#' @title File listing helper for upload-all
#' @description Returns every file tracked for the label so they can all be
#'   added to the destination regardless of remote state.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Character vector of filenames sourced from the project manifest.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_fn_upload_all <- function(label) {
  # will add whatever is in `fn_source`, nothing else needed
  list("fn_source" = .dest_send_label_get_fn_source(label))
}

#' @title Retrieve project-side filenames for a label
#' @description Pulls the file list for the current project version from the
#'   local manifest for the supplied label.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Character vector of filenames present locally.
#' @keywords internal
#' @noRd
.dest_send_label_get_fn_source <- function(label) {
  fn_vec <- .dest_send_label_get_manifest_source(label)[["fn"]]
  fn_vec[!is.na(fn_vec)]
}

#' @title Retrieve manifest rows for a label
#' @description Filters the project manifest down to the current version and the
#'   requested label to support downstream comparisons.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Tibble/data.frame of manifest entries.
#' @keywords internal
#' @noRd
.dest_send_label_get_manifest_source <- function(label) {
  .remote_get_manifest("project") |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(projr_version_get())
}

#' @title File listing helper for upload-missing
#' @description Determines which files exist locally but not remotely so they
#'   can be uploaded without touching existing remote content.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Named list containing `fn_source_extra` with files missing remotely.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_fn_upload_missing <- function(inspect,
                                                        version_comp,
                                                        remote_comp,
                                                        remote_pre,
                                                        type,
                                                        label) {
  # add all in `fn_souce_extra`, so need `fn_source` and `fn_dest`,
  # and then we diff them.
  fn_source <- .dest_send_label_get_fn_source(label)
  fn_dest <- .dest_send_label_get_fn_dest(
    inspect, version_comp, type, remote_comp, remote_pre, label
  )
  fn_source_extra <- setdiff(fn_source, fn_dest)
  list("fn_source_extra" = fn_source_extra)
}

#' @title Retrieve destination filenames for comparison
#' @description Returns the file listing that represents remote state, using
#'   hashes, manifests, or direct listings based on inspection choices.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Character vector of filenames representing the remote state.
#' @keywords internal
#' @noRd
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

#' @title File comparison helper for sync strategies
#' @description Builds full change tables between local and remote manifests to
#'   support `sync-diff` and `sync-purge` strategies.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Change table as produced by `.change_get_hash()` describing adds,
#'   deletes, and matches.
#' @keywords internal
#' @noRd
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

#' @title Retrieve remote manifest data
#' @description Pulls either hashes or recorded manifest entries for the remote
#'   side depending on whether manifests are trusted for the comparison version.
#' @inheritParams .dest_send_label_get_plan_fn
#' @return Manifest-like tibble describing remote state.
#' @keywords internal
#' @noRd
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

#' @title Hash-based remote manifest helper
#' @description Computes manifest rows by hashing the remote files when a trusted
#'   manifest is unavailable.
#' @inheritParams .dest_send_label_get_manifest_remote
#' @return Manifest-like tibble built from remote hashes.
#' @keywords internal
#' @noRd
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

#' @title Manifest-based remote helper
#' @description Retrieves the remote manifest file (if available) and filters it
#'   to the supplied label and version.
#' @inheritParams .dest_send_label_get_manifest_remote
#' @param version Optional version override (defaults to current project
#'   version).
#' @return Tibble/data.frame of manifest entries from the remote metadata.
#' @keywords internal
#' @noRd
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
                                             inspect,
                                             changelog) {
  plan <- switch(strategy,
    "upload-all" = .dest_send_label_get_plan_action_upload_all(
      fn_source, remote_dest, type, remote_pre, label, inspect
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

#' @title Determine whether a remote destination already exists
#' @description Normalizes GitHub remotes (handling empty variants) and checks
#'   if the destination asset/directory currently exists.
#' @inheritParams .dest_send_label_get_plan_action
#' @param is_remote_dest_empty Logical flag signalling whether the plan regards
#'   the upcoming remote as empty (affects GitHub filename selection).
#' @return Logical flag indicating whether the remote destination already
#'   exists.
#' @keywords internal
#' @noRd
.dest_send_label_check_remote_dest_actual_exists <-
  function(type,
           remote_dest,
           is_remote_dest_empty) {
    if (type == "github") {
      remote_dest <- if (is_remote_dest_empty) {
        remote_dest[["fn"]] <- gsub("\\.zip$", "-empty.zip", remote_dest[["fn"]])
        remote_dest
      } else {
        remote_dest
      }
    }
    !.remote_final_check_exists_direct(type, remote_dest)
  }

# upload-all
# ---------------------------
#' @title Build action plan for upload-all strategy
#' @description Prepares a plan that blindly uploads every tracked file and
#'   refreshes metadata regardless of existing remote contents.
#' @inheritParams .dest_send_label_get_plan_action
#' @return Named list describing adds, manifest updates, and version metadata.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_all <- function(fn_source, # nolint
                                                        remote_dest,
                                                        type,
                                                        remote_pre,
                                                        label) {
  # will add whatever is in `fn_source`, nothing else needed
  is_remote_dest_empty <-
    .dest_send_label_get_plan_action_upload_all_check_is_remote_dest_empty(
      type, remote_dest, fn_source
    )
  # ensure_remote_dest_exists
  # *latest*
  # clearly it must always exist
  # *archive*
  # It's upload-all, so we're basically always going to upload,
  # so yes here, too,
  ensure_remote_dest_exists <- TRUE
  # asterisk handling
  asterisk_treatment <-
    .dest_send_label_get_plan_action_upload_all_get_asterisk_treatment_force(
      type, remote_dest, is_remote_dest_empty
    )
  # get final version file
  args_version_file <- c(
    list(
      type = type, remote_pre = remote_pre, label = label,
      update_label = TRUE
    ),
    asterisk_treatment
  )
  version_file <- do.call(
    .dest_send_label_get_plan_action_version_file,
    args_version_file
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
    "is_remote_dest_empty" = is_remote_dest_empty,
    "ensure_remote_dest_exists" = ensure_remote_dest_exists
  )
}

#' @title Detect whether the remote destination is effectively empty
#' @description Handles GitHub's paired `-empty` assets and determines whether a
#'   remote should be treated as empty for planning purposes.
#' @inheritParams .dest_send_label_get_plan_action_upload_all
#' @return Logical flag indicating whether the destination is empty.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_all_check_is_remote_dest_empty <- function(type,
                                                                                   remote_dest,
                                                                                   fn_source) {
  # only needs to be tweaked for flat remotes, which at this stage
  # is only GitHub
  if (type != "github") {
    return(remote_dest)
  }
  # if there are files to upload, it's non-empty
  if (!.is_len_0(fn_source)) {
    FALSE
  }
  # now that it is 0, it's whatever is there already
  if (.remote_final_check_exists_direct("github", remote_dest)) {
    FALSE
  } else {
    TRUE
  }
}

#' @title Decide whether to force trusted version markers
#' @description Ensures version files gain or lose the trust asterisk when upload
#'   plans will create new remote artifacts.
#' @inheritParams .dest_send_label_get_plan_action_upload_all
#' @return Named list of arguments passed to
#'   `.dest_send_label_get_plan_action_version_file()`.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_all_get_asterisk_treatment_force <- function(type,
                                                                                     remote_dest,
                                                                                     is_remote_dest_empty) {
  # we trust it if we create it
  create <- !.dest_send_label_check_remote_dest_actual_exists(
    type, remote_dest, is_remote_dest_empty
  )
  if (create) {
    return(list(
      "asterisk_force_rm" = TRUE
    ))
  }
  list()
}

#' @title Update version file metadata for a destination
#' @description Loads the remote version file, updates the project version, and
#'   optionally refreshes the label entry while applying trust (asterisk)
#'   overrides.
#' @inheritParams .dest_send_label_get_plan_action_upload_all
#' @param update_label Logical flag indicating whether the label entry should be
#'   rewritten.
#' @param asterisk_force_add Logical flag forcing the untrusted indicator.
#' @param asterisk_force_rm Logical flag forcing removal of the untrusted
#'   indicator.
#' @return Character vector content of the updated version file.
#' @keywords internal
#' @noRd
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

#' @title Decide whether the version entry should be marked untrusted
#' @description Combines forced overrides with prior version file state to
#'   determine whether an asterisk should appear next to the label entry.
#' @inheritParams .dest_send_label_get_plan_action_version_file
#' @param asterisk_force_rm Logical flag forcing removal.
#' @param asterisk_force_add Logical flag forcing addition.
#' @param version_remote Character vector representation of the version file.
#' @return Logical flag indicating whether the entry should include an asterisk.
#' @keywords internal
#' @noRd
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

#' @title Inspect version file for trust markers
#' @description Checks whether the existing version file already marks the label
#'   as untrusted.
#' @inheritParams .dest_send_label_get_plan_action_version_file
#' @param version_file Character vector contents of the version file.
#' @return Logical flag indicating whether the label entry currently ends with
#'   an asterisk.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_version_file_check_untrusted <- function(version_file, # nolint
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
#' @inheritParams .dest_send_label_get_plan_action_upload_all
#' @param rm_existing Logical flag to remove any entries that will be overwritten.
#' @param rm_existing_all Logical flag removing all current rows for the version
#'   and label before appending.
#' @param rm_adding Logical flag removing files from the append set if they are
#'   unchanged (used by upload-missing).
#' @return Tibble/data.frame representing the manifest that should be written to
#'   the remote.
#' @keywords internal
#' @noRd
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

#' @title Remove manifest rows that will be replaced
#' @description Helper that strips any remote manifest rows overlapping with the
#'   current append set, ensuring the resulting manifest has unique filenames.
#' @param manifest_remote Existing remote manifest tibble.
#' @inheritParams .dest_send_label_get_plan_action_upload_all
#' @param manifest_append Manifest rows that will be appended.
#' @return Remote manifest tibble with overlapping rows removed.
#' @keywords internal
#' @noRd
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
#' @title Build action plan for upload-missing strategy
#' @description Generates an upload plan that only adds files missing remotely
#'   while preserving existing remote contents.
#' @inheritParams .dest_send_label_get_plan_action
#' @return Named list describing files to add plus manifest/version updates.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_missing <- function(fn_source_extra, # nolint
                                                            remote_dest,
                                                            type,
                                                            remote_pre,
                                                            label,
                                                            cue) {
  # START HERE
  browser()
  # will add whatever is in `fn_source`, nothing else needed
  is_remote_dest_empty <-
    .dest_send_label_get_plan_action_upload_missing_check_is_remote_dest_empty(
      type, remote_dest, fn_source_extra
    )
  fn_add <- c(fn_source_extra, fn_diff, fn_same)
  # check if we will actually upload
  will_upload <- cue == "always" ||
    !.is_len_0(fn_source_extra)
  # ensure_remote_dest_exists
  # *latest*
  # clearly it must always exist
  # *archive*
  # It's upload-missing, so we should always
  # hit, so yes, then, too.
  will_upload <- cue == "always" ||
    !.is_len_0(fn_source_extra) ||

  ensure_remote_dest_exists <- TRUE
  # asterisk handling
  asterisk_treatment <-
    .dest_send_label_get_plan_action_upload_missing_get_asterisk_treatment_force(
      type, remote_dest, is_remote_dest_empty, will_upload
    )
  # get final version file
  args_version_file <- c(
    list(
      type = type, remote_pre = remote_pre, label = label,
      update_label = TRUE
    ),
    asterisk_treatment
  )
  version_file <- do.call(
    .dest_send_label_get_plan_action_version_file,
    args_version_file
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
    "purge" = FALSE,
    "is_remote_dest_empty" = is_remote_dest_empty,
    "ensure_remote_dest_exists" = ensure_remote_dest_exists
  )
}

#' @title Force asterisk behaviour for upload-missing
#' @description Determines whether upload-missing workflows should force trust
#'   status changes when creating or reusing remote assets.
#' @inheritParams .dest_send_label_get_plan_action_upload_missing
#' @return Named list of overrides to pass to the version file helper.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_missing_get_asterisk_treatment_force <- function(type,
                                                                                         remote_dest,
                                                                                         is_remote_dest_empty) {

  # we trust it if we create it
  create <- !.dest_send_label_check_remote_dest_actual_exists(
    type, remote_dest, is_remote_dest_empty
  )
  if (create) {
    return(list(
      "asterisk_force_rm" = TRUE
    ))
  }
  list()
}


#' @title Determine emptiness for upload-missing plans
#' @description GitHub-specific helper that inspects the destination to see if
#'   an "empty" placeholder asset already exists before adding files.
#' @inheritParams .dest_send_label_get_plan_action_upload_missing
#' @return Logical flag indicating whether the destination is empty.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_upload_missing_check_is_remote_dest_empty <- function(type,
                                                                                       remote_dest,
                                                                                       fn_source_extra) {
  # only needs to be tweaked for flat remotes, which at this stage
  # is only GitHub
  if (type != "github") {
    return(FALSE)
  }
  # if there are files to upload, it's non-empty
  if (!.is_len_0(fn_source_extra)) {
    FALSE
  }
  # now that it is 0, it's whatever is there already
  if (.remote_final_check_exists_direct("github", remote_dest)) {
    FALSE
  } else {
    TRUE
  }
}


# sync
# ---------------------------

#' @title Dispatch sync strategy planning
#' @description Chooses between purge and diff implementations for sync
#'   strategies and forwards the required parameters.
#' @inheritParams .dest_send_label_get_plan_action
#' @return Named list describing the resulting action plan.
#' @keywords internal
#' @noRd
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
  if (strategy == "sync-purge") {
    .dest_send_label_get_plan_action_purge(
      type, remote_pre, remote_dest, label, version_comp,
      fn_source_extra, fn_dest_extra, fn_diff, fn_same, strategy
    )
  } else {
    .assert_has(strategy, "sync-diff")
    .dest_send_label_get_plan_action_diff(
      fn_source_extra, type, remote_pre, label, version_comp,
      fn_dest_extra, fn_diff, fn_same
    )
  }
}

# --- purge ---
#' @title Build purge variant of sync plan
#' @description Generates an action plan that mirrors the local state by
#'   removing and re-uploading files as required for `sync-purge`.
#' @inheritParams .dest_send_label_get_plan_action
#' @return Named list describing purge operations and metadata changes.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_purge <- function(type, # nolint
                                                   remote_pre,
                                                   remote_dest,
                                                   label,
                                                   version_comp,
                                                   fn_source_extra,
                                                   fn_dest_extra,
                                                   fn_diff,
                                                   fn_same,
                                                   strategy,
                                                   inspect) {
  fn_add <- c(fn_source_extra, fn_diff, fn_same)
  # it will obviously be zero if we're purging
  is_remote_empty <- .is_len_0(fn_add)
  # check if there is a change
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  # check if we will actually upload
  will_upload <- cue == "always" ||
    is_change ||
    # always upload if we don't trust the last version
    # (we went through all the effort to sync)
    is.null(version_comp)

  # the destination remote should not necessarily exist,
  # if we're archiving - only if there was a change from before.
  ensure_remote_dest_exists <- will_upload

  # purge operations should only be trusted if
  # we actually upload (which we do if "cue" is "always",
  # of if it didn't exist before and we're uploading now,
  trust_necessarily <- will_upload

  # don't trust if we did not inspect
  dont_trust <- !will_upload && inspect == "none"
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = TRUE,
    asterisk_force_rm = trust_necessarily,
    asterisk_force_add = dont_trust
  )
  manifest <- .dest_send_label_get_plan_action_manifest(
    type, remote_pre, label,
    rm_existing_all = TRUE
  )
  # ensure remote_dest exists in these scenarios:
  # *archive*
  # - adding files (happens automatically)
  # - removing files (does not happen automatically)
  list(
    fn_add = c(fn_source_extra, fn_diff, fn_same), # nolint
    fn_rm = character(0L),
    version = version_file,
    manifest = manifest,
    purge = TRUE,
    is_remote_dest_empty = is_remote_empty,
    ensure_remote_dest_exists = ensure_remote_dest_exists
  )
}

# --- diff ---
#' @title Build diff variant of sync plan
#' @description Generates an action plan that applies only the detected diffs
#'   between local and remote manifests.
#' @inheritParams .dest_send_label_get_plan_action
#' @return Named list describing diff-based uploads/removals plus metadata.
#' @keywords internal
#' @noRd
.dest_send_label_get_plan_action_diff <- function(fn_source_extra, # nolint
                                                  type,
                                                  remote_pre,
                                                  label,
                                                  version_comp,
                                                  fn_dest_extra,
                                                  fn_diff,
                                                  fn_same) {
  fn_add <- c(fn_source_extra, fn_diff, fn_same)
  # it will obviously be zero if we're purging
  is_remote_empty <- .is_len_0(fn_add)
  # check if there is a change
  is_change <- .is_len_pos(c(fn_source_extra, fn_dest_extra, fn_diff))
  # check if we will actually upload
  will_upload <- cue == "always" ||
    is_change ||
    !.dest_send_label_check_remote_dest_actual_exists(
      type, remote_dest, is_remote_empty
    )

  # remote must look like local, so it will be empty if
  # nothing is kept (the same or changed) or added
  is_remote_dest_empty <- .is_len_0(c(
    fn_same, fn_diff, fn_source_extra
  ))
  # remote_dest should exist if
  # - there is a difference (by definition),
  # - the previous version is not trusted or does not exist
  #   (version_comp is NULL), or
  # - we are supposed to always upload (cue is "always")
  ensure_remote_dest_exists <- is_change ||
    is.null(version_comp) ||
    cue == "always"
  # For diff operations, preserve untrusted status, unless we are uploading
  # to a new remote

  # We update the label if we upload afresh, which happens
  # if we've detected a change or if we always upload.
  update_label <- is_change || is.null(version_comp)

  # We remove the asterisk if we've compared against the actual
  # files on the remote.

  asterisk_force_rm <- !.remote_check_exists(type, )
  version_file <- .dest_send_label_get_plan_action_version_file(
    type, remote_pre, label,
    update_label = is_change || cue == "always",
    asterisk_force_rm =
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
    purge = FALSE,
    is_remote_dest_empty = is_remote_desty_empty,
    ensure_remote_dest_exists = ensure_remote_dest_exists
  )
}

# ==========================================================================
# Implement plan
# ==========================================================================

#' @title Execute a prepared send plan
#' @description Applies purge/create operations, performs file uploads/removals,
#'   manages GitHub empty placeholders, and writes manifest/version files back to
#'   the remote.
#' @inheritParams .dest_send_label_get_plan_action
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
                                            remote_pre,
                                            output_level = "std") {
  .cli_debug(
    "Content '{label}': Implementing upload plan",
    output_level = output_level
  )

  if (purge) {
    .cli_debug(
      "Content '{label}': Purging all existing remote files",
      output_level = output_level
    )
    .remote_final_empty(type, remote_dest)
  }

  if (create || type == "github") {
    .cli_debug(
      "Content '{label}': Creating/updating remote destination",
      output_level = output_level
    )
    # will create for OSF and local,
    # but not GitHub, so GitHub remote
    # is only created if there is a file to add
    # this means if the create is TRUE
    # and .is_len_pos(fn_add) is FALSE,
    # then we need to create an empty
    # GitHub remote. Could do that later, I guess?
    remote_dest <- .remote_final_get(
      type, id, label, structure, path, path_append_label, NULL
    )
  }

  if (.is_len_pos(fn_rm)) {
    .cli_debug(
      "Content '{label}': Removing {length(fn_rm)} file(s) from remote",
      output_level = output_level
    )
    .remote_file_rm(type, fn_rm, remote_dest, output_level)
  }

  if (.is_len_pos(fn_add)) {
    .cli_debug(
      "Content '{label}': Adding {length(fn_add)} file(s) to remote",
      output_level = output_level
    )
    .remote_file_add(type, remote_dest, path_dir_local, fn_add, output_level)
    if (type == "github") {
      # ensure that we remove the empty one
      remote_dest_empty <- remote_dest
      remote_dest_empty["fn"] <- gsub(
        "\\.zip$", "-empty.zip", remote_dest_empty[["fn"]]
      )
      if (.remote_check_exists_github_httr(
        remote_dest_empty[["tag"]], remote_dest_empty[["fn"]]
      )) {
        .remote_final_empty_github(remote_dest_empty)
      }
    }
  }

  if (type == "github") {
    remote_exists <- .remote_final_check_exists_direct("github", remote_dest)
    remote_dest_empty <- remote_dest
    remote_dest_empty["fn"] <- gsub(
      "\\.zip$", "-empty.zip", remote_dest_empty[["fn"]]
    )
    remote_empty_exists <- .remote_final_check_exists_direct(
      "github", remote_dest_empty
    )
    if (remote_exists) {
      .cli_debug(
        "Content '{label}': Remote destination exists after upload, so removing empty placeholder", # nolint
        output_level = output_level
      )
      if (remote_empty_exists) {
        .remote_final_empty("github", remote_dest_empty)
      }
    } else {
      # will need to create an empty placeholder, if the
      if (create || structure == "latest") {
        .cli_debug(
          "Content '{label}': Remote destination does not exist after upload, so ensuring an empty placeholder", # nolint
          output_level = output_level
        )
        if (!remote_empty_exists) {
          path_fn <- file.create(
            file.path(tempdir(), "projr-empty"),
            showWarnings = FALSE
          )
          .remote_file_add(
            "github", remote_dest_empty, tempdir(), "projr-empty", output_level
          )
        }
      }
    }
  }

  # need to use remote_pre and just add an individual file
  .cli_debug(
    "Content '{label}': Updating remote manifest and version files",
    output_level = output_level
  )
  .remote_write_manifest(type, remote_pre, manifest)
  .remote_write_version_file(type, remote_pre, version_file)
  if (changelog) {
    .cli_debug(
      "Content '{label}': Writing changelog to remote",
      output_level = output_level
    )
    .remote_write_changelog(type, remote_pre)
  }
  invisible(TRUE)
}