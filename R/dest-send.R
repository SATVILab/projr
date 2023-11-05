# send to all remotes when they're the destination
# --------------------------

.projr_dest_send <- function(bump_component) {
  # consider early exit
  # ------------------
  if (!.projr_dest_send_check(bump_component = bump_component)) {
    return(invisible(FALSE))
  }

  # loop over remotes
  for (remote in .projr_dest_send_get()) {
    .projr_dest_send_remote(
      remote_type = remote,
      bump_component = bump_component
    )
  }
}

.projr_dest_send_check <- function(bump_component) {
  # output_run
  output_run <- .projr_run_type_check(bump_component = bump_component)
  if (!output_run) {
    return(invisible(FALSE))
  }
  length(.projr_dest_send_get()) > 0
}

.projr_dest_send_get <- function() {
  yml_projr_build <- projr_yml_get_unchecked()[["build"]]
  dest_vec <- c("github", "local", "osf")
  dest_vec[dest_vec %in% names(yml_projr_build)]
}

# send to one remote
# -----------------------------

.projr_dest_send_remote <- function(remote_type,
                                    bump_component) {
  yml_projr_remote <- projr_yml_get_unchecked()[["build"]][[remote_type]]
  for (x in names(yml_projr_remote)) {
    .projr_dest_send_instance(
      remote_type = remote_type,
      bump_component = bump_component,
      remote_name = x,
      yml_remote = yml_projr_remote[[x]]
    )
  }
}

# send to one instance of a remote
# -----------------------------

.projr_dest_send_instance <- function(remote_type,
                                      bump_component,
                                      remote_name,
                                      yml_remote) {
  # set defaults if not set for yml_remote
  yml_remote <- .projr_dest_send_yml_remote_complete(
    yml_remote = yml_remote, remote_type = remote_type
  )

  # consider early exit
  cue_met <- .projr_cue_check(
    cue = yml_remote[["cue"]], bump_component = bump_component
  )

  if (!cue_met) {
    # recurse over components for OSF,
    # as their cue might not be unmet
    yml_component <- yml_remote[["component"]]
    if (!is.null(yml_component)) {
      return(
        .projr_dest_send_instance(
          remote_type = remote_type,
          bump_component = bump_component,
          remote_name = remote_name,
          yml_remote = yml_component
        )
      )
    } else {
      return(invisible(FALSE))
    }
  }

  # find what needs to be sent and removed
  # first, as if we attempt to find out
  # what has changed after already uploading
  # for another type of content we could have problems
  # (especially if we compare versions on the
  #  basis of what's already uploaded)
  output_run <- .projr_run_type_check(bump_component = bump_component)
  args_list <- list(
    output_run = output_run,
    remote_type = remote_type,
    remote_name = remote_name,
    yml_remote = yml_remote
  )
  change_list <- do.call(.projr_dest_change_get_content, args_list)

  .projr_dest_send_content(
    remote_type = remote_type,
    remote_name = remote_name,
    yml_remote = yml_remote,
    change_list = change_list
  )

  # send changes
  # recurse over components for OSF
  yml_component <- yml_remote[["component"]]
  if (!is.null(yml_component)) {
    .projr_dest_send_instance(
      remote_type = remote_type,
      bump_component = bump_component,
      remote_name = remote_name,
      yml_remote = yml_component
    )
  }
  invisible(TRUE)
}

# ================================
# change functions
# ================================

# for multiple labels
# ------------------------
.projr_dest_change_get_content <- function(content,
                                           output_run,
                                           remote_type,
                                           remote_name,
                                           yml_remote) {
  args_list <- list(
    output_run = output_run, remote_type = remote_type,
    remote_name = remote_name, yml_remote = yml_remote
  )
  lapply(content, function(x) {
    do.call(.projr_dest_change_get_label, list(label = x) |> append(args_list))
  }) |>
    stats::setNames(content)
}

# for a single label
# ------------------------

.projr_dest_change_get_label <- function(label,
                                         output_run,
                                         remote_type,
                                         remote_base,
                                         remote_final,
                                         path_remote_rel,
                                         version_source) {
  # this will assume that the manifest knows
  # about what's online
  # TODO: make this function differently dependening
  # on what sync-approach and remote-structure are
  # (in addition to version source)
  switch(version_source,
    "manifest" = .projr_change_get_manifest(label = label),
    "file" = .projr_change_get_file(
      output_run = output_run,
      remote_type_pre = remote_type,
      remote_base_pre = remote_base,
      remote_final_pre = remote_final,
      path_remote_rel_pre = path_remote_rel,
      label_post = label
    ),
    stop(paste0("version_source '", version_source, "' not recognized"))
  )
}

# ================================
# send functions
# ================================

# for multiple labels
# ------------------------

.projr_dest_send_content <- function(remote_type,
                                     remote_name,
                                     yml_remote,
                                     change_list) {
  # may have to consider what to delete before uploading
  # all at once
  # okay, so, I'm stuck because I don't actually have an idea yet.
  # all right, let's start talking.
  # okay, so what's the story.
  # so, so, what do we know now:
  # - what files have been removed, added or changed since last upload
  # - remote type (local, osf, github)
  # - remote name (e.g. "My OSF project" or "Project inputs")
  # - remote structure (e.g. "latest" or "version")
  #   - Question: does this matter for GitHub?
  # - sync approach (upload-all, upload-missing, sync-using-deletion or
  #   sync-using version)
  # - conflict
  #   - what to do about file conflicts
  # - version source does not matter now, as we already have decided
  #   - well, wait - what if we just want to send everything?
  #   - in that case, presumably the change_list should be empty - don't
  #     bother getting it
  # - OSF-specific:
  #  - {remote_base (id), path and path_append_label} OR path_final
  #    (probably the former only)
  # - GitHub-specific:
  #   - body
  # - local-specific:
  #   - nothing
  # okay, so it seems to depend a lot on the remote_structure,
  # the sync approach and the version_source
  # ya, when doing this for OSF the first thing I did
  # was to get the remote structure
  # then I deleted things if we used sync-using-deletion
  # so, we actually only get the change list if the
  # version-source is sync-using-version. Otherwise,
  # we just either upload everything or delete everything.
  # Okay, so now we just focus on the process for adding
}

# for a single label
# ------------------------

.projr_dest_send_label <- function(remote_type,
                                   remote_name,
                                   yml_remote,
                                   label,
                                   output_run,
                                   change_list) {
  # get place where files are taken from and
  # where they should go to
  path_dir_local <- projr_dir_get(label, output_safe = !output_run)
  remote_final <- .projr_remote_final_get(
    remote_type = remote_type,
    remote_name = remote_name,
    label = label,
    output_run = output_run,
    yml_remote = yml_remote
  )

  # empty remote_final if we're using sync-using-deletion
  .projr_remote_empty_sync_using_deletion <- function() {
    if (yml_remote[["upload"]][["sync-approach"]] != "sync-using-deletion") {
      return(invisible(NULL))
    }
    .projr_remote_empty(
      remote_final = remote_final,
      remote_type = remote_type,
      remote_structure = yml_remote[["remote-structure"]]
    )
  }

  # delete unused versioned remote directories if creatd
  .projr_remote_rm_final_empty(
    remote_final = remote_final,
    remote_type = remote_type,
    remote_structure = yml_remote[["remote-structure"]]
  )
}

# ================================
# clear function
# ================================

# delete the remote if it's empty and it's versioned
.projr_dest_send_label_clear <- function(remote,
                                         label,
                                         remote_structure,
                                         sync_approach,
                                         remote_type) {
  if (!sync_approach == "sync-using-deletion") {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "local" = .projr_dest_send_label_clear_local(
      remote = remote,
      label = label,
      remote_structure = remote_structure
    ),
  )
}

# ================================
# complete yml_remote
# ================================

# main function
.projr_dest_send_yml_remote_complete <- function(yml_remote,
                                                 remote_type) {
  yml_remote[["remote_structure"]] <-
    yml_remote[["remote_structure"]] %||% "version"
  yml_remote[["upload"]][["cue"]] <-
    yml_remote[["upload"]][["cue"]] %||% "patch"
  yml_remote[["upload"]][["version_source"]] <-
    yml_remote[["upload"]][["version_source"]] %||% "manifest"
  # default will depend on remote type, as GitHub
  # uses sync-using-deletion by default
  yml_remote[["upload"]][["sync-approach"]] <-
    .projr_dest_send_par_get_sync_approach(
      sync_approach = yml_remote[["upload"]][["sync-approach"]],
      version_source = yml_remote[["upload"]][["version_source"]],
      remote_type = remote_type
    )
  yml_remote[["upload"]][["conflict"]] <- "overwrite"
  yml_remote
}

# ----------------------
# individual parameters
# ----------------------

# sync-approach
.projr_dest_send_par_get_sync_approach <- function(sync_approach,
                                                   version_source,
                                                   remote_type) {
  switch(remote_type,
    "local" = ,
    "osf" = .projr_dest_send_par_get_sync_approach_hierarchy(
      sync_approach = sync_approach,
      version_source = version_source
    ),
    "github" =
      .projr_dest_send_par_get_sync_approach_github(
        sync_approach = sync_approach,
        version_source = version_source
      )
  )
}

.projr_dest_send_par_get_sync_approach_hierarchy <- function(sync_approach,
                                                             version_source) {
  # default is sync-using-version
  sync_approach <- sync_approach %||% "sync-using-version"
  version_source <- version_source %||% "manifest"
  # if we cannot use versioning but must sync, the only option is
  # sync-using-deletion
  if (version_source == "none" && sync_approach == "sync-using-version") {
    return("sync-using-deletion")
  }
  sync_approach
}

.projr_dest_send_par_get_sync_approach_github <- function(sync_approach,
                                                          version_source) {
  # default is sync-using-version, for speeds
  sync_approach <- sync_approach %||% "sync-using-version"
  version_source <- version_source %||% "manifest"
  # only if we're allowed to use versioning and we're syncing
  # do we use sync-using-version. Otherwise, we use sync-using-deletion
  if (sync_approach == "sync-using-version" && version_source != "none") {
    return("sync-using-version")
  }
  "sync-using-deletion"
}
