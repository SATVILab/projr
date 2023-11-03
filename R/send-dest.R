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
  dest_vec <- c("github-release", "local", "osf")
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
  # - remote type (local, osf, github-release)
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
  #  - {remote_base, path and path_append_label} OR path_final
  #    (probably the former only)
  # - GitHub-specific:
  #   - body
  # - local-specific:
  #   - nothing
  #
}

# for a single label
# ------------------------

.projr_dest_send_label <- function(remote_type,
                                   remote_name,
                                   yml_remote,
                                   label,
                                   change_list) {

}

# ================================
# clear function
# ================================

.projr_dest_send_content_clear <- function(remote_type,
                                           remote_name,
                                           yml_remote,
                                           change_list) {
  # may have to consider what to delete before uploading
  # all at once
}
