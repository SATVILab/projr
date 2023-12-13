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
  yml_projr_build <- .projr_yml_build_get(NULL)
  dest_vec <- c("github", "local", "osf")
  dest_vec[dest_vec %in% names(yml_projr_build)]
}

# send to one remote
# -----------------------------

.projr_dest_send_remote <- function(remote_type,
                                    bump_component) {
  yml_projr_remote <- .projr_yml_build_get(NULL)[[remote_type]]
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
  if (!.projr_state_cue(yml_remote[["cue"]], bump_component)) {
    # recurse over components for OSF,
    # as their cue might not be unmet
    if (.projr_state_null(yml_remote[["component"]])) {
      return(
        .projr_dest_send_instance(
          remote_type = remote_type,
          bump_component = bump_component,
          remote_name = remote_name,
          yml_remote = yml_remote[["component"]]
        )
      )
    } else {
      return(invisible(FALSE))
    }
  }

  output_run <- .projr_run_type_check(bump_component = bump_component)

  .projr_dest_send_content(
    remote_type = remote_type,
    remote_name = remote_name,
    yml_remote = yml_remote,
    output_run = output_run
  )

  # send changes
  # recurse over components for OSF
  if (!.projr_state_null(yml_remote[["component"]])) {
    .projr_dest_send_instance(
      remote_type = remote_type,
      output_run = output_run,
      remote_name = remote_name,
      yml_remote = yml_remote[["component"]]
    )
  }
  invisible(TRUE)
}

# ================================
# send functions
# ================================

# for multiple labels
# ------------------------

.projr_dest_send_content <- function(remote_type,
                                     remote_name,
                                     yml_remote,
                                     output_run,
                                     change_list) {
  label_vec <- yml_remote[["content"]]
  for (i in seq_along(label_vec)) {
    .projr_dest_send_label(
      remote_type = remote_type,
      remote_name = remote_name,
      yml_remote = yml_remote,
      label = label_vec[[i]],
      output_run = output_run
    )
  }
}

# for a single label
# ------------------------

.projr_dest_send_label <- function(remote_type,
                                   remote_name,
                                   yml_remote,
                                   label,
                                   output_run) {
  # get place where files are taken from and
  # where they should go to
  path_dir_local <- projr_dir_get(label, safe = !output_run)
  remote_final <- .projr_remote_final_get(
    remote_type = remote_type,
    remote_name = remote_name,
    label = label,
    id = yml_remote[["id"]],
    path = yml_remote[["path"]],
    path_append_label = yml_remote[["path-append-label"]],
    structure = yml_remote[["remote-structure"]]
  )

  # get overall type of plan
  plan <- .projr_dest_send_get_plan(
    structure = yml_remote[["remote-structure"]],
    remote_type = remote_type,
    version_source = yml_remote[["version-source"]],
    sync_approach = yml_remote[["upload"]][["sync-approach"]]
  )

  # get details of what files to remove and add
  plan_list_detail <- .projr_dest_send_get_plan_detail(
    plan = plan,
    label = label,
    path_dir_local = path_dir_local,
    remote_final = remote_final,
    remote_type = remote_type,
    version_source = yml_remote[["version-source"]],
    structure = yml_remote[["remote-structure"]],
    sync_approach = yml_remote[["upload"]][["sync-approach"]]
  )

  # remove and upload files as stated
  .projr_plan_implement(
    plan = plan,
    plan_detail = plan_list_detail,
    remote_final = remote_final,
    remote_type = remote_type
  )
}

# ================================
# clear function
# ================================

# delete the remote if it's empty and it's versioned
.projr_dest_send_label_clear <- function(remote,
                                         label,
                                         structure,
                                         sync_approach,
                                         remote_type) {
  if (!sync_approach == "sync-using-deletion") {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "local" = .projr_dest_send_label_clear_local(
      remote = remote,
      label = label,
      structure = structure
    ),
  )
}

# ================================
# complete yml_remote
# ================================

# main function
.projr_dest_send_yml_remote_complete <- function(yml_remote,
                                                 remote_type) {
  yml_remote[["structure"]] <-
    yml_remote[["structure"]] %||% "version"
  yml_remote[["upload"]][["cue"]] <-
    yml_remote[["upload"]][["cue"]] %||% "patch"
  yml_remote[["upload"]][["version-source"]] <-
    yml_remote[["upload"]][["version-source"]] %||% "manifest"
  # default will depend on remote type, as GitHub
  # uses sync-using-deletion by default
  yml_remote[["upload"]][["sync-approach"]] <-
    .projr_dest_send_par_get_sync_approach(
      sync_approach = yml_remote[["upload"]][["sync-approach"]],
      version_source = yml_remote[["upload"]][["version-source"]],
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
  # do we use sync-using-version (which is the default).
  # Otherwise, we use sync-using-deletion
  if (sync_approach == "sync-using-version" && version_source != "none") {
    return("sync-using-version")
  }
  "sync-using-deletion"
}

# ================================
# empty the remote if using sync-using-deletion
# ================================

# empty remote_final if we're using sync-using-deletion
.projr_remote_file_rm_all_sync_using_deletion <- function(remote_final,
                                                          remote_type,
                                                          sync_approach) {
  if (sync_approach != "sync-using-deletion") {
    return(invisible(FALSE))
  }
  .projr_remote_file_rm_all(remote = remote_final, remote_type = remote_type)
}
