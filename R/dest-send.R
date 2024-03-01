# send to all remotes when they're the destination
# --------------------------

.projr_dest_send <- function(bump_component) {
  # consider early exit
  # ------------------
  if (!.projr_dest_send_check(bump_component)) {
    return(invisible(FALSE))
  }

  # loop over types of remotes
  for (type in .projr_dest_send_get_type()) {
    .projr_dest_send_type(type, bump_component)
  }
}

.projr_dest_send_check <- function(bump_component) {
  # output_run
  output_run <- .projr_build_get_output_run(bump_component)
  if (!output_run) {
    return(invisible(FALSE))
  }
  length(.projr_dest_send_get_type()) > 0
}

.projr_dest_send_get_type <- function() {
  .projr_dest_send_get_type_opt()[
    .projr_dest_send_get_type_opt() %in% names(.projr_yml_build_get(NULL))
  ]
}

.projr_dest_send_get_type_opt <- function() {
  c("local", "github", "osf")
}

# send to one type of remote
# -----------------------------

.projr_dest_send_type <- function(type,
                                  bump_component) {
  for (x in names(.projr_yml_dest_get_type(type, NULL))) {
    .projr_dest_send_title(x, type, bump_component)
  }
}

# send to one label of a remote
# -----------------------------

.projr_dest_send_title <- function(title, type, bump_component) {
  force(title)

  if (!.projr_dest_send_title_check(title, type, bump_component)) {
    return(invisible(FALSE))
  }

  for (x in .projr_yml_dest_get_title(title, type, NULL)[["content"]]) {
    .projr_dest_send_label(
      x, title, type, .projr_build_get_output_run(bump_component)
    )
  }
  invisible(TRUE)
}

.projr_dest_send_title_check <- function(title, type, bump_component) {
  force(title)
  .projr_yml_dest_get_title_complete(title, type, NULL)[["cue"]] |>
    .is_cue(bump_component)
}

# ================================
# send functions
# ================================

# for a single label
# ------------------------

.projr_dest_send_label <- function(label,
                                   title,
                                   type,
                                   output_run) {
  force(title)
  # where they should go to
  path_dir_local <- projr_path_get_dir(label, safe = !output_run)
  yml_title <- .projr_yml_dest_get_title_complete(title, type, NULL)
  remote <- .projr_remote_get_final(
    type = type,
    id = yml_title[["id"]],
    path = yml_title[["path"]],
    path_append_label = yml_title[["path-append-label"]],
    label = label,
    structure = yml_title[["structure"]]
  )

  # get overall type of plan
  plan <- .projr_dest_send_get_plan(
    version_source = yml_title[["send"]][["version-source"]],
    sync_approach = yml_title[["send"]][["sync-approach"]],
    type = type,
    structure = yml_title[["structure"]]
  )

  # get details of what files to remove and add
  plan_list_detail <- .projr_dest_send_get_plan_detail(
    plan = plan,
    label = label,
    path_dir_local = path_dir_local,
    remote = remote,
    type = type,
    version_source = yml_title[["send"]][["version-source"]]
  )

  # remove and upload files as stated
  .projr_plan_implement(
    plan = plan,
    plan_detail = plan_list_detail,
    remote = remote,
    type = type,
    structure = yml_title[["structure"]],
    path_dir_local = path_dir_local,
    conflict = yml_title[["send"]][["conflict"]]
  )
}

# ================================
# clear function
# ================================

# delete the remote if it's empty and it's versioned
.projr_dest_send_label_clear <- function(sync_approach,
                                         type,
                                         remote,
                                         structure) {
  if (!.is_opt(sync_approach, "sync-using-deletion")) {
    return(invisible(FALSE))
  }
  .projr_remote_rm_final_if_empty(type, remote, structure)
}
