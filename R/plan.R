# =====================
# make plan
# =====================
.projr_dest_send_get_plan <- function(remote_structure,
                                      remote_type,
                                      version_source,
                                      sync_approach) {
  switch(version_source,
    "none" = .projr_dest_send_get_plan_none(
      sync_approach = sync_approach
    ),
    "manifest" = ,
    "file" = .projr_dest_send_get_plan_man_file(
      remote_structure = remote_structure,
      remote_type = remote_type,
      sync_approach = sync_approach
    )
  )
}

# ------------------------
# version source
# ------------------------

# basic if not using any
.projr_dest_send_get_plan_none <- function(sync_approach) {
  switch(sync_approach,
    "upload-all" = ,
    "upload-missing" = "add_all",
    "sync-using-deletion" = ,
    "sync-using-version" = "delete_add_all"
  )
}

# more complex if using one
.projr_dest_send_get_plan_man_file <- function(remote_structure,
                                               remote_type,
                                               sync_approach) {
  switch(remote_type,
    "github" = .projr_dest_send_get_plan_flat(
      sync_approach = sync_approach
    ),
    "local" = ,
    "osf" = .projr_dest_send_get_plan_hier(
      remote_structure = remote_structure,
      sync_approach = sync_approach
    )
  )
}

# ------------------------
# sync approach
# ------------------------

# flat remotes
.projr_dest_send_get_plan_flat <- function(sync_approach) {
  switch(sync_approach,
    "upload-all" = "add_all",
    "upload-missing" = "add_all_if_missing",
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = "delete_add_all_if_change"
  )
}

# hierarchical remotes
.projr_dest_send_get_plan_hier <- function(remote_structure,
                                           sync_approach) {
  switch(sync_approach,
    "upload-all" = "add_all",
    "upload-missing" = .projr_dest_send_get_plan_hier_missing(
      remote_structure = remote_structure
    ),
    "sync-using-deletion" = "delete_add_all",
    "sync-using-version" = .projr_dest_send_get_plan_hier_version(
      remote_structure = remote_structure
    )
  )
}

# ------------------------
# remote structure
# ------------------------

.projr_dest_send_get_plan_hier_missing <- function(remote_structure) {
  switch(remote_structure,
    "latest" = "add_missing",
    "version" = "add_all"
  )
}

.projr_dest_send_get_plan_hier_version <- function(remote_structure) {
  switch(remote_structure,
    "latest" = "change",
    "version" = "delete_add_all_if_change"
  )
}

# unique plans:
# add_all
# add_missing
# change
# add_all_if_missing
# delete_add_all
# delete_add_all_if_change

# ========================
# get plan details
# ========================

.projr_dest_send_get_plan_detail <- function(plan,
                                             label,
                                             path_dir_local,
                                             remote_final,
                                             remote_type,
                                             version_source,
                                             remote_structure,
                                             sync_approach) {
  switch(plan,
    "add_all" = ,
    "delete_add_all" = .projr_dest_send_get_plan_detail_add_all(
      path_dir_local = path_dir_local
    ),
    "add_missing" = .projr_dest_send_get_plan_detail_add_missing(
      path_dir_local = path_dir_local,
      remote_final = remote_final,
      remote_type = remote_type
    ),
    "add_all_if_missing" = .projr_dest_send_get_plan_detail_add_all_if_missing(
      path_dir_local = path_dir_local,
      remote_final = remote_final,
      remote_type = remote_type
    ),
    "change" = .projr_dest_send_get_plan_detail_change(
      path_dir_local = path_dir_local,
      remote_final = remote_final,
      remote_type = remote_type,
      version_source = version_source
    )
  )
}

# ------------------------
# get plan details: add all
# ------------------------

.projr_dest_send_get_plan_detail_add_all <- function(path_dir_local) {
  list("add" = list.files(path_dir_local, recursive = TRUE), rm = character())
}

# ------------------------
# get plan details: add missing
# ------------------------

.projr_dest_send_get_plan_detail_add_missing <- function(path_dir_local,
                                                         remote_final,
                                                         remote_type) {
  path_dir_save_local <- file.path(tempdir(), "remote", signif(rnorm(1)))
  fn_vec_remote <- .projr_remote_file_ls(
    remote_type = remote_type,
    remote = remote_final,
    path_dir_save_local = path_dir_save_local
  )
  fn_vec_local <- list.files(path_dir_local, recursive = TRUE)
  fn_vec_add <- setdiff(fn_vec_local, fn_vec_remote)
  unlink(path_dir_save_local, recursive = TRUE)
  list("add" = fn_vec_add, "rm" = character())
}

# ------------------------
# get plan details: add all if missing
# ------------------------

.projr_dest_send_get_plan_detail_add_all_if_missing <- function(path_dir_local,
                                                                remote_final,
                                                                remote_type) {
  fn_vec_add <- .projr_dest_send_get_plan_detail_add_missing(
    path_dir_local = path_dir_local,
    remote_final = remote_final,
    remote_type = remote_type
  )$add
  if (length(fn_vec_add) > 0L) {
    fn_vec_add <- list.files(path_dir_local, recursive = TRUE)
  }
  list("add" = fn_vec_add, "rm" = character())
}

# ------------------------
# get plan details: change
# ------------------------

.projr_dest_send_get_plan_detail_change <- function(remote_final,
                                                    remote_type,
                                                    version_source,
                                                    label,
                                                    output_run) {
  change_list <- .projr_change_get(
    label = label,
    outptut_run = output_run,
    remote_final = remote_final,
    remote_type = remote_type,
    version_source = version_source
  )
  list(
    "add" = c(change_list[["kept_changed"]], change_list[["added"]]),
    "rm" = change_list[["removed"]]
  )
}

# ------------------------
# get plan details: delete and add all if change
# ------------------------

.projr_dest_send_get_plan_detail_delete_add_all_if_change <- function(remote_final,
                                                                      remote_type,
                                                                      version_source,
                                                                      label,
                                                                      output_run) {
  plan_list <- .projr_dest_send_get_plan_detail_change(
    remote_final = remote_final,
    remote_type = remote_type,
    version_source = version_source,
    label = label,
    output_run = output_run
  )
  if (length(plan_list[["add"]]) == 0L && length(plan_list[["rm"]]) == 0L) {
    return(plan_list)
  }
  path_dir_local <- projr_dir_get(label, output_safe = !output_run)
  list("add" = list.files(path_dir_local, recursive = TRUE), rm = character())
}

# ========================
# remote files from remote
# ========================

# all files according to plan
.projr_remote_file_rm_all_if_plan <- function(plan,
                                              remote_final,
                                              remote_type) {
  if (!plan %in% c("delete_add_all", "delete_add_all_if_change")) {
    return(invisible(FALSE))
  }
  .projr_remote_file_rm_all(remote = remote_final, remote_type = remote_type)
}

# ========================
# implement plan
# ========================

.projr_plan_implement <- function(plan,
                                  plan_detail,
                                  remote_final,
                                  remote_type,
                                  path_dir_local) {
  # clear if using sync_using_deletion
  .projr_remote_file_rm_all_if_plan(
    plan = plan,
    remote_final = remote_final,
    remote_type = remote_type
  )

  # remove any files that need to be removed
  .projr_remote_rm_file(
    fn = plan_detail[["rm"]],
    remote = remote_final,
    remote_type = remote_type
  )

  # add any files that need to be added
  # START HERE: fix arguments (doesn't match what .projr_remote_add_file
  # is defined to use as parameters)
  .projr_remote_add_file(
    fn = plan_detail[["add"]],
    path_dir_local = path_dir_local,
    remote = remote_final,
    remote_type = remote_type
  )

  # delete unused versioned remote directories if creatd
  .projr_remote_file_rm_all_if_plan(
    plan = plan,
    remote_final = remote_final,
    remote_type = remote_type
  )
}
