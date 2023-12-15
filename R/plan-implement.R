.projr_plan_implement <- function(plan,
                                  plan_detail,
                                  remote,
                                  type,
                                  structure,
                                  path_dir_local) {
  # clear if needed
  .projr_remote_file_rm_all_if_plan(plan, type, remote)

  # remove any files that need to be removed
  .projr_remote_file_rm(
    type = type,
    fn = plan_detail[["rm"]],
    remote = remote
  )

  # add any files that need to be added
  .projr_remote_file_add(type, remote, path_dir_local, plan_detail[["add"]])

  # delete unused versioned remote directories if creatd
  .projr_remote_rm_final_if_empty(type, remote, structure)
}


# clear remote if up-front clearance is requested
.projr_remote_file_rm_all_if_plan <- function(plan,
                                              type,
                                              remote) {
  if (!.projr_remote_file_rm_all_if_plan_check(plan)) {
    return(invisible(FALSE))
  }
  .projr_remote_file_rm_all(type, remote)
}

.projr_remote_file_rm_all_if_plan_check <- function(plan) {
  grepl("^delete", plan)
}
