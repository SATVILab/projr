.plan_implement <- function(plan,
                                  plan_detail,
                                  remote,
                                  type,
                                  structure,
                                  path_dir_local,
                                  conflict) {
  # clear if needed
  .remote_file_rm_all_if_plan(plan, plan_detail, type, remote)

  # remove any files that need to be removed
  .remote_file_rm(
    type = type,
    fn = plan_detail[["rm"]],
    remote = remote
  )

  # add any files that need to be added
  .remote_file_add(
    type, remote, path_dir_local, plan_detail[["add"]], conflict
  )

  # delete unused versioned remote directories if created
  .remote_rm_final_if_empty(type, remote, structure)
}


# clear remote if up-front clearance is requested
.remote_file_rm_all_if_plan <- function(plan,
                                              plan_detail,
                                              type,
                                              remote) {
  if (!.remote_file_rm_all_if_plan_check(plan, plan_detail)) {
    return(invisible(FALSE))
  }
  .remote_file_rm_all(type, remote)
}

.remote_file_rm_all_if_plan_check <- function(plan, plan_detail) {
  grepl("^delete", plan) &&
    (.is_len_pos(plan_detail[["rm"]]) || .is_len_pos(plan_detail[["add"]]))
}
