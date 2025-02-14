.dest_send_get_plan_detail <- function(plan,
                                             label,
                                             path_dir_local,
                                             remote,
                                             type,
                                             inspect) {
  # here we want to find out what files to add and what files to remove
  # from the remote
  switch(plan,
    # for both of these, we just list the local files
    # (deletion happens later)
    "add_all" = ,
    "delete_add_all" = .dest_send_get_plan_detail_add_all(
      path_dir_local
    ),
    # here we also need to list what's in the remote, and compare
    "add_missing" = .dest_send_get_plan_detail_add_missing(
      path_dir_local = path_dir_local,
      remote = remote,
      type = type
    ),
    # here we do the same as above, except if anything's missing
    # at all we add anything rather than just what's missing
    "add_all_if_missing" = .dest_send_get_plan_detail_add_all_if_missing(
      path_dir_local = path_dir_local,
      remote = remote,
      type = type
    ),
    # here we add and remove based on what's changed.
    # additions are not just what's missing, but also what's changed.
    # and removals are what's been removed.
    "change" = .dest_send_get_plan_detail_change(
      path_dir_local = path_dir_local,
      remote = remote,
      type = type,
      label = label,
      inspect = inspect
    ),
    "delete_add_all_if_change" = .dest_send_get_plan_detail_delete_add_all_if_change( # nolint
      path_dir_local = path_dir_local,
      remote = remote,
      type = type,
      inspect = inspect,
      label = label
    ),
    stop(paste0("plan '", plan, "' not supported"), call. = FALSE)
  )
}

# ------------------------
# get plan details: add all
# ------------------------

.dest_send_get_plan_detail_add_all <- function(path_dir_local) {
  list("add" = .file_ls(path_dir_local), rm = character())
}

# ------------------------
# get plan details: add missing
# ------------------------

.dest_send_get_plan_detail_add_missing <- function(path_dir_local,
                                                         remote,
                                                         type) {
  path_dir_local_remote <- .dir_create_tmp_random()
  fn_vec_remote <- .remote_file_ls(type, remote)
  fn_vec_local <- .file_ls(path_dir_local)
  fn_vec_add <- setdiff(fn_vec_local, fn_vec_remote)
  .dir_rm(path_dir_local_remote)
  list("add" = fn_vec_add, "rm" = character())
}

# ------------------------
# get plan details: add all if missing
# ------------------------

.dest_send_get_plan_detail_add_all_if_missing <- function(path_dir_local,
                                                                remote,
                                                                type) {
  fn_vec_add <- .dest_send_get_plan_detail_add_missing(
    path_dir_local = path_dir_local,
    remote = remote,
    type = type
  )[["add"]]
  if (.is_len_pos(fn_vec_add)) {
    fn_vec_add <- .file_ls(path_dir_local)
  }
  list("add" = fn_vec_add, "rm" = character())
}

# ------------------------
# get plan details: change
# ------------------------

.dest_send_get_plan_detail_change <- function(remote,
                                                    type,
                                                    label,
                                                    inspect,
                                                    path_dir_local) {
  change_list <- .change_get(
    label = label,
    path_dir_local = path_dir_local,
    inspect = inspect,
    type = type,
    remote = remote
  )
  list(
    "add" = c(
      change_list[["kept_changed"]][["fn"]] %@@% character(),
      change_list[["added"]][["fn"]] %@@% character()
    ) |>
      as.character(),
    "rm" = change_list[["removed"]][["fn"]] %@@% character() |> as.character()
  )
}

# ------------------------
# get plan details: delete and add all if change
# ------------------------

.dest_send_get_plan_detail_delete_add_all_if_change <-
  function(remote,
           type,
           inspect,
           path_dir_local,
           label) {
    plan_list <- .dest_send_get_plan_detail_change(
      remote = remote,
      type = type,
      inspect = inspect,
      path_dir_local = path_dir_local,
      label = label
    )
    if (
      !.dest_send_get_plan_detail_delete_add_all_if_change_check(plan_list) # nolint
    ) {
      return(plan_list)
    }
    list("add" = .file_ls(path_dir_local), rm = character())
  }

.dest_send_get_plan_detail_delete_add_all_if_change_check <-
  function(plan_list) {
    !(length(plan_list[["add"]]) == 0L && length(plan_list[["rm"]]) == 0L)
  }
