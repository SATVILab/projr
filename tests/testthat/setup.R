# if any nodes are created during tests
# and have not yet been removed, remove them
withr::defer(
  {
    path_dir_rm <- file.path(tempdir(), "osf_node_to_remove")
    if (dir.exists(path_dir_rm)) {
      fn_vec <- list.files(path_dir_rm)
      for (i in seq_along(fn_vec)) {
        try(
          .projr_osf_rm_node_id(fn_vec[i]),
          silent = TRUE
        )
      }
      unlink(path_dir_rm, recursive = TRUE)
    }
  },
  envir = teardown_env()
)
