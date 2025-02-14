# remove any nodes and repos that were created

# directories to record them in


# function to delete them
.test_osf_rm <- function() {
  fn_vec <- list.files(.test_osf_remote_dir_get_tmp())
  for (i in seq_along(fn_vec)) {
    try(
      .remote_host_rm(remote_type = "osf", fn_vec[i]),
      silent = TRUE
    )
  }
  unlink(.test_osf_remote_dir_get_tmp(), recursive = TRUE)
}
.test_github_rm <- function() {
  fn_vec <- list.files(.test_git_remote_dir_get_tmp())
  fn_vec <- setdiff(fn_vec, "projr")
  for (i in seq_along(fn_vec)) {
    try(
      .remote_host_rm(remote_type = "github", fn_vec[i]),
      silent = TRUE
    )
  }
  unlink(.test_git_remote_dir_get_tmp(), recursive = TRUE)
}

.test_set()

# instruct deletion upon completion of all tests
withr::defer(
  {
    .test_osf_rm()
    .test_github_rm()
    .test_unset()
    # .test_unset_fast()
    # .test_unset_select()
    try(.test_rm_random_abc(), silent = TRUE)
  },
  envir = teardown_env()
)

.test_rm_random_abc <- function() {
  path_random <- file.path(dirname(tempdir()), "abc")
  if (file.exists(path_random)) {
    if (fs::is_dir(path_random)) {
      .dir_rm(path_random)
    }
    if (fs::is_file(path_random)) {
      file.remove(path_random)
    }
  }
  invisible(TRUE)
}
