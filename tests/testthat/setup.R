# remove any nodes and repos that were created

# directories to record them in


# function to delete them
.projr_test_osf_rm <- function() {
  fn_vec <- list.files(.projr_test_osf_remote_dir_get_tmp())
  for (i in seq_along(fn_vec)) {
    try(
      .projr_remote_host_rm(remote_type = "osf", fn_vec[i]),
      silent = TRUE
    )
  }
  unlink(.projr_test_osf_remote_dir_get_tmp(), recursive = TRUE)
}
.projr_test_github_rm <- function() {
  fn_vec <- list.files(.projr_test_git_remote_dir_get_tmp())
  fn_vec <- setdiff(fn_vec, "projr")
  for (i in seq_along(fn_vec)) {
    try(
      .projr_remote_host_rm(remote_type = "github", fn_vec[i]),
      silent = TRUE
    )
  }
  unlink(.projr_test_git_remote_dir_get_tmp(), recursive = TRUE)
}

.test_set()

# instruct deletion upon completion of all tests
withr::defer(
  {
    .projr_test_osf_rm()
    .projr_test_github_rm()
    .test_unset()
    .test_unset_fast()
    .test_unset_select()
    .file_rm(file.path(dirname(tempdir()), "abc"))
  },
  envir = teardown_env()
)
