test_that(".projr_local_dir_create works", {
  # skips

  # setup
  dir_test <- .projr_test_setup_project(
    git = TRUE, github = TRUE, set_env_var = FALSE
  )

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.projr_state_chr(.projr_metadata_get_author_host()))
      expect_true(.projr_state_chr(.projr_metadata_get_author_host_env()))
      expect_true(.projr_state_chr(.projr_metadata_get_author_sys_info()))
      expect_true(.projr_state_chr(.projr_metadata_get_host()))
      expect_true(.projr_state_chr(.projr_metadata_get_time()))
    }
  )
})
