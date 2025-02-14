test_that(".local_dir_create works", {
  # skips
  skip_if(.is_test_select())

  # setup
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = FALSE
  )

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.is_chr(.metadata_get_author_host()))
      expect_true(.is_chr(.metadata_get_author_host_env()))
      expect_true(.is_chr(.metadata_get_author_sys_info()))
      expect_true(.is_chr(.metadata_get_host()))
      expect_true(.is_chr(.metadata_get_time()))
      expect_true(.is_chr(.metadata_get_date()))
    }
  )
})
