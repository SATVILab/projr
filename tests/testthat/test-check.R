test_that(".projr_remote_create works", {
  skip_if_offline()
  dir_test <- .projr_test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      a <- "abc"
      expect_true(.assert_string(a, FALSE))
      expect_true(.assert_string(a, TRUE))
      a <- NULL
      expect_true(.assert_string(a, FALSE))
      expect_error(.assert_string(a, TRUE))
      expect_error(.assert_string(a, TRUE), regexp = "a must be given")
    }
  )
})
