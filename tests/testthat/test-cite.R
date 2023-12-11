test_that(".projr_yml_script_ functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      browser()
      projr_yml_cite_unset()
    }
  )
})
