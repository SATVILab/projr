test_that("projr_test_renv_restore works", {
  # setup
  # skip_if(.is_test_select())
  skip_on_cran()
  skip_if_offline()
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_init_renv(force = FALSE, bioc = FALSE, skip_init = FALSE)
      pak_setting <- Sys.getenv("RENV_CONFIG_PAK_ENABLED")
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
      try(renv::snapshot(prompt = FALSE))
      try(renv::update(prompt = FALSE))
      expect_true(projr_test_renv())
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = pak_setting)
    }
  )
})
