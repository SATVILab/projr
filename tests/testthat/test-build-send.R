test_that("projr_build_output works", {
  # skip_if(.is_test_select())
  skip_if(.is_test_fast())
  dir_test <- .projr_test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      .projr_test_yml_unset_remote()
      .projr_yml_git_set_commit(TRUE, TRUE, NULL)
      .projr_yml_git_set_add_untracked(TRUE, TRUE, NULL)
      .projr_yml_git_set_push(FALSE, TRUE, NULL)
      # debugonce(.projr_build_output_get_msg)
      # debugonce(.projr_git_msg_get)
      projr_build_output("patch", msg = "test")
      projr_version_get()
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "_book")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      # run repeat build
      projr_build_output("minor", msg = "test")
      # browser()
      # no add that we're pushing to GitHub, but
      # data-raw and source are empty
      projr_yml_dest_add_github(
        title = "@version",
        content = "code"
      )
      # handle nothing to upload
      # ---------------------
      projr_build_patch(msg = "Vat are you vinking about")

      # handle something to upload
      # ---------------------
      projr_yml_dest_add_github(
        title = "Raw data",
        content = "data-raw"
      )
      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Vat are you vinking about")

      # re upload, no change
      # ----------------------
      .projr_test_setup_content("data-raw")
      projr_build_patch(msg = "Vat are you vinking about")


      browser()
    },
    quiet = TRUE,
    force = TRUE
  )
})
