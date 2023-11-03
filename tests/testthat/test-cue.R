test_that(".projr_cue_check works", {
  # skips
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.projr_cue_check(cue = NULL))
      expect_true(.projr_cue_check(cue = "dev", bump_component = FALSE))
      expect_false(.projr_cue_check(cue = "patch", bump_component = FALSE))
      expect_false(.projr_cue_check(cue = "none", bump_component = "dev"))
      expect_true(.projr_cue_check(cue = "dev", bump_component = "major"))
      expect_false(.projr_cue_check(cue = "major", bump_component = "dev"))
      expect_false(.projr_cue_check(cue = "minor", bump_component = "dev"))
      expect_false(.projr_cue_check(cue = "patch", bump_component = "dev"))
      expect_true(.projr_cue_check(cue = "patch", bump_component = "patch"))
      expect_true(.projr_cue_check(cue = "minor", bump_component = "minor"))
      expect_true(.projr_cue_check(cue = "major", bump_component = "major"))
    }
  )
})
