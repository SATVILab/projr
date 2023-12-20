test_that(".is_cue works", {
  # skips
  skip_if(FALSE)

  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.is_cue(cue = NULL))
      expect_true(.is_cue(cue = "dev", bump_component = FALSE))
      expect_false(.is_cue(cue = "patch", bump_component = FALSE))
      expect_false(.is_cue(cue = "none", bump_component = "dev"))
      expect_true(.is_cue(cue = "dev", bump_component = "major"))
      expect_false(.is_cue(cue = "major", bump_component = "dev"))
      expect_false(.is_cue(cue = "minor", bump_component = "dev"))
      expect_false(.is_cue(cue = "patch", bump_component = "dev"))
      expect_true(.is_cue(cue = "patch", bump_component = "patch"))
      expect_true(.is_cue(cue = "minor", bump_component = "minor"))
      expect_true(.is_cue(cue = "major", bump_component = "major"))
    }
  )
})
