test_that(".is_cue works", {
  # skips
  skip_if(FALSE)
  skip_if(.is_test_select())

  # setup
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # Test: cue="always" with bump_component=NULL should return FALSE (not output build)
      expect_false(.is_cue(cue = "always", bump_component = NULL))
      
      # Test: cue="always" with bump_component="dev" should return FALSE (dev is not output build)
      expect_false(.is_cue(cue = "always", bump_component = "dev"))
      
      # Test: cue="never" with bump_component="patch" should return FALSE (never act)
      expect_false(.is_cue(cue = "never", bump_component = "patch"))
      
      # Test: cue="always" with bump_component="none" should return FALSE (none is not output build)
      expect_false(.is_cue(cue = "always", bump_component = "none"))
      
      # Test: cue="always" with bump_component="major" should return TRUE (always act, output build)
      expect_true(.is_cue(cue = "always", bump_component = "major"))
      
      # Test: cue="if-change" with bump_component="major" should return TRUE (if-change + output build)
      expect_true(.is_cue(cue = "if-change", bump_component = "major"))
      
      # Test: cue="if-change" with bump_component="dev" should return FALSE (dev is not output build)
      expect_false(.is_cue(cue = "if-change", bump_component = "dev"))
      
      # Test: cue="never" with bump_component="dev" should return FALSE (never act)
      expect_false(.is_cue(cue = "never", bump_component = "dev"))
      
      # Test: cue="always" with bump_component="patch" should return TRUE (always act, output build)
      expect_true(.is_cue(cue = "always", bump_component = "patch"))
      
      # Test: cue="if-change" with bump_component="minor" should return TRUE (if-change + output build)
      expect_true(.is_cue(cue = "if-change", bump_component = "minor"))
      
      # Test: cue="always" with bump_component=FALSE should return TRUE (FALSE is not NULL/dev/none)
      expect_true(.is_cue(cue = "always", bump_component = FALSE))
    }
  )
})
