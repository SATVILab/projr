test_that(".projr_cue_check works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .projr_changelog_add(
        msg = "Test",
        bump_component = "patch",
        version_run_on_list = list(
          desc = list(
            success = "0.0.1"
          )
        )
      )
      .projr_changelog_add(
        msg = "Test",
        bump_component = "minor",
        version_run_on_list = list(
          desc = list(
            success = "0.1.0"
          )
        )
      )
      .projr_changelog_add(
        msg = "Test",
        bump_component = "major",
        version_run_on_list = list(
          desc = list(
            success = "1.0.0"
          )
        )
      )
      expect_identical(
        .projr_changelog_get(),
        c(
          "# CHANGELOG", "", "- **major** (v1.0.0): Test", "",
          "- *minor* (v0.1.0): Test",
          "- patch (v0.0.1): Test", ""
        )
      )
    }
  )
})
