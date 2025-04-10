test_that(".changelog_add works", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .changelog_add(
        msg = "Test",
        bump_component = "patch",
        version_run_on_list = list(
          desc = list(
            success = "0.0.1"
          )
        )
      )
      .changelog_add(
        msg = "Test",
        bump_component = "minor",
        version_run_on_list = list(
          desc = list(
            success = "0.1.0"
          )
        )
      )
      .changelog_add(
        msg = "Test",
        bump_component = "major",
        version_run_on_list = list(
          desc = list(
            success = "1.0.0"
          )
        )
      )
      expect_identical(
        .changelog_read() |> sub("\\):.*$", "\\)", x = _),
        c(
          "# CHANGELOG", "", "- **Major** (v1.0.0): Miguel Julio Rodo (14:39:59)", # nolint
          "  - Test", "", "___", "", "- *Minor* (v0.1.0): Miguel Julio Rodo (14:39:59)", # nolint
          "  - Test", "", "- Patch (v0.0.1): Miguel Julio Rodo (14:39:59)",
          "  - Test", ""
        ) |>
          sub("\\):.*$", "\\)", x = _)
      )
      .cat_changelog()
    }
  )
})
