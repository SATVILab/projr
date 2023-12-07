test_that("script-related functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # test projr_yml_script_add
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre"
      )
      expect_identical(
        .projr_yml_script_get(),
        list(
          "test-script" = list(
            "stage" = "pre",
            "path" = "tests/testthat/test-script.R"
          )
        )
      )

      # test projr_yml_script_rm_path
      projr_yml_script_rm(title = "test-script")
      expect_identical(
        .projr_yml_script_get(),
        list()
      )

      # test projr_yml_script_rm_title
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "major"
      )
      .projr_yml_script_rm_title(
        title = "test-script"
      )
      expect_identical(
        .projr_yml_script_get(),
        list()
      )

      # test projr_yml_script_rm_all
      .projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre-build",
        cue = "test-script"
      )
      projr_yml_script_rm_all()
      expect_identical(
        .projr_yml_script_get(),
        list()
      )

      # test projr_yml_script_rm
      browser()
      projr_yml_script_add(
        path = c("tests/testthat/test-script.R", "tests/testthat/test-script-2.R"),
        title = "test-script",
        stage = "post",
        cue = "minor"
      )
      expect_identical(
        .projr_yml_script_get(),
        list(
          `test-script` = list(
            stage = "post",
            path = c(
              "tests/testthat/test-script.R",
              "tests/testthat/test-script-2.R"
            ),
            cue = "minor"
          )
        )
      )

      projr_yml_script_rm(
        title = "test-script",
        path = "tests/testthat/test-script.R"
      )
      expect_identical(
        .projr_yml_script_get(),
        list(
          "test-script" = list(
            "stage" = "post",
            "path" = "tests/testthat/test-script-2.R",
            cue = "minor"
          )
        )
      )
    }
  )
})
