test_that(".yml_script_ functions work works", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # test.yml_script_add
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = NULL,
        profile = "default"
      )
      expect_identical(
        .yml_script_get("default"),
        list(
          "test-script" = list(
            "stage" = "pre",
            "path" = "tests/testthat/test-script.R"
          )
        )
      )

      # test.yml_script_rm_path
      .yml_script_rm(title = "test-script", profile = "default")
      expect_identical(
        .yml_script_get("default"),
        NULL
      )

      # test.yml_script_rm_title
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "major",
        profile = "default"
      )
      .yml_script_rm_title(
        title = "test-script", profile = "default"
      )
      expect_identical(
        .yml_script_get("default"),
        NULL
      )

      # test.yml_script_rm_all
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "minor",
        profile = "default"
      )
      .yml_script_rm_all(profile = "default")
      expect_identical(
        .yml_script_get("default"),
        NULL
      )

      # test.yml_script_rm
      .yml_script_add(
        path = c(
          "tests/testthat/test-script.R", "tests/testthat/test-script-2.R"
        ),
        title = "test-script",
        stage = "post",
        cue = "minor",
        profile = "default"
      )
      expect_identical(
        .yml_script_get("default"),
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

      .yml_script_rm(
        title = "test-script",
        path = "tests/testthat/test-script.R",
        profile = "default"
      )
      expect_identical(
        .yml_script_get("default"),
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

test_that(".yml_script_ functions work works with other profile", {
  skip_if(.is_test_select())
  # setup
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      Sys.setenv("PROJR_PROFILE" = "test")
      withr::defer(
        Sys.unsetenv("PROJR_PROFILE"),
        envir = rlang::caller_env()
      )
      file.create("_projr-test.yml")
      yaml::write_yaml(
        list(directories = list(path = "abc")), "_projr-test.yml"
      )
      # test.yml_script_add
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        profile = "test"
      )
      expect_identical(
        .yml_script_get("test"),
        list(
          "test-script" = list(
            "stage" = "pre",
            "path" = "tests/testthat/test-script.R"
          )
        )
      )

      # test.yml_script_rm_path
      .yml_script_rm(title = "test-script", profile = "test")
      expect_identical(
        .yml_script_get("test"),
        NULL
      )

      # test.yml_script_rm_title
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "major",
        profile = "test"
      )
      .yml_script_rm_title(
        title = "test-script", profile = "test"
      )
      expect_identical(
        .yml_script_get("test"),
        NULL
      )

      # test.yml_script_rm_all
      .yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "minor",
        profile = "test"
      )

      .yml_script_rm_all(profile = "test")
      expect_identical(
        .yml_script_get("test"),
        NULL
      )

      # test.yml_script_rm
      .yml_script_add(
        path = c(
          "tests/testthat/test-script.R", "tests/testthat/test-script-2.R"
        ),
        title = "test-script",
        stage = "post",
        cue = "minor",
        profile = "test"
      )
      expect_identical(
        .yml_script_get("test"),
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

      .yml_script_rm(
        title = "test-script",
        path = "tests/testthat/test-script.R",
        profile = "test"
      )
      expect_identical(
        .yml_script_get("test"),
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

test_that(".build_script... functions work works", {
  skip_if(.is_test_select())
  # setup
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      .yml_script_add(
        title = "test-script",
        path = c("script1.R", "script2.R"),
        stage = "pre",
        cue = NULL,
        profile = "default"
      )
      cat("x <- 1; saveRDS(x, 'x.rds')", file = "script1.R")
      cat("y <- 1; saveRDS(y, 'y.rds')", file = "script2.R")
      # nothing done
      if (exists("x", envir = .GlobalEnv)) {
        rm(x, envir = .GlobalEnv)
      }
      if (exists("y", envir = .GlobalEnv)) {
        rm(y, envir = .GlobalEnv)
      }
      .build_script_run(
        stage = "post"
      )
      expect_false(exists("x"))
      expect_false(exists("y"))
      # nothing done
      if (exists("x")) {
        rm(x)
      }
      if (exists("y")) {
        rm(y)
      }
      # something done
      .build_script_run(
        stage = "pre"
      )
      expect_true(exists("x"))
      expect_true(exists("y"))
    }
  )
})

test_that(".build_script_run validates script paths before execution", {
  skip_if(.is_test_select())
  # setup
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with non-existent script in pre stage
      .yml_script_add(
        title = "test-missing",
        path = c("missing_script.R"),
        stage = "pre",
        cue = NULL,
        profile = "default",
        overwrite = TRUE
      )
      
      # Should throw error before attempting to run the script
      expect_error(
        .build_script_run(stage = "pre"),
        "The following script\\(s\\) specified in _projr.yml do not exist"
      )
      expect_error(
        .build_script_run(stage = "pre"),
        "missing_script.R"
      )
      
      # Test with mix of existing and non-existent scripts
      cat("x <- 1", file = "existing_script.R")
      .yml_script_add(
        title = "test-mixed",
        path = c("existing_script.R", "another_missing.R"),
        stage = "post",
        cue = NULL,
        profile = "default",
        overwrite = TRUE
      )
      
      expect_error(
        .build_script_run(stage = "post"),
        "another_missing.R"
      )
      
      # Test that scripts in other stages don't cause errors
      .yml_script_rm_all(profile = "default")
      .yml_script_add(
        title = "test-other-stage",
        path = c("missing_post_script.R"),
        stage = "post",
        cue = NULL,
        profile = "default",
        overwrite = TRUE
      )
      
      # Running pre-stage should not error for missing post-stage scripts
      expect_invisible(.build_script_run(stage = "pre"))
      
      # But running post-stage should error
      expect_error(
        .build_script_run(stage = "post"),
        "missing_post_script.R"
      )
    }
  )
})

