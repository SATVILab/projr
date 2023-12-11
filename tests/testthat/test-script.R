test_that(".projr_yml_script_ functions work works", {
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
        .projr_yml_script_get("default"),
        list(
          "test-script" = list(
            "stage" = "pre",
            "path" = "tests/testthat/test-script.R"
          )
        )
      )

      # test projr_yml_script_rm_path
      browser()
      debugonce(projr_yml_script_rm)
      projr_yml_script_rm(title = "test-script", profile = "default")
      expect_identical(
        .projr_yml_script_get("default"),
        list()
      )
      browser()

      # test projr_yml_script_rm_title
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "major"
      )
      .projr_yml_script_rm_title(
        title = "test-script", profile = "default"
      )
      expect_identical(
        .projr_yml_script_get("default"),
        NULL
      )

      # test projr_yml_script_rm_all
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "minor"
      )
      projr_yml_script_rm_all(profile = "default")
      expect_identical(
        .projr_yml_script_get("default"),
        NULL
      )

      # test projr_yml_script_rm
      projr_yml_script_add(
        path = c(
          "tests/testthat/test-script.R", "tests/testthat/test-script-2.R"
        ),
        title = "test-script",
        stage = "post",
        cue = "minor"
      )
      expect_identical(
        .projr_yml_script_get("default"),
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
        path = "tests/testthat/test-script.R",
        profile = "default"
      )
      expect_identical(
        .projr_yml_script_get("default"),
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

test_that(".projr_yml_script_ functions work works with other profile", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

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
      # test projr_yml_script_add
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        profile = "test"
      )
      expect_identical(
        .projr_yml_script_get("test"),
        list(
          "test-script" = list(
            "stage" = "pre",
            "path" = "tests/testthat/test-script.R"
          )
        )
      )

      # test projr_yml_script_rm_path
      projr_yml_script_rm(title = "test-script", profile = "test")
      expect_identical(
        .projr_yml_script_get("test"),
        NULL
      )

      # test projr_yml_script_rm_title
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "major",
        profile = "test"
      )
      .projr_yml_script_rm_title(
        title = "test-script", profile = "test"
      )
      expect_identical(
        .projr_yml_script_get("test"),
        NULL
      )

      # test projr_yml_script_rm_all
      projr_yml_script_add(
        path = "tests/testthat/test-script.R",
        title = "test-script",
        stage = "pre",
        cue = "minor",
        profile = "test"
      )

      projr_yml_script_rm_all(profile = "test")
      expect_identical(
        .projr_yml_script_get("test"),
        NULL
      )

      # test projr_yml_script_rm
      projr_yml_script_add(
        path = c(
          "tests/testthat/test-script.R", "tests/testthat/test-script-2.R"
        ),
        title = "test-script",
        stage = "post",
        cue = "minor",
        profile = "test"
      )
      expect_identical(
        .projr_yml_script_get("test"),
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
        path = "tests/testthat/test-script.R",
        profile = "test"
      )
      expect_identical(
        .projr_yml_script_get("test"),
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

test_that(".projr_build_script... functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_script_add(
        "title" = "test-script",
        path = c("script1.R", "script2.R"),
        stage = "pre"
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
      .projr_build_script_run(
        bump_component = "patch",
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
      .projr_build_script_run(
        bump_component = "patch",
        stage = "pre"
      )
      expect_true(exists("x"))
      expect_true(exists("y"))
    }
  )
})
