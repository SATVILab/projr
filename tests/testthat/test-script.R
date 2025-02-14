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
        stage = "pre"
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
        cue = "major"
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
        cue = "minor"
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
        cue = "minor"
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
      Sys.setenv(.PROFILE" = "test")
      withr::defer(
        Sys.unsetenv(.PROFILE"),
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
