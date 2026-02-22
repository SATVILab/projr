test_that(".yml_script_ functions work works", {
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  # setup
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      if (exists("x", inherits = FALSE)) rm(x)
      if (exists("y", inherits = FALSE)) rm(y)
      .yml_script_add(
        "title" = "test-script",
        path = c("script1.R", "script2.R"),
        stage = "pre"
      )
      cat("x <- 1; saveRDS(x, 'x.rds')", file = "script1.R")
      cat("y <- 1; saveRDS(y, 'y.rds')", file = "script2.R")
      # Clean up any existing RDS files
      if (file.exists("x.rds")) unlink("x.rds")
      if (file.exists("y.rds")) unlink("y.rds")
      # Nothing done for "post" stage
      .build_script_run(
        stage = "post"
      )
      expect_false(file.exists("x.rds"))
      expect_false(file.exists("y.rds"))
      # Scripts run for "pre" stage
      .build_script_run(
        stage = "pre"
      )
      # Scripts should run in isolated environments, so x and y are NOT in global env
      expect_false(exists("x", inherits = FALSE))
      expect_false(exists("y", inherits = FALSE))
      # But the RDS files should be created
      expect_true(file.exists("x.rds"))
      expect_true(file.exists("y.rds"))
      # Clean up
      unlink("x.rds")
      unlink("y.rds")
    }
  )
})

# Tests for exported functions
test_that("projr_yml_script_add works with all parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test basic add with pre stage
      projr_yml_script_add(
        path = "script1.R",
        title = "test-script",
        stage = "pre"
      )
      expect_identical(
        .yml_script_get("default")[["test-script"]][["stage"]],
        "pre"
      )
      expect_identical(
        .yml_script_get("default")[["test-script"]][["path"]],
        "script1.R"
      )

      # Test add with post stage and cue
      projr_yml_script_add(
        path = "script2.R",
        title = "post-script",
        stage = "post",
        cue = "minor"
      )
      expect_identical(
        .yml_script_get("default")[["post-script"]][["stage"]],
        "post"
      )
      expect_identical(
        .yml_script_get("default")[["post-script"]][["cue"]],
        "minor"
      )

      # Test add with multiple paths
      projr_yml_script_add(
        path = c("script3.R", "script4.R"),
        title = "multi-script",
        stage = "pre"
      )
      expect_identical(
        .yml_script_get("default")[["multi-script"]][["path"]],
        c("script3.R", "script4.R")
      )
    }
  )
})

test_that("projr_yml_script_add handles title with spaces", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test title with leading/trailing spaces
      projr_yml_script_add(
        path = "script1.R",
        title = "  test script  ",
        stage = "pre"
      )
      expect_true("test-script" %in% names(.yml_script_get("default")))

      # Test title with multiple spaces
      projr_yml_script_add(
        path = "script2.R",
        title = "my   test   script",
        stage = "post"
      )
      expect_true("my-test-script" %in% names(.yml_script_get("default")))
    }
  )
})

test_that("projr_yml_script_add overwrite parameter works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add initial script
      projr_yml_script_add(
        path = "script1.R",
        title = "test-script",
        stage = "pre"
      )

      # Test overwrite = TRUE (default) allows replacement
      projr_yml_script_add(
        path = "script2.R",
        title = "test-script",
        stage = "post",
        overwrite = TRUE
      )
      expect_identical(
        .yml_script_get("default")[["test-script"]][["path"]],
        "script2.R"
      )
      expect_identical(
        .yml_script_get("default")[["test-script"]][["stage"]],
        "post"
      )

      # Test overwrite = FALSE throws error
      expect_error(
        projr_yml_script_add(
          path = "script3.R",
          title = "test-script",
          stage = "pre",
          overwrite = FALSE
        ),
        "already exists"
      )
    }
  )
})

test_that("projr_yml_script_add works with different cue values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test each valid cue value
      cue_values <- c("build", "dev", "patch", "minor", "major")
      for (i in seq_along(cue_values)) {
        title <- paste0("script-", i)
        projr_yml_script_add(
          path = paste0("script", i, ".R"),
          title = title,
          stage = "pre",
          cue = cue_values[i]
        )
        expect_identical(
          .yml_script_get("default")[[title]][["cue"]],
          cue_values[i]
        )
      }

      # Test NULL cue (should not be in output)
      projr_yml_script_add(
        path = "script-no-cue.R",
        title = "no-cue-script",
        stage = "pre",
        cue = NULL
      )
      expect_null(
        .yml_script_get("default")[["no-cue-script"]][["cue"]]
      )
    }
  )
})

test_that("projr_yml_script_add_pre and projr_yml_script_add_post work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test projr_yml_script_add_pre
      projr_yml_script_add_pre(
        path = "pre-script.R",
        title = "pre-test"
      )
      expect_identical(
        .yml_script_get("default")[["pre-test"]][["stage"]],
        "pre"
      )
      expect_identical(
        .yml_script_get("default")[["pre-test"]][["path"]],
        "pre-script.R"
      )

      # Test projr_yml_script_add_post
      projr_yml_script_add_post(
        path = "post-script.R",
        title = "post-test",
        cue = "major"
      )
      expect_identical(
        .yml_script_get("default")[["post-test"]][["stage"]],
        "post"
      )
      expect_identical(
        .yml_script_get("default")[["post-test"]][["cue"]],
        "major"
      )
    }
  )
})

test_that("projr_yml_script_rm works with title only", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add scripts
      projr_yml_script_add(
        path = c("script1.R", "script2.R"),
        title = "test-script",
        stage = "pre"
      )

      # Remove entire script by title
      projr_yml_script_rm(title = "test-script")
      expect_null(.yml_script_get("default")[["test-script"]])
    }
  )
})

test_that("projr_yml_script_rm works with title and path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add script with multiple paths
      projr_yml_script_add(
        path = c("script1.R", "script2.R", "script3.R"),
        title = "test-script",
        stage = "pre"
      )

      # Remove one path
      projr_yml_script_rm(title = "test-script", path = "script2.R")
      expect_identical(
        .yml_script_get("default")[["test-script"]][["path"]],
        c("script1.R", "script3.R")
      )

      # Remove another path
      projr_yml_script_rm(title = "test-script", path = "script1.R")
      expect_identical(
        .yml_script_get("default")[["test-script"]][["path"]],
        "script3.R"
      )

      # Remove last path should remove entire script
      projr_yml_script_rm(title = "test-script", path = "script3.R")
      expect_null(.yml_script_get("default")[["test-script"]])
    }
  )
})

test_that("projr_yml_script_rm_all works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Add multiple scripts
      projr_yml_script_add(
        path = "script1.R",
        title = "script-1",
        stage = "pre"
      )
      projr_yml_script_add(
        path = "script2.R",
        title = "script-2",
        stage = "post"
      )
      projr_yml_script_add(
        path = "script3.R",
        title = "script-3",
        stage = "pre"
      )

      # Verify scripts exist
      expect_length(.yml_script_get("default"), 3)

      # Remove all scripts
      projr_yml_script_rm_all()
      expect_null(.yml_script_get("default"))
    }
  )
})

test_that("projr_yml_script functions work with custom profile", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create custom profile file
      file.create("_projr-custom.yml")
      yaml::write_yaml(
        list(directories = list(path = "test")), "_projr-custom.yml"
      )

      # Add script to custom profile
      projr_yml_script_add(
        path = "script1.R",
        title = "custom-script",
        stage = "pre",
        profile = "custom"
      )
      expect_identical(
        .yml_script_get("custom")[["custom-script"]][["path"]],
        "script1.R"
      )

      # Remove from custom profile
      projr_yml_script_rm(title = "custom-script", profile = "custom")
      expect_null(.yml_script_get("custom")[["custom-script"]])

      # Add multiple and remove all from custom profile
      projr_yml_script_add(
        path = "script2.R",
        title = "script-a",
        stage = "pre",
        profile = "custom"
      )
      projr_yml_script_add(
        path = "script3.R",
        title = "script-b",
        stage = "post",
        profile = "custom"
      )
      projr_yml_script_rm_all(profile = "custom")
      expect_null(.yml_script_get("custom"))
    }
  )
})

test_that("projr_yml_script input validation works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Invalid stage
      expect_error(
        projr_yml_script_add(
          path = "script.R",
          title = "test",
          stage = "invalid"
        )
      )

      # Invalid cue
      expect_error(
        projr_yml_script_add(
          path = "script.R",
          title = "test",
          stage = "pre",
          cue = "invalid"
        )
      )

      # Missing required parameters
      expect_error(
        projr_yml_script_add(
          path = "script.R",
          title = "test",
          stage = NULL
        )
      )

      expect_error(
        projr_yml_script_add(
          path = "script.R",
          title = NULL,
          stage = "pre"
        )
      )

      expect_error(
        projr_yml_script_add(
          path = NULL,
          title = "test",
          stage = "pre"
        )
      )
    }
  )
})
