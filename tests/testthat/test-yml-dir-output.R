test_that("output key can be set on any directory label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Test output: true on a non-output label
      yml <- .yml_get("default")
      yml[["directories"]][["raw-data"]][["output"]] <- TRUE
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Test output: false on a non-output label
      yml <- .yml_get("default")
      yml[["directories"]][["cache"]][["output"]] <- FALSE
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Test output: ["output"] on a custom label
      yml <- .yml_get("default")
      yml[["directories"]][["custom-dir"]] <- list(
        path = "_custom",
        output = "output"
      )
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("output key validates character values exist in config", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Valid: output label exists in config
      yml <- .yml_get("default")
      yml[["directories"]][["raw-data"]][["output"]] <- "output"
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Invalid: non-existent output label
      yml <- .yml_get("default")
      yml[["directories"]][["raw-data"]][["output"]] <- "nonexistent-output"
      .yml_set(yml, "default")
      expect_error(projr_yml_check())

      # Valid: Multiple output labels
      yml <- .yml_get("default")
      yml[["directories"]][["output2"]] <- list(path = "_output2")
      yml[["directories"]][["raw-data"]][["output"]] <- c("output", "output2")
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("output key defaults to 'output' when no output labels exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # When only default 'output' label exists
      yml <- .yml_get("default")
      yml[["directories"]][["raw-data"]][["output"]] <- "output"
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directories with output: true are copied to all output directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup multiple output directories
      yml <- .yml_get("default")
      yml[["directories"]][["output2"]] <- list(path = "_output2")
      yml[["directories"]][["raw-data"]][["output"]] <- TRUE
      .yml_set(yml, "default")

      # Create test file in raw-data
      dir.create("_raw_data", showWarnings = FALSE, recursive = TRUE)
      writeLines("test content", "_raw_data/test.txt")

      # Run build
      projr_build_dev()

      # For dev builds, output directories are in safe (temp) location
      # Check that file was copied to both output directories
      output_path <- projr_path_get_dir("output", "raw-data", safe = TRUE)
      output2_path <- projr_path_get_dir("output2", "raw-data", safe = TRUE)
      
      expect_true(file.exists(file.path(output_path, "test.txt")))
      expect_true(file.exists(file.path(output2_path, "test.txt")))

      # Verify content is the same
      expect_identical(
        readLines(file.path(output_path, "test.txt")),
        "test content"
      )
      expect_identical(
        readLines(file.path(output2_path, "test.txt")),
        "test content"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directories with output: [specific] are copied only to specified directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup multiple output directories
      yml <- .yml_get("default")
      yml[["directories"]][["output2"]] <- list(path = "_output2")
      yml[["directories"]][["raw-data"]][["output"]] <- "output2"
      .yml_set(yml, "default")

      # Create test file in raw-data
      dir.create("_raw_data", showWarnings = FALSE, recursive = TRUE)
      writeLines("test content", "_raw_data/test.txt")

      # Run build
      projr_build_dev()

      # For dev builds, output directories are in safe (temp) location
      # Check that file was copied only to output2, not output
      output_path <- projr_path_get_dir("output", "raw-data", safe = TRUE)
      output2_path <- projr_path_get_dir("output2", "raw-data", safe = TRUE)
      
      expect_false(file.exists(file.path(output_path, "test.txt")))
      expect_true(file.exists(file.path(output2_path, "test.txt")))

      # Verify content
      expect_identical(
        readLines(file.path(output2_path, "test.txt")),
        "test content"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directories with output: false are not copied", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup with output: false
      yml <- .yml_get("default")
      yml[["directories"]][["raw-data"]][["output"]] <- FALSE
      .yml_set(yml, "default")

      # Create test file in raw-data
      dir.create("_raw_data", showWarnings = FALSE, recursive = TRUE)
      writeLines("test content", "_raw_data/test.txt")

      # Run build
      projr_build_dev()

      # Check that file was not copied
      output_path <- projr_path_get_dir("output", "raw-data", safe = TRUE)
      expect_false(file.exists(file.path(output_path, "test.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("directories without output key are not copied by default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup custom directory without output key
      yml <- .yml_get("default")
      yml[["directories"]][["custom-dir"]] <- list(path = "_custom")
      .yml_set(yml, "default")

      # Create test file in custom directory
      dir.create("_custom", showWarnings = FALSE, recursive = TRUE)
      writeLines("test content", "_custom/test.txt")

      # Run build
      projr_build_dev()

      # Check that file was not copied (no output key = no copy)
      output_path <- projr_path_get_dir("output", "custom-dir", safe = TRUE)
      expect_false(file.exists(file.path(output_path, "test.txt")))
    },
    force = TRUE,
    quiet = TRUE
  )
})
