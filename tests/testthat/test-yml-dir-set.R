test_that("projr_yml_dir_path_set functions work", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # Test setting path for existing label
      projr_yml_dir_path_set("output", "_my_output")
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["path"]], "_my_output")

      # Test setting path for cache directory
      projr_yml_dir_path_set("cache", "_my_cache")
      yml_dir <- .yml_dir_get_label("cache", "default")
      expect_identical(yml_dir[["path"]], "_my_cache")

      # Test creating new label with valid prefix
      projr_yml_dir_path_set("output-results", "_results")
      yml_dir <- .yml_dir_get_label("output-results", "default")
      expect_identical(yml_dir[["path"]], "_results")

      # Test setting path for raw-data
      projr_yml_dir_path_set("raw-data", "_my_raw")
      yml_dir <- .yml_dir_get_label("raw-data", "default")
      expect_identical(yml_dir[["path"]], "_my_raw")
    }
  )
})

test_that("projr_yml_dir_path_rm works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set custom path
      projr_yml_dir_path_set("output", "_custom_output")
      yml_dir_before <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir_before[["path"]], "_custom_output")

      # Revert to default
      projr_yml_dir_path_rm("output")
      yml_dir_after <- .yml_dir_get_label("output", "default")

      # Should revert to default path
      expect_identical(yml_dir_after[["path"]], "_output")
    }
  )
})

test_that("projr_yml_dir_path_set validation works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test invalid label (doesn't start with valid prefix)
      expect_error(
        projr_yml_dir_path_set("invalid-label", "_test"),
        "not valid"
      )

      # Test invalid type for path
      expect_error(
        projr_yml_dir_path_set("output", c("path1", "path2")),
        "must be a non-empty string"
      )

      # Test missing path argument
      expect_error(
        projr_yml_dir_path_set("output"),
        "path must be given"
      )
    }
  )
})

test_that("projr_yml_dir_path_set works with different profiles", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a profile
      projr_profile_create("test-profile")

      # Set directory path in test profile
      projr_yml_dir_path_set("output", "_profile_output", profile = "test-profile")

      # Check it's set in the test profile
      yml_dir <- .yml_dir_get_label("output", "test-profile")
      expect_identical(yml_dir[["path"]], "_profile_output")

      # Check default profile is unchanged (should have default path)
      yml_dir_default <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir_default[["path"]], "_output")
    }
  )
})
