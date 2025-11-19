test_that("projr_dir_set functions work", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # Test setting path for existing label
      projr_dir_set("output", path = "_my_output")
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["path"]], "_my_output")

      # Test setting ignore settings
      projr_dir_set("output", ignore_git = FALSE, ignore_rbuild = TRUE)
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["ignore-git"]], FALSE)
      expect_identical(yml_dir[["ignore-rbuild"]], TRUE)

      # Test setting git_skip_adjust
      projr_dir_set("cache", git_skip_adjust = FALSE)
      yml_dir <- .yml_dir_get_label("cache", "default")
      expect_identical(yml_dir[["git-skip-adjust"]], FALSE)

      # Test creating new label with valid prefix
      projr_dir_set("output-results", path = "_results", ignore_git = TRUE)
      yml_dir <- .yml_dir_get_label("output-results", "default")
      expect_identical(yml_dir[["path"]], "_results")
      expect_identical(yml_dir[["ignore-git"]], TRUE)

      # Test setting with character ignore values
      projr_dir_set("output", ignore = "manual")
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["ignore"]], "manual")

      # Test simplify_default = TRUE removes entire config if at defaults
      projr_dir_set("cache", path = "_tmp", simplify_default = TRUE)
      yml_dir <- .yml_dir_get_label("cache", "default")
      # cache with default path only should be simplified away
      expect_identical(yml_dir[["path"]], "_tmp")

      # Test simplify_default = FALSE keeps all values
      projr_dir_set("output", path = "_output_custom", simplify_default = FALSE)
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["path"]], "_output_custom")
    }
  )
})

test_that("projr_dir_set_default works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set custom configuration
      projr_dir_set("output", path = "_custom_output", ignore_git = FALSE)
      yml_dir_before <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir_before[["path"]], "_custom_output")
      expect_identical(yml_dir_before[["ignore-git"]], FALSE)

      # Revert to defaults
      projr_dir_set_default("output")
      yml_dir_after <- .yml_dir_get_label("output", "default")

      # Should revert to default path
      expect_identical(yml_dir_after[["path"]], "_output")
      # After set_default, ignore-git should not be present
      expect_null(yml_dir_after[["ignore-git"]])
    }
  )
})

test_that("projr_dir_set validation works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Test error when creating new label without path
      expect_error(
        projr_dir_set("output-new", ignore_git = TRUE),
        "requires a 'path' argument"
      )

      # Test invalid label (doesn't start with valid prefix)
      expect_error(
        projr_dir_set("invalid-label", path = "_test"),
        "not valid"
      )

      # Test invalid ignore value
      expect_error(
        projr_dir_set("output", ignore = "invalid"),
        "must be one of"
      )

      # Test invalid type for path
      expect_error(
        projr_dir_set("output", path = c("path1", "path2")),
        "must be a non-empty string"
      )

      # Test invalid type for git_skip_adjust
      expect_error(
        projr_dir_set("output", git_skip_adjust = "yes"),
        "must be a non-NA flag"
      )
    }
  )
})

test_that("projr_dir_set works with different profiles", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a profile
      projr_profile_create("test-profile")

      # Set directory in test profile
      projr_dir_set("output", path = "_profile_output", profile = "test-profile")

      # Check it's set in the test profile
      yml_dir <- .yml_dir_get_label("output", "test-profile")
      expect_identical(yml_dir[["path"]], "_profile_output")

      # Check default profile is unchanged (should have default path)
      yml_dir_default <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir_default[["path"]], "_output")
    }
  )
})

test_that("projr_dir_set handles ignore conflicts correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Set ignore and ignore_git to different values
      projr_dir_set("output", ignore = FALSE, ignore_git = FALSE)
      yml_dir <- .yml_dir_get_label("output", "default")
      expect_identical(yml_dir[["ignore"]], FALSE)
      expect_identical(yml_dir[["ignore-git"]], FALSE)

      # Both can coexist; validation happens at check time
    }
  )
})
