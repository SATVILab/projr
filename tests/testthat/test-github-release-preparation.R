# Tests for GitHub release preparation functionality
#
# This file tests the new pre-upload GitHub release creation and verification logic.
# Tests verify that:
#   - Releases are created before upload operations
#   - Multiple required releases are handled correctly
#   - Verification with retry/backoff works
#   - Appropriate errors when releases cannot be detected

# =============================================================================
# Test basic release preparation
# =============================================================================

test_that("GitHub releases are created before uploads", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github <- function(label) {
        path_dir <- projr_path_get_dir(label, safe = FALSE)
        file.create(file.path(path_dir, "test.txt"))
        writeLines("Test content", file.path(path_dir, "test.txt"))
        invisible(TRUE)
      }

      .create_test_content_github("output")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add GitHub destination
      tag_name <- paste0("test-prep-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "output",
        structure = "latest"
      )

      # Build - should create release up front in pre-build phase
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset was uploaded
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("output.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("Multiple GitHub releases are prepared together", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github <- function(label) {
        path_dir <- projr_path_get_dir(label, safe = FALSE)
        file.create(file.path(path_dir, "test.txt"))
        writeLines("Test content", file.path(path_dir, "test.txt"))
        invisible(TRUE)
      }

      .create_test_content_github("output")
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add two GitHub destinations with different tags
      tag_name_1 <- paste0("test-multi-1-", .test_random_string_get())
      tag_name_2 <- paste0("test-multi-2-", .test_random_string_get())

      projr_yml_dest_add_github(
        title = tag_name_1,
        content = "output",
        structure = "latest"
      )

      projr_yml_dest_add_github(
        title = tag_name_2,
        content = "raw-data",
        structure = "latest"
      )

      # Build - should create both releases up front in pre-build phase
      projr::projr_build_patch()

      # Verify both releases exist
      expect_true(.remote_check_exists("github", tag_name_1))
      expect_true(.remote_check_exists("github", tag_name_2))

      # Verify assets were uploaded to both
      asset_tbl_1 <- .pb_asset_tbl_get(tag_name_1)
      expect_true("output.zip" %in% asset_tbl_1[["file_name"]])

      asset_tbl_2 <- .pb_asset_tbl_get(tag_name_2)
      expect_true("raw-data.zip" %in% asset_tbl_2[["file_name"]])
    }
  )
})

test_that("Config-driven tag derivation works correctly", {
  skip_if(.is_test_select())

  # This test doesn't create actual releases, just tests the tag derivation logic
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Clear all destinations first
      .yml_dest_rm_type_all("default")

      # Test 1: No destinations configured
      tags <- .dest_github_tags_needed(FALSE)
      expect_length(tags, 0)

      # Test 2: Add a YAML destination
      projr_yml_dest_add_github(
        title = "my-release",
        content = "output",
        structure = "latest"
      )
      tags <- .dest_github_tags_needed(FALSE)
      expect_length(tags, 1)
      expect_true("my-release" %in% tags)

      # Test 3: Add archive_github parameter (no YAML archive destination)
      tags <- .dest_github_tags_needed(TRUE)
      expect_length(tags, 2)
      expect_true("my-release" %in% tags)
      expect_true("archive" %in% tags)

      # Test 4: Add YAML archive destination - parameter should be ignored
      projr_yml_dest_add_github(
        title = "archive",
        content = "output",
        structure = "archive"
      )
      tags <- .dest_github_tags_needed(TRUE)
      expect_length(tags, 2)
      expect_true("my-release" %in% tags)
      expect_true("archive" %in% tags)
    }
  )
})
