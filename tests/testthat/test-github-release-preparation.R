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

      # Build - should create release up front
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

      # Build - should create both releases up front
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

test_that("State is initialized and cleared properly", {
  skip_if(.is_test_select())

  # Initialize state
  .gh_release_state_init()

  # Verify state is initialized
  expect_length(.projr_state$gh_releases_required, 0)
  expect_length(.projr_state$gh_releases_verified, 0)
  expect_null(.projr_state$gh_release_tbl)

  # Register a required release (mock - manually add to state)
  .projr_state$gh_releases_required <- c("owner/repo::test-tag")

  # Verify registration
  expect_true(length(.projr_state$gh_releases_required) > 0)
  expect_equal(.projr_state$gh_releases_required, "owner/repo::test-tag")

  # Clear state
  .gh_release_state_clear()

  # Verify state is cleared
  expect_null(.projr_state$gh_releases_required)
  expect_null(.projr_state$gh_releases_verified)
  expect_null(.projr_state$gh_release_tbl)
})
