# Additional Coverage Tests for R/restore.R
#
# This file adds tests specifically to increase coverage of R/restore.R
# focusing on code paths not covered by existing tests.

# =============================================================================
# Destination Source Selection Tests
# =============================================================================

test_that("projr_content_update selects destination with source=TRUE in YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup content
      .test_content_setup_label("raw-data")

      # Setup remote
      remote_dest <- .dir_create_tmp_random()
      on.exit(unlink(remote_dest, recursive = TRUE), add = TRUE)

      projr_yml_dest_add_local(
        title = "dest-title",
        content = "raw-data",
        path = remote_dest,
        structure = "latest"
      )

      # Build to populate remote
      projr_build_patch(msg = "test")

      # Set source=TRUE for this destination
      yml <- .yml_get(NULL)
      yml$build$local$`dest-title`$source <- TRUE
      .yml_set(yml, NULL)

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from dest (should use dest with source=TRUE)
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest"
      )

      expect_true(result)
    }
  )
})

test_that("projr_content_update skips destination with source=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup content
      .test_content_setup_label("raw-data")

      # Setup remote
      remote_dest <- .dir_create_tmp_random()
      on.exit(unlink(remote_dest, recursive = TRUE), add = TRUE)

      projr_yml_dest_add_local(
        title = "dest-title",
        content = "raw-data",
        path = remote_dest,
        structure = "latest"
      )

      # Build to populate remote
      projr_build_patch(msg = "test")

      # Set source=FALSE for this destination
      yml <- .yml_get(NULL)
      yml$build$local$`dest-title`$source <- FALSE
      .yml_set(yml, NULL)

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Try to restore from dest (should fail - source=FALSE)
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest",
        type = "local",
        title = "dest-title"
      )

      expect_false(result)
    }
  )
})

test_that("projr_content_update selects destination with label in source vector", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup content
      .test_content_setup_label("raw-data")

      # Setup remote
      remote_dest <- .dir_create_tmp_random()
      on.exit(unlink(remote_dest, recursive = TRUE), add = TRUE)

      projr_yml_dest_add_local(
        title = "dest-title",
        content = "raw-data",
        path = remote_dest,
        structure = "latest"
      )

      # Build to populate remote
      projr_build_patch(msg = "test")

      # Set source as vector of labels
      yml <- .yml_get(NULL)
      yml$build$local$`dest-title`$source <- c("raw-data")
      .yml_set(yml, NULL)

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from dest
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest"
      )

      expect_true(result)
    }
  )
})

test_that("projr_content_update handles no type found in dest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Try to restore from dest with no configured destinations
      result <- suppressWarnings(
        projr_content_update(
          label = "raw-data",
          pos = "dest"
        )
      )

      # Should return FALSE (no dest found)
      expect_false(result)
    }
  )
})

# =============================================================================
# Additional Edge Case Tests
# =============================================================================

test_that("projr_content_update handles label not in destination content list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup content for raw-data
      .test_content_setup_label("raw-data")
      .test_content_setup_label("cache")

      # Setup remote but only for cache
      remote_dest <- .dir_create_tmp_random()
      on.exit(unlink(remote_dest, recursive = TRUE), add = TRUE)

      projr_yml_dest_add_local(
        title = "dest-title",
        content = "cache",  # Only cache, not raw-data
        path = remote_dest,
        structure = "latest"
      )

      # Build
      projr_build_patch(msg = "test")

      # Clear raw-data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Try to restore raw-data from dest (not in content list)
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest",
        type = "local",
        title = "dest-title"
      )

      # Should return FALSE (label not in destination's content list)
      expect_false(result)
    }
  )
})

test_that("projr_content_update handles first available type when none specified in dest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup content
      .test_content_setup_label("raw-data")

      # Setup local remote
      remote_dest <- .dir_create_tmp_random()
      on.exit(unlink(remote_dest, recursive = TRUE), add = TRUE)

      projr_yml_dest_add_local(
        title = "dest-title",
        content = "raw-data",
        path = remote_dest,
        structure = "latest"
      )

      # Build
      projr_build_patch(msg = "test")

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore without specifying type (should use first available: local)
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest"
      )

      # Should succeed - finds local as first type
      expect_true(result)
    }
  )
})
