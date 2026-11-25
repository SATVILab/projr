# Remote selection when fetching directories with projr_source()

# =============================================================================
# Remote discovery
# =============================================================================

test_that("projr_source needs a registered remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = TRUE,
    github = FALSE,
    set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal manifest
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")

      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # No remote is registered to fetch from
      expect_error(projr_source("raw-data"), "No remotes are registered")
    }
  )
})

# =============================================================================
# Additional Edge Case Tests
# =============================================================================

test_that("projr_source reports a label the remote did not publish", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = TRUE,
    github = FALSE,
    set_env_var = TRUE
  )

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
        content = "cache", # Only cache, not raw-data
        path = remote_dest,
        structure = "latest"
      )

      # Build
      projr_build_patch(msg = "test")

      # Clear raw-data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Try to restore raw-data from dest (not in content list)
      expect_error(projr_source("raw-data", "local:dest-title"))
    }
  )
})

test_that("projr_source discovers the remote that published a label", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project(
    git = TRUE,
    github = FALSE,
    set_env_var = TRUE
  )

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

      # Without a remote, use the single registered remote that published it
      expect_true(projr_source("raw-data"))
      expect_true(
        length(list.files(projr_path_get_dir("raw-data", safe = FALSE))) > 0
      )
    }
  )
})
