# Dedicated Restore Tests
#
# This file tests restore functionality (projr_content_update and related functions)
# All tests use local remotes and run in LITE mode for fast iteration
# Tests focus on parameter validation, edge cases, and error handling

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# Parameter Validation Tests
# =============================================================================

test_that("projr_content_update validates label parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest so validation can proceed
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")
      
      # Invalid type - not character
      expect_error(
        projr_content_update(label = 123),
        "'label' must be NULL or a character vector"
      )
      
      # Invalid type - empty vector
      expect_error(
        projr_content_update(label = character(0)),
        "'label' must have at least one element if not NULL"
      )
      
      # Valid - NULL is allowed
      expect_error(
        projr_content_update(label = NULL),
        NA  # Should not error on NULL
      )
      
      # Valid - character vector
      expect_error(
        projr_content_update(label = "raw-data"),
        NA  # Should not error
      )
    }
  )
})

test_that("projr_content_update validates pos parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")
      
      # Invalid type - not character
      expect_error(
        projr_content_update(pos = 123),
        "'pos' must be NULL or a character vector"
      )
      
      # Invalid type - empty vector
      expect_error(
        projr_content_update(pos = character(0)),
        "'pos' must have at least one element if not NULL"
      )
      
      # Invalid value
      expect_error(
        projr_content_update(pos = "invalid"),
        "'pos' must be 'source' or 'dest'"
      )
      
      # Valid - NULL
      expect_error(
        projr_content_update(pos = NULL),
        NA
      )
      
      # Valid - "source"
      expect_error(
        projr_content_update(pos = "source"),
        NA
      )
      
      # Valid - "dest"
      expect_error(
        projr_content_update(pos = "dest"),
        NA
      )
      
      # Valid - both
      expect_error(
        projr_content_update(pos = c("source", "dest")),
        NA
      )
    }
  )
})

test_that("projr_content_update validates type parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")
      
      # Invalid type - not character
      expect_error(
        projr_content_update(type = 123),
        "'type' must be NULL or a character vector"
      )
      
      # Invalid type - empty vector
      expect_error(
        projr_content_update(type = character(0)),
        "'type' must have at least one element if not NULL"
      )
      
      # Invalid type - multiple values
      expect_error(
        projr_content_update(type = c("local", "github")),
        "'type' must be a single character value"
      )
      
      # Invalid value
      expect_error(
        projr_content_update(type = "invalid"),
        "'type' must be one of: local, github"
      )
      
      # Valid - NULL
      expect_error(
        projr_content_update(type = NULL),
        NA
      )
      
      # Valid - "local"
      expect_error(
        projr_content_update(type = "local"),
        NA
      )
    }
  )
})

test_that("projr_content_update validates title parameter", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")
      
      # Invalid type - not character
      expect_error(
        projr_content_update(title = 123),
        "'title' must be NULL or a character vector"
      )
      
      # Invalid type - empty vector
      expect_error(
        projr_content_update(title = character(0)),
        "'title' must have at least one element if not NULL"
      )
      
      # Invalid type - multiple values
      expect_error(
        projr_content_update(title = c("title1", "title2")),
        "'title' must be a single character value"
      )
      
      # Valid - NULL
      expect_error(
        projr_content_update(title = NULL),
        NA
      )
      
      # Valid - single string
      expect_error(
        projr_content_update(title = "test-title"),
        NA
      )
    }
  )
})

# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("projr_content_update handles missing manifest.csv", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Remove manifest if it exists
      if (file.exists("manifest.csv")) {
        file.remove("manifest.csv")
      }
      
      # Should error (empty message due to stop("", .call = FALSE))
      expect_error(
        projr_content_update()
      )
    }
  )
})

test_that("projr_content_update returns FALSE when no files to restore", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest where raw-data label exists but has no files (empty marker)
      manifest <- data.frame(
        label = "raw-data",
        fn = "",
        version = "v0.0.1",
        hash = "",
        stringsAsFactors = FALSE
      )
      write.csv(manifest, "manifest.csv", row.names = FALSE)
      
      # When no label specified, it looks for raw labels
      # raw-data exists but has no files, so should return FALSE
      result <- projr_content_update()
      expect_false(result)
    }
  )
})

test_that("projr_content_update handles empty directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest with empty label
      manifest <- data.frame(
        label = "raw-data",
        fn = "",
        version = "v0.0.1",
        hash = "",
        stringsAsFactors = FALSE
      )
      write.csv(manifest, "manifest.csv", row.names = FALSE)
      
      # Configure local remote
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      # Should return FALSE and not error
      result <- projr_content_update(label = "raw-data", type = "local")
      expect_false(result)
    }
  )
})

test_that("projr_content_update handles non-existent remote source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create manifest with a label
      .test_content_setup_label("raw-data")
      
      # Setup a local remote
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      projr_yml_dest_add_local(
        title = "existing-title",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      projr_build_patch(msg = "test")
      
      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Try to restore from a non-existent title
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "non-existent-title"
      )
      
      # Should return FALSE
      expect_false(result)
    }
  )
})

# =============================================================================
# Clear Parameter Tests
# =============================================================================

test_that("projr_content_update clear parameter works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup: Create content and build
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      .test_content_setup_label("raw-data")
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      projr_build_patch(msg = "test")
      
      # Add an extra file locally
      extra_file <- file.path(projr_path_get_dir("raw-data", safe = FALSE), "extra.txt")
      writeLines("extra content", extra_file)
      expect_true(file.exists(extra_file))
      
      # Restore without clear - extra file should remain
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "test-local",
        clear = FALSE
      )
      expect_true(result)
      expect_true(file.exists(extra_file))
      
      # Restore with clear - extra file should be removed
      writeLines("extra content again", extra_file)
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "test-local",
        clear = TRUE
      )
      expect_true(result)
      expect_false(file.exists(extra_file))
    }
  )
})

# =============================================================================
# Position Preference Tests
# =============================================================================

test_that("projr_content_update respects pos parameter for dest only", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content
      .test_content_setup_label("raw-data")
      
      # Setup remote as dest only
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
      
      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Restore with pos = "dest" - should work
      result <- projr_content_update(
        label = "raw-data",
        pos = "dest"
      )
      expect_true(result)
      expect_true(length(list.files(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)) > 0)
      
      # Clear local again
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Restore with pos = "source" - should fail (no source configured)
      result <- projr_content_update(
        label = "raw-data",
        pos = "source"
      )
      expect_false(result)
    }
  )
})

# =============================================================================
# Multiple Label Tests
# =============================================================================

test_that("projr_content_update can restore single label successfully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content for raw-data
      .test_content_setup_label("raw-data")
      
      # Setup remote
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      # Build
      projr_build_patch(msg = "test")
      
      # Clear label
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Restore
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "test-local"
      )
      
      # Should be restored
      expect_true(result)
      expect_true(length(list.files(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)) > 0)
    }
  )
})

# =============================================================================
# Archive Structure Tests
# =============================================================================

test_that("projr_content_update restores from archive structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content
      .test_content_setup_label("raw-data")
      
      # Setup archive remote
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      projr_yml_dest_add_local(
        title = "test-archive",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
      )
      
      # Build to create archive
      projr_build_patch(msg = "test")
      
      # Verify archive exists
      version <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(dir.exists(file.path(remote_base, "raw-data", version)))
      
      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Restore from archive
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "test-archive"
      )
      
      expect_true(result)
      expect_true(length(list.files(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)) > 0)
    }
  )
})

# =============================================================================
# Error Recovery Tests
# =============================================================================

test_that("projr_content_update handles partial failures gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content for one label
      .test_content_setup_label("raw-data")
      
      # Setup remote
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      # Build
      projr_build_patch(msg = "test")
      
      # Clear local
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      # Try to restore both raw-data (exists) and cache (doesn't exist)
      # Should handle the error for cache and still succeed for raw-data
      result <- projr_content_update(
        label = c("raw-data", "cache"),
        type = "local",
        title = "test-local"
      )
      
      # Should return FALSE due to cache failure, but raw-data should be restored
      expect_false(result)
      expect_true(length(list.files(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)) > 0)
    }
  )
})

# =============================================================================
# Latest Structure Overwrite Tests
# =============================================================================

test_that("projr_content_update handles latest structure overwrites", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      
      .test_content_setup_label("raw-data")
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      
      # Build version 1
      projr_build_patch(msg = "test v1")
      
      # Modify content
      writeLines("modified", file.path(projr_path_get_dir("raw-data", safe = FALSE), "abc.txt"))
      
      # Build version 2 - should overwrite in latest structure
      projr_build_minor(msg = "test v2")
      
      # Clear and restore - should get version 2
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      
      result <- projr_content_update(
        label = "raw-data",
        type = "local",
        title = "test-local"
      )
      
      expect_true(result)
      content <- readLines(file.path(projr_path_get_dir("raw-data", safe = FALSE), "abc.txt"))
      expect_identical(content, "modified")
    }
  )
})
