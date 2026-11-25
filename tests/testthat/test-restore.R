# Dedicated Restore Tests
#
# This file tests restoring directories with projr_source() and the deprecated
# projr_content_update() wrapper
# All tests use local remotes and run in LITE mode for fast iteration
# Tests focus on parameter validation, edge cases, and error handling

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE,
  github = FALSE,
  set_env_var = TRUE
)

# =============================================================================
# Deprecated wrapper
# =============================================================================

test_that("projr_source warns and delegates to projr_source", {
  skip_if(.is_test_select())
  received <- NULL
  testthat::local_mocked_bindings(
    projr_source = function(label, remote, cue, conflict) {
      received <<- list(label, remote, cue, conflict)
      invisible(TRUE)
    }
  )
  expect_warning(
    result <- projr_content_update(
      "raw-data",
      pos = "dest",
      type = "local",
      title = "archive",
      clear = TRUE
    ),
    "deprecated.*`pos` is ignored"
  )
  expect_true(result)
  expect_identical(
    received,
    list("raw-data", "local:archive", "always", "remote")
  )
  expect_warning(projr_content_update(title = "archive"), "deprecated")
  expect_identical(received, list(NULL, "archive", "always", "error"))
  suppressWarnings({
    expect_error(projr_content_update(type = "local"), "Name one with `title`")
    expect_error(
      projr_content_update(destination = "v1"),
      "no longer supported"
    )
  })
})

# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("projr_source reports a remote that is not registered", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .test_content_setup_label("raw-data")
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)
      projr_yml_dest_add_local(
        title = "existing-title",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )
      projr_build_patch(msg = "test")
      expect_error(
        projr_source("raw-data", remote = "local:non-existent-title")
      )
    }
  )
})

# =============================================================================
# Clear Parameter Tests
# =============================================================================

test_that("projr_source conflict setting keeps or replaces local changes", {
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
      extra_file <- file.path(
        projr_path_get_dir("raw-data", safe = FALSE),
        "extra.txt"
      )
      writeLines("extra content", extra_file)
      expect_true(file.exists(extra_file))

      # A local change stops the fetch unless it is kept or replaced
      expect_error(
        projr_source("raw-data", "local:test-local", conflict = "error"),
        "local changes"
      )
      expect_true(file.exists(extra_file))
      expect_true(projr_source(
        "raw-data",
        "local:test-local",
        conflict = "local"
      ))
      expect_true(file.exists(extra_file))
      expect_true(projr_source(
        "raw-data",
        "local:test-local",
        cue = "always",
        conflict = "remote"
      ))
      expect_false(file.exists(extra_file))
    }
  )
})

# =============================================================================
# Multiple Label Tests
# =============================================================================

test_that("projr_source can restore single label successfully", {
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
      result <- projr_source("raw-data", "local:test-local")

      # Should be restored
      expect_true(result)
      expect_true(
        length(list.files(
          projr_path_get_dir("raw-data", safe = FALSE),
          recursive = TRUE
        )) >
          0
      )
    }
  )
})

# =============================================================================
# Archive Structure Tests
# =============================================================================

test_that("projr_source restores from archive structure", {
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
      result <- projr_source("raw-data", "local:test-archive")

      expect_true(result)
      expect_true(
        length(list.files(
          projr_path_get_dir("raw-data", safe = FALSE),
          recursive = TRUE
        )) >
          0
      )
    }
  )
})

# =============================================================================
# Error Recovery Tests
# =============================================================================

test_that("projr_source changes nothing when a label is missing", {
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

      # The remote has raw-data but no cache
      expect_error(
        projr_source(c("raw-data", "cache"), "local:test-local"),
        "has no cache"
      )

      # Every label is checked before anything changes
      expect_length(
        list.files(
          projr_path_get_dir("raw-data", safe = FALSE),
          recursive = TRUE
        ),
        0
      )
    }
  )
})

# =============================================================================
# Latest Structure Overwrite Tests
# =============================================================================

test_that("projr_source handles latest structure overwrites", {
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
      writeLines(
        "modified",
        file.path(projr_path_get_dir("raw-data", safe = FALSE), "abc.txt")
      )

      # Build version 2 - should overwrite in latest structure
      projr_build_minor(msg = "test v2")

      # Clear and restore - should get version 2
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      result <- projr_source("raw-data", "local:test-local")

      expect_true(result)
      content <- readLines(file.path(
        projr_path_get_dir("raw-data", safe = FALSE),
        "abc.txt"
      ))
      expect_identical(content, "modified")
    }
  )
})
