# Integration tests for local remote destinations
# =================================================
#
# These tests run in LITE mode (no skip_if(.is_test_lite())) to increase
# test coverage for local remotes as destinations in LITE test suites.
# They test common, real-world scenarios without exhaustive parameter combinations.
#
# Note: Tests skip only in CRAN mode (skip_if(.is_test_cran()))

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

usethis::with_project(
  path = dir_test,
  code = {
    # Create test content
    content_vec_test_file <- .test_content_setup_label("raw-data") |>
      .file_ls()
    # Avoid pushing to Git
    .yml_git_set_push(FALSE, TRUE, NULL)
    # Remove default remotes
    .yml_dest_rm_type_all("default")
  }
)

# =============================================================================
# Integration tests for common local remote scenarios
# =============================================================================

test_that("local archive with default settings works end-to-end", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local-archive",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
        # Using defaults: send_cue = "if-change", send_strategy = "sync-diff"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      .test_content_setup_label("raw-data")

      # First build - should create archive
      projr_build_patch(msg = "test")
      raw_data_dir <- file.path(remote_base, "raw-data")
      expect_true(dir.exists(raw_data_dir))
      remote_vec_1 <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_1 <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(version_1 %in% remote_vec_1)

      # Second build with no changes - should NOT create new archive (if-change)
      projr_build_patch(msg = "test")
      remote_vec_2 <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_2 <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_false(version_2 %in% remote_vec_2)
      expect_true(version_1 %in% remote_vec_2)

      # Modify content and build - should create new archive
      writeLines("modified", projr_path_get("raw-data", "abc.txt"))
      projr_build_patch(msg = "test")
      remote_vec_3 <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_3 <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(version_3 %in% remote_vec_3)
      expect_true(version_1 %in% remote_vec_3)

      # Restore from archive
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-archive"
      )
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_true(length(fn_vec) > 0)
    }
  )
})

test_that("local latest with always cue works end-to-end", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local-latest",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_cue = "always"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec <- .test_content_setup_label("raw-data") |> .file_ls()

      # First build
      projr_build_patch(msg = "test")
      expect_true(dir.exists(file.path(remote_base, "raw-data")))
      fn_vec <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec, content_vec)

      # Second build - should update (always)
      projr_build_patch(msg = "test")
      fn_vec_2 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_2, content_vec)

      # Restore from latest
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest"
      )
      expect_true(result)
      fn_vec_restored <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec_restored, content_vec)
    }
  )
})

test_that("local remote with sync-purge strategy works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-purge",
        send_cue = "always"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec <- .test_content_setup_label("raw-data") |> .file_ls()

      # First build
      projr_build_patch(msg = "test")
      fn_vec_1 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_1, content_vec)

      # Add a file manually to remote that shouldn't be there
      writeLines("extra", file.path(remote_base, "raw-data", "extra.txt"))
      expect_true(file.exists(file.path(remote_base, "raw-data", "extra.txt")))

      # Build with sync-purge - should remove everything and re-upload
      projr_build_patch(msg = "test")
      fn_vec_2 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_false("extra.txt" %in% fn_vec_2)
      expect_identical(fn_vec_2, content_vec)
    }
  )
})

test_that("local remote with upload-all strategy works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "upload-all",
        send_cue = "always"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec <- .test_content_setup_label("raw-data") |> .file_ls()

      # First build
      projr_build_patch(msg = "test")
      fn_vec_1 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_1, content_vec)

      # Add a file manually to remote
      writeLines("extra", file.path(remote_base, "raw-data", "extra.txt"))

      # Modify a file locally
      writeLines("modified", projr_path_get("raw-data", "abc.txt"))

      # Build with upload-all - should upload all files but NOT remove extra.txt
      projr_build_patch(msg = "test")
      fn_vec_2 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_true("extra.txt" %in% fn_vec_2)
      expect_true("abc.txt" %in% fn_vec_2)
      expect_identical(
        readLines(file.path(remote_base, "raw-data", "abc.txt")),
        "modified"
      )
    }
  )
})

test_that("local remote with upload-missing strategy works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "upload-missing",
        send_cue = "always"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec <- .test_content_setup_label("raw-data") |> .file_ls()

      # First build
      projr_build_patch(msg = "test")
      fn_vec_1 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_1, content_vec)

      # Modify a file locally
      original_content <- readLines(file.path(remote_base, "raw-data", "abc.txt"))
      writeLines("modified", projr_path_get("raw-data", "abc.txt"))

      # Build with upload-missing - should NOT overwrite existing files
      projr_build_patch(msg = "test")
      fn_vec_2 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_2, content_vec)
      # File should NOT be modified
      expect_identical(
        readLines(file.path(remote_base, "raw-data", "abc.txt")),
        original_content
      )

      # Add a new file locally
      writeLines("new", projr_path_get("raw-data", "newfile.txt"))

      # Build - should upload only the new file
      projr_build_patch(msg = "test")
      fn_vec_3 <- .file_ls(file.path(remote_base, "raw-data"))
      expect_true("newfile.txt" %in% fn_vec_3)
      expect_identical(
        readLines(file.path(remote_base, "raw-data", "newfile.txt")),
        "new"
      )
    }
  )
})

test_that("local remote with manifest inspection works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always",
        send_inspect = "manifest"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      .test_content_setup_label("raw-data")

      # First build
      projr_build_patch(msg = "test")
      expect_true(dir.exists(file.path(remote_base, "raw-data")))

      # Add a file manually to remote (not in manifest)
      writeLines("extra", file.path(remote_base, "raw-data", "extra.txt"))

      # Build with manifest inspection - should NOT remove extra.txt
      # because it's not tracked in manifest
      projr_build_patch(msg = "test")
      fn_vec <- .file_ls(file.path(remote_base, "raw-data"))
      expect_true("extra.txt" %in% fn_vec)
    }
  )
})

test_that("local remote with file inspection works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always",
        send_inspect = "file"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      .test_content_setup_label("raw-data")

      # First build
      projr_build_patch(msg = "test")
      expect_true(dir.exists(file.path(remote_base, "raw-data")))

      # Add a file manually to remote
      writeLines("extra", file.path(remote_base, "raw-data", "extra.txt"))

      # Build with file inspection - should remove extra.txt
      projr_build_patch(msg = "test")
      fn_vec <- .file_ls(file.path(remote_base, "raw-data"))
      expect_false("extra.txt" %in% fn_vec)
    }
  )
})

test_that("local remote with multiple labels works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")

      # Add remotes for multiple labels
      projr_yml_dest_add_local(
        title = "local-raw",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
      )

      projr_yml_dest_add_local(
        title = "local-docs",
        content = "docs",
        path = remote_base,
        structure = "latest"
      )

      # Clear and add content
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      .test_content_setup_label("raw-data")

      # Build (this creates docs output)
      projr_build_patch(msg = "test")

      # Verify both remotes exist
      expect_true(dir.exists(file.path(remote_base, "raw-data")))
      expect_true(dir.exists(file.path(remote_base, "docs")))

      # Verify archive structure for raw-data
      raw_data_dir <- file.path(remote_base, "raw-data")
      remote_vec <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(version %in% remote_vec)

      # Verify latest structure for docs
      docs_files <- list.files(file.path(remote_base, "docs"), recursive = TRUE)
      expect_true(length(docs_files) > 0)
    }
  )
})

test_that("local remote handles empty directories correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      remote_base <- .dir_create_tmp_random()
      on.exit(unlink(remote_base, recursive = TRUE), add = TRUE)

      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
      )

      # Ensure raw-data is empty
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }

      # Build with empty directory
      projr_build_patch(msg = "test")

      # Verify empty marker was created
      raw_data_dir <- file.path(remote_base, "raw-data")
      if (dir.exists(raw_data_dir)) {
        remote_vec <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
        version_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
        expect_true(version_empty %in% remote_vec)
      }

      # Add content and build again
      .test_content_setup_label("raw-data")
      projr_build_patch(msg = "test")

      # Verify non-empty version was created
      remote_vec_2 <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_full <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(version_full %in% remote_vec_2)
    }
  )
})
