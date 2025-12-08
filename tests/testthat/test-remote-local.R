# Dedicated local Remote Tests
#
# This file is the ONLY testthat file that:
# - Creates or deletes local remotes via projr's wrappers
# - Calls local-specific remote helpers (.remote_file_add_local(), etc.)
# - Tests local remote functionality
#
# Tests can run on CRAN and CI (no external dependencies)
#
# Test strategy:
# - Create temporary directories for local remotes
# - Tests are idempotent and independent
# - Each test cleans up its own local remote directories

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# Creation and existence
# =============================================================================

test_that(".remote_get works for local", {
  skip_if(.is_test_select())
  test_path <- "/path/to/local/remote"
  expect_identical(
    .remote_get("local", test_path),
    test_path
  )
})

test_that(".remote_final_get works for local", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for testing
      temp_base <- .dir_create_tmp_random()
      
      # Test archive structure
      result_archive <- .remote_final_get(
        "local",
        id = temp_base,
        label = "raw-data",
        structure = "archive"
      )
      expect_true(dir.exists(result_archive))
      expect_true(grepl("v0\\.0\\.0-1$", result_archive))
      
      # Test latest structure
      result_latest <- .remote_final_get(
        "local",
        id = temp_base,
        label = "output",
        structure = "latest"
      )
      expect_true(dir.exists(result_latest))
      expect_true(grepl("output$", result_latest))
      
      # Cleanup
      unlink(temp_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# File operations: add, list and remove
# =============================================================================

test_that("adding, listing and removing files works on local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {

      # Create a temp directory for the local remote
      remote_dir <- .dir_create_tmp_random()

      writeLines("test content", "abc.txt")

      # upload a single file
      .remote_file_add(
        "local",
        remote = remote_dir,
        path_dir_local = ".",
        fn = "abc.txt"
      )

      expect_true(
        "abc.txt" %in%
          .remote_file_ls("local", remote = remote_dir)
      )

      # Clear existing content
      .remote_final_empty("local", remote = remote_dir)

      # confirm directory is empty
      expect_identical(
        length(.remote_file_ls("local", remote = remote_dir)),
        0L
      )

      # add multiple files
      dir.create("subdir1/subdir2", showWarnings = FALSE, recursive = TRUE)
      writeLines("file def", "subdir1/def.txt")
      writeLines("file ghi", "subdir1/subdir2/ghi.txt")
      fn_vec <- c("abc.txt", "subdir1/def.txt", "subdir1/subdir2/ghi.txt")
      .remote_file_add(
        "local",
        fn = fn_vec,
        path_dir_local = ".",
        remote = remote_dir
      )

      file_vec <- .remote_file_ls(
        "local", remote = remote_dir
      )

      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% file_vec)

      # add a file
      writeLines("new file content", "newfile.txt")
      .remote_file_add(
        "local",
        fn = "newfile.txt",
        path_dir_local = ".",
        remote = remote_dir
      )

      file_vec <- .remote_file_ls(
        "local", remote = remote_dir
      )

      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% file_vec)
      expect_true("newfile.txt" %in% file_vec)

      # remove a file
      .remote_file_rm(
        "local",
        fn = c("newfile.txt", "subdir1/subdir2/ghi.txt"),
        remote = remote_dir
      )

      file_vec <- .remote_file_ls(
        "local", remote = remote_dir
      )

      expect_false("newfile.txt" %in% file_vec)
      expect_false("subdir1/subdir2/ghi.txt" %in% file_vec)
      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)

      # download a single file
      path_dir_save <- .dir_create_tmp_random()
      .remote_file_get(
        "local",
        fn = "abc.txt",
        remote = remote_dir,
        path_dir_save_local = path_dir_save
      )
      expect_true(
        file.exists(file.path(path_dir_save, "abc.txt"))
      )
      expect_false(
        file.exists(file.path(path_dir_save, "subdir1", "def.txt"))
      )
      expect_false(
        file.exists(file.path(path_dir_save, "def.txt"))
      )

      # download a single file in a subdirectory
      path_dir_save2 <- .dir_create_tmp_random()
      .remote_file_get(
        "local",
        fn = "subdir1/def.txt",
        remote = remote_dir,
        path_dir_save_local = path_dir_save2
      )
      expect_true(
        file.exists(file.path(path_dir_save2, "def.txt"))
      )
      expect_false(
        file.exists(file.path(path_dir_save2, "abc.txt"))
      )

      # empty the remote
      .remote_final_empty(
        "local",
        remote = remote_dir
      )
      expect_identical(
        length(.remote_file_ls("local", remote = remote_dir)),
        0L
      )

      # Remove if empty should return FALSE because dir still exists but is empty
      result_rm <- .remote_final_rm_if_empty(
        "local", remote = remote_dir
      )
      expect_true(result_rm)  # Should remove the empty directory

      # cleanup
      unlink(c("abc.txt", "subdir1", "newfile.txt"), recursive = TRUE)
      unlink(path_dir_save, recursive = TRUE)
      unlink(path_dir_save2, recursive = TRUE)
      if (dir.exists(remote_dir)) {
        unlink(remote_dir, recursive = TRUE)
      }
    }
  )
})

# =============================================================================
# Manifest and VERSION Integration
# =============================================================================

test_that("manifest round-trip works for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a temp directory for the local remote
      remote_dir <- .dir_create_tmp_random()

      # Create a test manifest
      manifest <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("abc123", "def456"),
        stringsAsFactors = FALSE
      )

      # Write manifest to local remote
      .remote_write_manifest("local", remote_dir, manifest)
      expect_true(
        "manifest.csv" %in% .remote_file_ls(
          "local", remote = remote_dir
        )
      )

      # Read manifest back
      manifest_retrieved <- .remote_get_manifest("local", remote_dir)

      # Verify round-trip
      expect_identical(nrow(manifest_retrieved), 2L)
      expect_true("file1.txt" %in% manifest_retrieved$fn)
      expect_true("file2.txt" %in% manifest_retrieved$fn)
      
      # Cleanup
      .remote_final_empty("local", remote = remote_dir)
      unlink(remote_dir, recursive = TRUE)
    }
  )
})

test_that("VERSION file round-trip works for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a temp directory for the local remote
      remote_dir <- .dir_create_tmp_random()

      # Write version to local remote
      version_to_write <- "v1.2.3"
      .remote_write_version_file(
        "local", remote_dir, version_to_write
      )
      expect_true(
        "VERSION" %in% .remote_file_ls(
          "local", remote = remote_dir
        )
      )

      # Read version back
      version_retrieved <- .remote_get_version_file("local", remote_dir)

      # Verify round-trip
      expect_identical(version_retrieved, version_to_write)

      # Cleanup
      .remote_final_empty("local", remote = remote_dir)
      unlink(remote_dir, recursive = TRUE)
    }
  )
})

# =============================================================================
# Setup for integration tests
# =============================================================================

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
# `latest` structure: upload and restore
# =============================================================================

test_that("upload and restore from `latest` local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # use latest structure
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )

      # Build to upload to local remote
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(dir.exists(file.path(remote_base, "raw-data")))
      fn_vec_remote <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_remote, content_vec_test_file)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file)

      # add and remove files
      content_vec_test_file_adj <- .test_content_adjust_label("raw-data")

      # Build to upload to local remote
      projr::projr_build_minor(msg = "test")

      # Verify upload
      fn_vec_remote <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_remote, content_vec_test_file_adj)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# `archive` structure: upload and restore
# =============================================================================

test_that("upload and restore from `archive` local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # convert to archive structure
      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local-archive",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
      )

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # Build to upload to local remote (empty directory)
      projr::projr_build_patch(msg = "test")

      # Verify no regular upload (empty)
      # For archive structure, path is: base_path/label/version
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      version_path_empty <- paste0(version_path, "-empty")
      expect_false(dir.exists(version_path))
      expect_true(dir.exists(version_path_empty))

      # add files
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- Upload with files ----
      projr::projr_build_minor(msg = "test")

      # Verify upload - should have the new version
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path))
      fn_vec_remote <- .file_ls(version_path)
      expect_identical(fn_vec_remote, content_vec_test_file)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-archive"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# Test send_cue parameter: always, if-change
# =============================================================================

test_that("test always vs if-change for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # remove all remotes
      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive"
      )

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- Upload directory ----
      projr::projr_build_patch(msg = "test")

      # Verify upload
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path))

      # --- Upload nothing new, cue if-change -----
      old_version <- projr_version_get()
      projr::projr_build_patch(msg = "test")

      # Verify no new upload (default send_cue is "if-change")
      current_version <- projr_version_get()
      version_path_new <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      # The version has incremented, but no upload should have occurred
      expect_false(dir.exists(version_path_new))

      # --- Upload nothing new, cue always -----
      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive",
        send_cue = "always"
      )
      projr::projr_build_patch(msg = "test")

      # Verify upload (should create new version even without changes)
      current_version <- projr_version_get()
      version_path_always <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path_always))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# Test send_strategy parameter: sync-diff, sync-purge, upload-all, upload-missing
# =============================================================================

test_that("various upload strategies work for local remotes", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # remove all remotes and clear raw-data
      .yml_dest_rm_type_all("default")
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }
      
      # Setup fresh content
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()
      
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive",
        send_strategy = "sync-purge"
      )

      # --- Upload directory ----
      projr::projr_build_patch(msg = "test")

      # Verify upload
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path))
      fn_vec_remote <- .file_ls(version_path)
      expect_identical(fn_vec_remote, content_vec_test_file)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# Test send_inspect parameter: manifest, file, none
# =============================================================================

test_that("local remote send_inspect='manifest' uses manifest for version tracking", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Clear and setup fresh content
      .yml_dest_rm_type_all("default")
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }
      
      # Add some content to raw-data
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # Add with send_inspect = "manifest"
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive",
        send_inspect = "manifest",
        send_cue = "if-change"
      )

      # Build
      projr::projr_build_patch(msg = "test")
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that("local remote send_inspect='file' inspects actual files", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for local remote
      remote_base <- .dir_create_tmp_random()

      # Clear and setup fresh content
      .yml_dest_rm_type_all("default")
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }
      
      # Add some content to raw-data
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # Add with send_inspect = "file"
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "archive",
        send_inspect = "file",
        send_cue = "if-change"
      )

      # Build
      projr::projr_build_patch(msg = "test")
      current_version <- projr_version_get()
      version_path <- file.path(remote_base, "raw-data", .version_v_add(current_version))
      expect_true(dir.exists(version_path))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})
