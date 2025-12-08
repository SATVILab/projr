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
#
# Note: Comprehensive integration tests are in test-local-remote-comprehensive.R

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

      # Remove if empty should return TRUE and remove the directory
      result_rm <- .remote_final_rm_if_empty(
        "local", remote = remote_dir
      )
      expect_true(result_rm)
      expect_false(dir.exists(remote_dir))

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
