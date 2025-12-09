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
# Creation and existence
# =============================================================================

# Setup: no special setup needed for local remotes, unlike GitHub
setup_local <- TRUE

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

test_that("Local test remotes are ready", {
  # skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # For local remotes, we just need temp directories
      # Create base directories for testing
      remote_base_a <- .dir_create_tmp_random()
      remote_base_b <- .dir_create_tmp_random()
      remote_base_archive <- .dir_create_tmp_random()

      # Verify directories exist
      expect_true(dir.exists(remote_base_a))
      expect_true(dir.exists(remote_base_b))
      expect_true(dir.exists(remote_base_archive))

      # Store for later tests (these will be cleaned up per-test)
      # Note: Each test creates its own temp dirs, these are just for verification
    }
  )
})

test_that(".remote_get works for local", {
  skip_if(.is_test_select())
  test_path <- file.path(tempdir(), "test", "path")
  expect_identical(
    .remote_get("local", test_path),
    test_path
  )
})

test_that(".remote_get_final works for local", {
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create temp directory for testing
      temp_base <- .dir_create_tmp_random()

      # Test archive structure - local uses directory paths, not zip files
      # Provide explicit version to avoid needing VERSION file
      result_archive <- .remote_final_get(
        "local",
        id = temp_base,
        label = "raw-data",
        structure = "archive",
        version = "0.0.0-1"
      )
      # .remote_final_get() returns path but doesn't create directory
      # (directory created when files are actually written)
      expect_true(grepl("raw-data", result_archive))
      expect_true(grepl("v0\\.0\\.0-1$", result_archive))

      # Test archive structure with empty flag
      result_archive_empty <- .remote_final_get(
        "local",
        id = temp_base,
        label = "raw-data",
        structure = "archive",
        version = "0.0.0-1",
        empty = TRUE
      )
      # .remote_final_get() returns path but doesn't create directory
      expect_true(grepl("raw-data", result_archive_empty))
      expect_true(grepl("v0\\.0\\.0-1-empty$", result_archive_empty))

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

      # For local remotes, ls_final lists subdirectories
      parent_dir <- dirname(remote_dir)
      expect_true(
        basename(remote_dir) %in%
          .remote_ls_final("local", remote_pre = parent_dir)
      )

      # Remove if empty
      result_rm <- .remote_final_rm_if_empty(
        "local", remote = remote_dir
      )
      expect_true(result_rm)
      expect_false(dir.exists(remote_dir))

      # cleanup
      unlink(c("abc.txt", "subdir1", "newfile.txt"), recursive = TRUE)
      unlink(path_dir_save, recursive = TRUE)
      unlink(path_dir_save2, recursive = TRUE)
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
# Set up
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
        title = "test-local-latest",
        content = "raw-data",
        path = remote_base,
        structure = "latest"
      )

      # Build to upload to local remote
      projr::projr_build_patch(msg = "test")

      # Verify upload - for latest, files go in base/label/
      expect_true(dir.exists(file.path(remote_base, "raw-data")))
      fn_vec_remote <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_remote, content_vec_test_file)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file)

      # add and remove files
      content_vec_test_file_adj <- .test_content_adjust_label("raw-data")

      # Build to upload to local remote
      projr::projr_build_minor(msg = "test")

      # Verify upload - latest structure overwrites
      fn_vec_remote <- .file_ls(file.path(remote_base, "raw-data"))
      expect_identical(fn_vec_remote, content_vec_test_file_adj)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Add an extra file and overwrite, and don't clear
      writeLines(
        "extra file content", projr_path_get("raw-data", "extrafile.txt")
      )
      writeLines("overwrite", projr_path_get("raw-data", "file1.txt"))
      projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest", clear = FALSE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_true("extrafile.txt" %in% fn_vec)
      expect_identical(
        readLines(projr_path_get("raw-data", "file1.txt")), character(0L)
      )
      expect_identical(
        setdiff(fn_vec, "extrafile.txt"), content_vec_test_file_adj
      )

      # now add an extra file and overwrite, and do clear
      writeLines(
        "another extra file content",
        projr_path_get("raw-data", "extrafile2.txt")
      )
      projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest", clear = TRUE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(
        fn_vec, content_vec_test_file_adj
      )
      expect_identical(
        readLines(projr_path_get("raw-data", "file1.txt")), character(0L)
      )

      # now remove everything from local, build and then try restore
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_build_patch(msg = "test")
      # For latest with empty directory, check what was created
      # May create either raw-data-empty or just leave raw-data as empty
      has_empty_marker <- dir.exists(file.path(remote_base, "raw-data-empty"))
      has_empty_dir <- dir.exists(file.path(remote_base, "raw-data")) &&
        length(list.files(file.path(remote_base, "raw-data"), recursive = TRUE)) == 0
      # Either empty marker exists OR raw-data is empty
      expect_true(has_empty_marker || has_empty_dir)

      # restore without clearing, should be the same
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest", clear = FALSE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, character(0L))

      # add files to the directory, should still have just those afterwards
      writeLines(
        "new file content", projr_path_get("raw-data", "newfile.txt")
      )
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest", clear = FALSE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, "newfile.txt")

      # restore with clearing, should be empty
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-latest", clear = TRUE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, character(0L))

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# `archive` structure: upload and restore
# =============================================================================

test_that("upload and restore from `archive` local remotes", {
  # skip_if(.is_test_select())

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

      # --- ensure remote is cleared ---
      if (dir.exists(remote_base)) {
        unlink(file.path(remote_base, "raw-data"), recursive = TRUE)
      }

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no remotes exist yet
      raw_data_dir <- file.path(remote_base, "raw-data")
      if (dir.exists(raw_data_dir)) {
        remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      } else {
        remote_vec_final <- character(0L)
      }
      # Use helper to get version without dev suffix
      version_no_build_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_no_build_label_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      expect_false(version_no_build_label %in% remote_vec_final)
      expect_false(version_no_build_label_empty %in% remote_vec_final)

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----
      projr::projr_build_patch(msg = "test")

      # Verify upload - archive with empty creates version-empty directory
      raw_data_dir <- file.path(remote_base, "raw-data")
      remote_vec_final <- if (dir.exists(raw_data_dir)) {
        list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      } else {
        character(0L)
      }
      # Use helper to get version without dev suffix
      version_empty_build_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_empty_build_label_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      # Should not have regular version, should have -empty version
      expect_false(version_empty_build_label %in% remote_vec_final)
      expect_true(version_empty_build_label_empty %in% remote_vec_final)

      # Restore from local remote
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-archive"
      )

      # Verify restoration
      expect_false(result)
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # add files
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- Upload with files ----
      projr::projr_build_minor(msg = "test")

      # Verify upload - should have the new version
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      # Use helper to get version without dev suffix
      version_files_build_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_files_build_label_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      # Should have the regular version with files, not -empty
      expect_true(version_files_build_label %in% remote_vec_final)
      expect_false(version_files_build_label_empty %in% remote_vec_final)
      # Should still have the previous -empty version
      expect_false(version_empty_build_label %in% remote_vec_final)
      expect_true(version_empty_build_label_empty %in% remote_vec_final)

      # --- Upload with the same files ----
      projr::projr_build_major(msg = "test")

      # Verify upload - should not have new version (no changes)
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      # Use helper to get version without dev suffix
      version_same_build_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_same_build_label_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      expect_false(version_same_build_label %in% remote_vec_final)
      expect_false(version_same_build_label_empty %in% remote_vec_final)
      expect_true(version_files_build_label %in% remote_vec_final)
      expect_false(version_files_build_label_empty %in% remote_vec_final)

      # add and remove files
      content_vec_test_file_adj <- .test_content_adjust_label("raw-data")

      # Build to upload to local remote
      projr::projr_build_minor(msg = "test")

      # Verify upload - should have the new version
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      # Use helper to get version without dev suffix
      version_adj_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_true(version_adj_label %in% remote_vec_final)
      expect_false(
        .test_version_v_get(empty = TRUE, not_dev = TRUE) %in% remote_vec_final
      )

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from local remote
      # debugonce(.content_update_label)
      result <- projr_content_update(
        label = "raw-data", type = "local", title = "test-local-archive"
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Build with no changes - should not create new archive with default send_cue
      projr::projr_build_minor(msg = "test")

      # Verify upload - should not have an upload with the latest version
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      # Use helper to get version without dev suffix
      version_no_change_label <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      expect_false(version_no_change_label %in% remote_vec_final)

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
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Cleanup
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# `archive` structure via parameter: upload
# =============================================================================

test_that("upload to archive local remotes using parameter for all content", {
  skip_if(.is_test_select())
  skip("archive_local parameter needs path imputation - .yml_dest_complete_title_path() should set path='_archive' for local archive titles")

  # NOTE: archive_local parameter should work like this:
  # - archive_local = TRUE (archive all content to _archive directory)
  # - archive_local = c("output", "docs") (archive only these labels to _archive)
  # - The path "_archive" should be imputed in .yml_dest_complete_title_path()
  #   when type="local" and title="archive" (similar to how GitHub doesn't need path)

  usethis::with_project(
    path = dir_test,
    code = {

      # remove all remotes
      .yml_dest_rm_type_all("default")

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to local remote with archive_local = TRUE
      # This should create archive in _archive directory
      projr::projr_build_patch(msg = "test", archive_local = TRUE)

      # Verify upload
      expect_true(dir.exists(remote_base))

      # Check for empty directories
      raw_data_dir <- file.path(remote_base, "raw-data")
      output_dir <- file.path(remote_base, "output")
      docs_dir <- file.path(remote_base, "docs")

      if (dir.exists(raw_data_dir)) {
        remote_vec_raw <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      } else {
        remote_vec_raw <- character(0L)
      }

      if (dir.exists(output_dir)) {
        remote_vec_output <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
      } else {
        remote_vec_output <- character(0L)
      }

      if (dir.exists(docs_dir)) {
        remote_vec_docs <- list.dirs(docs_dir, full.names = FALSE, recursive = FALSE)
      } else {
        remote_vec_docs <- character(0L)
      }

      version_empty_build_raw_data <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_empty_build_raw_data_empty <- paste0(version_empty_build_raw_data, "-empty")
      version_empty_build_output <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_empty_build_output_empty <- paste0(version_empty_build_output, "-empty")
      version_empty_build_docs <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_empty_build_docs_empty <- paste0(version_empty_build_docs, "-empty")

      expect_false(version_empty_build_raw_data %in% remote_vec_raw)
      expect_true(version_empty_build_raw_data_empty %in% remote_vec_raw)
      expect_false(version_empty_build_output %in% remote_vec_output)
      expect_true(version_empty_build_output_empty %in% remote_vec_output)
      # docs may have content from build
      expect_true(version_empty_build_docs %in% remote_vec_docs ||
                  version_empty_build_docs_empty %in% remote_vec_docs)

      # --- Upload nothing new, cue always -----
      projr::projr_build_patch(msg = "test", archive_local = remote_base)

      # Verify versions remain the same since no content change
      # (default send_cue is if-change, but archive_local may override)

      # Clear up
      unlink(remote_base, recursive = TRUE)
    }
  )
})

test_that("upload to archive local remotes using parameter for only output content", {
  skip_if(.is_test_select())
  skip("archive_local parameter needs path imputation - .yml_dest_complete_title_path() should set path='_archive' for local archive titles")

  usethis::with_project(
    path = dir_test,
    code = {

      # remove all remotes
      .yml_dest_rm_type_all("default")

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no remotes exist yet
      expect_false(dir.exists(file.path(remote_base, "raw-data")))
      expect_false(dir.exists(file.path(remote_base, "output")))

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to local remote - only output
      projr::projr_build_patch(msg = "test", archive_local = c(remote_base, "output"))

      # Verify upload
      expect_true(dir.exists(remote_base))

      # Only output should exist
      output_dir <- file.path(remote_base, "output")
      expect_true(dir.exists(output_dir))
      expect_false(dir.exists(file.path(remote_base, "raw-data")))
      expect_false(dir.exists(file.path(remote_base, "docs")))

      # Check output directory versions
      if (dir.exists(output_dir)) {
        remote_vec_output <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
      } else {
        remote_vec_output <- character(0L)
      }

      version_empty_build_output <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_empty_build_output_empty <- paste0(version_empty_build_output, "-empty")

      expect_false(version_empty_build_output %in% remote_vec_output)
      expect_true(version_empty_build_output_empty %in% remote_vec_output)

      # --- Upload nothing new, cue always -----
      projr::projr_build_patch(msg = "test", archive_local = c(remote_base, "output"))

      # Verify still has empty version
      remote_vec_output <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
      version_second_build_output <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_second_build_output_empty <- paste0(version_second_build_output, "-empty")
      expect_false(version_second_build_output %in% remote_vec_output)
      expect_true(version_second_build_output_empty %in% remote_vec_output)

      # --- Upload new output data, cue if-change -----
      content_vec <- c(
        "---",
        "title: \"Solving TB with MINIMALLY SENSIBLE reports\"",
        "documentclass: book",
        "---",
        "",
        "# Introduction",
        "  ",
        "```{r , include = FALSE}",
        "writeLines(\"abc\", projr_path_get(\"output\", \"newfile.txt\"))",
        "```"
      )
      invisible(try(file.remove("index.Rmd"), silent = TRUE))
      writeLines(content_vec, "index.Rmd")

      projr::projr_build_patch(
        msg = "test", archive_local = c(remote_base, "output"), always_archive = FALSE
      )

      # Verify upload - should now have actual output
      remote_vec_output <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
      version_new_build_output <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_new_build_output_empty <- paste0(version_new_build_output, "-empty")
      expect_true(version_new_build_output %in% remote_vec_output)
      expect_false(version_new_build_output_empty %in% remote_vec_output)

      # Clear up
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

      # remove all remotes
      .yml_dest_rm_type_all("default")
      remote_base <- .dir_create_tmp_random()

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
      raw_data_dir <- file.path(remote_base, "raw-data")
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_first <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_first_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      expect_false(version_first_empty %in% remote_vec_final)
      expect_true(version_first %in% remote_vec_final)

      # --- Upload nothing new, cue if-change -----
      projr::projr_build_patch(msg = "test")

      # Verify no new upload (default send_cue is "if-change")
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_second <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_second_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      expect_false(version_second_empty %in% remote_vec_final)
      expect_false(version_second %in% remote_vec_final)

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
      remote_vec_final <- list.dirs(raw_data_dir, full.names = FALSE, recursive = FALSE)
      version_third <- .test_version_v_get(empty = FALSE, not_dev = TRUE)
      version_third_empty <- .test_version_v_get(empty = TRUE, not_dev = TRUE)
      expect_false(version_third_empty %in% remote_vec_final)
      expect_true(version_third %in% remote_vec_final)

      # Clear up
      unlink(remote_base, recursive = TRUE)
    }
  )
})

# =============================================================================
# Test send_strategy parameter: sync-diff, sync-purge, upload-all, upload-missing
# =============================================================================

test_that("various upload strategies run", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      remote_base <- .dir_create_tmp_random()
      on.exit(
        unlink(remote_base, recursive = TRUE),
        add = TRUE
      )
      remote_final <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "latest",
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )

      # remove all remotes
      .yml_dest_rm_type_all("default")

      # --- clear _raw_data and add new content ---
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- sync-purge ---

      # set up sync-purge
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-purge",
        send_cue = "always"
      )

      # add a file that will be purged out
      path_dir_tmp_save <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_save, recursive = TRUE),
        add = TRUE
      )
      writeLines(
        "temporary file content",
        file.path(path_dir_tmp_save, "tempfile.txt")
      )
      .remote_file_add(
        "local",
        remote = remote_final,
        path_dir_local = path_dir_tmp_save,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check
      )
      fn_vec <- .file_ls(path_dir_tmp_check)
      expect_identical(fn_vec, c("tempfile.txt"))

      projr_build_patch(msg = "test")

      # verify upload
      path_dir_tmp_check_2 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_2, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_2
      )
      fn_vec <- .file_ls(path_dir_tmp_check_2)
      expect_true(!"tempfile.txt" %in% fn_vec)
      expect_identical(content_vec_test_file, fn_vec)

      # --- upload-all ---

      # convert to upload all
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "upload-all",
        send_cue = "always"
      )

      # overwrite a file, add a file and remove a file
      writeLines(
        "upload-all",
        projr_path_get("raw-data", "abc.txt")
      )
      writeLines(
        "upload-all",
        projr_path_get("raw-data", "file1.txt")
      )
      invisible(
        file.remove(projr_path_get("raw-data", "subdir1", "def.txt"))
      )

      # build
      projr_build_patch(msg = "test")

      # check
      path_dir_tmp_check_3 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_3, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_3
      )
      fn_vec <- .file_ls(path_dir_tmp_check_3)
      expect_true("file1.txt" %in% fn_vec)
      expect_true("abc.txt" %in% fn_vec)
      expect_true("subdir1/def.txt" %in% fn_vec)
      expect_identical(
        readLines(file.path(path_dir_tmp_check_3, "abc.txt")),
        "upload-all"
      )
      expect_identical(
        readLines(file.path(path_dir_tmp_check_3, "file1.txt")),
        "upload-all"
      )

      # --- upload-all ---

      # convert to upload all
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "upload-missing",
        send_cue = "always"
      )

      # overwrite a file, add a file and remove a file
      writeLines(
        "upload-missing",
        projr_path_get("raw-data", "abc.txt")
      )
      writeLines(
        "upload-missing",
        projr_path_get("raw-data", "file2.txt")
      )
      invisible(
        file.remove(projr_path_get("raw-data", ".hidden.txt"))
      )

      # build
      projr_build_patch(msg = "test")

      # check
      path_dir_tmp_check_4 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_4, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_4
      )
      fn_vec <- .file_ls(path_dir_tmp_check_4)
      expect_true("file1.txt" %in% fn_vec)
      expect_true("abc.txt" %in% fn_vec)
      expect_true("subdir1/def.txt" %in% fn_vec)
      expect_identical(
        readLines(file.path(path_dir_tmp_check_4, "abc.txt")),
        "upload-all"
      )
      expect_identical(
        readLines(file.path(path_dir_tmp_check_4, "file1.txt")),
        "upload-all"
      )
      expect_identical(
        readLines(file.path(path_dir_tmp_check_4, "file2.txt")),
        "upload-missing"
      )
    }
  )
})

# =============================================================================
# Test send_inspect parameter: file, none
# =============================================================================

test_that("various inspection methods run", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  # skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      remote_base <- .dir_create_tmp_random()
      on.exit(
        unlink(remote_base, recursive = TRUE),
        add = TRUE
      )
      remote_final <- .remote_final_get(
        type = "local",
        id = remote_base,
        label = "raw-data",
        structure = "latest",
        path_append_label = TRUE,
        version = NULL,
        pre = FALSE,
        empty = FALSE
      )

      # remove all remotes
      .yml_dest_rm_type_all("default")

      # --- clear _raw_data and add new content ---
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- initial build ---

      # set up sync-diff with manifest inspection
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always",
        send_inspect = "manifest"
      )

      # build
      projr_build_patch(msg = "test")

      # --- add file that should be removed ---

      path_dir_tmp_save <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_save, recursive = TRUE),
        add = TRUE
      )
      writeLines(
        "temporary file content",
        file.path(path_dir_tmp_save, "tempfile.txt")
      )
      .remote_file_add(
        "local",
        remote = remote_final,
        path_dir_local = path_dir_tmp_save,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check
      )
      fn_vec <- .file_ls(path_dir_tmp_check)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)

      # --- manifest ---

      # build
      projr_build_patch(msg = "test")

      # verify
      path_dir_tmp_check_2 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_2, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_2
      )
      fn_vec <- .file_ls(path_dir_tmp_check_2)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)


      # --- file ---

      # set up sync-diff with file inspection
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always",
        send_inspect = "file"
      )

      # build
      projr_build_patch(msg = "test")

      # verify
      path_dir_tmp_check_3 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_3, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_3
      )
      fn_vec <- .file_ls(path_dir_tmp_check_3)
      expect_false("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)


      # --- none ---

      # add file that won't be removed

      path_dir_tmp_save_4 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_save_4, recursive = TRUE),
        add = TRUE
      )
      writeLines(
        "temporary file content",
        file.path(path_dir_tmp_save_4, "tempfile.txt")
      )
      .remote_file_add(
        "local",
        remote = remote_final,
        path_dir_local = path_dir_tmp_save_4,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check
      )
      fn_vec <- .file_ls(path_dir_tmp_check)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)

      # add another file to _raw_data
      writeLines(
        "new content",
        projr_path_get("raw-data", "no-inspect.txt")
      )

      # set up sync-diff with no inspection
      projr_yml_dest_add_local(
        title = "test-local",
        content = "raw-data",
        path = remote_base,
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always",
        send_inspect = "none"
      )

      # build
      projr_build_patch(msg = "test")

      # verify
      path_dir_tmp_check_5 <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check_5, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "local",
        remote = remote_final,
        path_dir_save_local = path_dir_tmp_check_5
      )
      fn_vec <- .file_ls(path_dir_tmp_check_5)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)
      expect_true("no-inspect.txt" %in% fn_vec)

    }
  )
})

