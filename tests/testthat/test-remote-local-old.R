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
      expect_identical(
        length(.remote_ls_final("local", remote_pre = dirname(remote_dir))),
        1L  # The remote_dir itself still exists as a subdirectory
      )

      expect_false(
        .remote_final_rm_if_empty(
          "local", remote = remote_dir
        )
      )

      # cleanup
      unlink(c("abc.txt", "subdir1", "newfile.txt"), recursive = TRUE)
      unlink(path_dir_save, recursive = TRUE)
      unlink(path_dir_save2, recursive = TRUE)
      unlink(remote_dir, recursive = TRUE)
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

test_that("upload and restore from `latest` GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      tag_name <- "projr-test-release-a"

      # use latest structure
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      # Build to upload to GitHub
      projr::projr_build_patch(msg = "test")

      # Verify upload
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      expect_true("raw-data.zip" %in% remote_vec_final)
      expect_false("raw-data-empty.zip" %in% remote_vec_final)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from GitHub
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file)

      # add and remove files
      content_vec_test_file_adj <- .test_content_adjust_label("raw-data")

      # Build to upload to GitHub
      projr::projr_build_minor(msg = "test")

      # Verify upload
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      expect_true("raw-data.zip" %in% remote_vec_final)
      expect_false("raw-data-empty.zip" %in% remote_vec_final)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from GitHub
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name
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
        label = "raw-data", type = "github", title = tag_name, clear = FALSE
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
        label = "raw-data", type = "github", title = tag_name, clear = TRUE
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
      asset_vec <- .remote_ls_final(
        "github", c("tag" = tag_name)
      )
      expect_true("raw-data-empty.zip" %in% asset_vec)
      expect_false("raw-data.zip" %in% asset_vec)

      # restore without clearing, should be the same
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name, clear = FALSE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, character(0L))

      # add files to the directory, should still have just those afterwards
      writeLines(
        "new file content", projr_path_get("raw-data", "newfile.txt")
      )
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name, clear = FALSE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, "newfile.txt")

      # restore with clearing, should be empty
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name, clear = TRUE
      )
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, character(0L))

      # Clear up
      remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
      for (fn in remote_final_vec) {
        .remote_final_empty(
          "github",
          remote = c("tag" = tag_name, "fn" = fn)
        )
      }
    }
  )
})

# =============================================================================
# `archive` structure: upload and restore
# =============================================================================

test_that("upload and restore from `archive` GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      # convert to archive structure
      .yml_dest_rm_type_all("default")
      tag_name <- "projr-test-release-a"
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", tag_name)) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no remotes exist yet
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_no_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_no_build_label %in% remote_vec_final)
      expect_false(version_no_build_label_empty %in% remote_vec_final)

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to GitHub
      # debugonce(.dest_send_label)
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_empty_build_label %in% remote_vec_final)
      expect_true(version_empty_build_label_empty %in% remote_vec_final)

      # Restore from GitHub
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name
      )

      # Verify restoration
      expect_false(result)
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # add files
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- Upload with files ----

      # Build to upload to GitHub
      projr::projr_build_minor(msg = "test")

      # Verify upload - should have the new version
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_files_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_files_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_true(version_files_build_label %in% remote_vec_final)
      expect_false(version_files_build_label_empty %in% remote_vec_final)
      expect_false(version_empty_build_label %in% remote_vec_final)
      expect_true(version_empty_build_label_empty %in% remote_vec_final)

      # --- Upload with the same files ----

      # Build to upload to GitHub
      projr::projr_build_major(msg = "test")

      # Verify upload - should have the new version
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_same_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_same_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_same_build_label %in% remote_vec_final)
      expect_false(version_same_build_label_empty %in% remote_vec_final)
      expect_true(version_files_build_label %in% remote_vec_final)
      expect_false(version_files_build_label_empty %in% remote_vec_final)

      # add and remove files
      content_vec_test_file_adj <- .test_content_adjust_label("raw-data")

      # Build to upload to GitHub
      projr::projr_build_minor(msg = "test")

      # Verify upload - should have the new version
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      expect_true(.test_label_version_get("raw-data") %in% remote_vec_final)
      expect_false(
        .test_label_version_get("raw-data", TRUE) %in% remote_vec_final
      )

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from GitHub
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Build with no changes - should not create new archive with default send_cue # nolint
      projr::projr_build_minor(msg = "test")

      # Verify upload - should not have an upload with the latest version
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      expect_false(.test_label_version_get("raw-data") %in% remote_vec_final)

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      projr_path_get_dir("raw-data", safe = FALSE)

      # Restore from GitHub
      result <- projr_content_update(
        label = "raw-data", type = "github", title = tag_name
      )

      # Verify restoration
      expect_true(result)
      fn_vec <- .file_ls(projr_path_get("raw-data"))
      expect_identical(fn_vec, content_vec_test_file_adj)

      # Clear up
      remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
      for (fn in remote_final_vec) {
        .remote_final_empty(
          "github",
          remote = c("tag" = tag_name, "fn" = fn)
        )
      }
    }
  )
})

# =============================================================================
# `archive` structure via parameter: upload
# =============================================================================

test_that("upload to archive remotes using parameter for all content", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      # remove all remotes
      .yml_dest_rm_type_all("default")
      tag_name <- "archive"

      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", tag_name)) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no remotes exist yet
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_no_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_no_build_label %in% remote_vec_final)
      expect_false(version_no_build_label_empty %in% remote_vec_final)

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to GitHub
      projr::projr_build_patch(msg = "test", archive_github = TRUE)

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_true(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_false(version_empty_build_output %in% remote_vec_final)
      expect_true(version_empty_build_output_empty %in% remote_vec_final)
      expect_true(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)

      # --- Upload nothing new, cue always -----
      projr::projr_build_patch(msg = "test", archive_github = TRUE)

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_true(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_false(version_empty_build_output %in% remote_vec_final)
      expect_true(version_empty_build_output_empty %in% remote_vec_final)
      expect_true(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)

        # --- Upload nothing new, cue if-change -----
        projr::projr_build_patch(
          msg = "test", archive_github = TRUE, always_archive = FALSE
        )

        # Verify upload
        expect_true(
          .remote_check_exists("github", tag_name, max_attempts = 4)
        )
        remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
        version_empty_build_raw_data <- .test_label_version_get(
          "raw-data", empty = FALSE
        )
        version_empty_build_raw_data_empty <- .test_label_version_get(
          "raw-data", empty = TRUE
        )
        version_empty_build_output <- .test_label_version_get(
          "output", empty = FALSE
        )
        version_empty_build_output_empty <- .test_label_version_get(
          "output", empty = TRUE
        )
        version_empty_build_docs <- .test_label_version_get(
          "docs", empty = FALSE
        )
        version_empty_build_docs_empty <- .test_label_version_get(
          "docs", empty = TRUE
        )
        expect_false(version_empty_build_raw_data %in% remote_vec_final)
        expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)
        expect_false(version_empty_build_output %in% remote_vec_final)
        expect_false(version_empty_build_output_empty %in% remote_vec_final)
        expect_false(version_empty_build_docs %in% remote_vec_final)
        expect_false(version_empty_build_docs_empty %in% remote_vec_final)

        # Clear up
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
    }
  )
})

test_that("upload to archive remotes using parameter for only output content", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      # remove all remotes
      .yml_dest_rm_type_all("default")
      tag_name <- "archive"

      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", tag_name)) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }

      # no remotes exist yet
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_no_build_label <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_no_build_label %in% remote_vec_final)
      expect_false(version_no_build_label_empty %in% remote_vec_final)

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to GitHub
      projr::projr_build_patch(msg = "test", archive_github = "output")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_false(version_empty_build_output %in% remote_vec_final)
      expect_true(version_empty_build_output_empty %in% remote_vec_final)
      expect_false(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)

      # --- Upload nothing new, cue always -----
      projr::projr_build_patch(msg = "test", archive_github = "output")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_false(version_empty_build_output %in% remote_vec_final)
      expect_true(version_empty_build_output_empty %in% remote_vec_final)
      expect_false(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)

      # --- Upload nothing new, cue if-change -----
      projr::projr_build_patch(
        msg = "test", archive_github = "output", always_archive = FALSE
      )

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_false(version_empty_build_output %in% remote_vec_final)
      expect_false(version_empty_build_output_empty %in% remote_vec_final)
      expect_false(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)


      # --- Upload new raw data, cue if-change -----
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
        msg = "test", archive_github = "output", always_archive = FALSE
      )

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output", empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output", empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs", empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)
      expect_true(version_empty_build_output %in% remote_vec_final)
      expect_false(version_empty_build_output_empty %in% remote_vec_final)
      expect_false(version_empty_build_docs %in% remote_vec_final)
      expect_false(version_empty_build_docs_empty %in% remote_vec_final)

      # Clear up
      remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
      for (fn in remote_final_vec) {
        .remote_final_empty(
          "github",
          remote = c("tag" = tag_name, "fn" = fn)
        )
      }
    }
  )
})

# =============================================================================
# Test send_cue parameter: always, if-change
# =============================================================================

test_that("test always vs if-change for projr config uploads", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {

      # remove all remotes
      .yml_dest_rm_type_all("default")
      tag_name <- "archive"
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", tag_name)) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- entirely empty raw data ----
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- Upload empty directory ----

      # Build to upload to GitHub
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_true(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)

      # --- Upload nothing new, cue if-change -----
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_false(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)

      # --- Upload nothing new, cue always -----
      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_cue = "always"
      )
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_raw_data <- .test_label_version_get(
        "raw-data", empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data", empty = TRUE
      )
      expect_true(version_empty_build_raw_data %in% remote_vec_final)
      expect_false(version_empty_build_raw_data_empty %in% remote_vec_final)

      # Clear up
      remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
      for (fn in remote_final_vec) {
        .remote_final_empty(
          "github",
          remote = c("tag" = tag_name, "fn" = fn)
        )
      }
    }
  )
})
