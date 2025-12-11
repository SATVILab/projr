# Dedicated GitHub Remote Tests
#
# This file is the ONLY testthat file that:
# - Creates or deletes GitHub releases via projr's wrappers
# - Calls GitHub-specific remote helpers (.remote_file_add_github(), etc.)
# - Tests GitHub release functionality
#
# All tests skip in CRAN, LITE, and FAST modes, and require GitHub credentials.
#
# Test strategy:
# - Create two fixed GitHub releases for testing (projr-test-release-a, projr-test-release-b)
# - Reuse these releases across tests to avoid repeated creation/wait cycles
# - Tests are idempotent when run sequentially - releases may already exist from previous runs
# - Some tests clear/modify release content; run tests sequentially to avoid interference

# =============================================================================
# Creation and existence
# ======================`=======================================================

# Only setup GitHub test environment if all conditions are met
setup_github <- tryCatch(
  {
    !.is_test_cran() &&
      !.is_test_lite() &&
      .has_internet() &&
      nzchar(.auth_get_github_pat_find())
  },
  error = function(e) FALSE
)

dir_test <- .test_setup_project(
    git = TRUE, github = setup_github, set_env_var = TRUE
  )

test_that("GitHub test releases are created and reusable", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()
  usethis::with_project(
    path = dir_test,
    code = {
      # Define two fixed tags for testing
      tag_a <- "projr-test-release-a"
      tag_b <- "projr-test-release-b"
      tag_archive <- "archive"

      # create both, neither will exist at this stage
      .remote_create("github", id = tag_a)
      .remote_create("github", id = tag_b)
      .remote_create(
        "github",
        id = tag_archive, max_attempts = 15, ensure_exists = TRUE
      )
      remote_exists_a <- .remote_check_exists("github", tag_a)
      if (!remote_exists_a) {
        # don't have to wait as long for a because we already
        # would have waited for archive
        .remote_create("github", id = tag_a, max_attempts = 5)
      }
      remote_exists_a <- if (!remote_exists_a) {
        .remote_check_exists("github", tag_a)
      } else {
        TRUE
      }
      remote_exists_b <- .remote_check_exists("github", tag_b)
      if (!remote_exists_b) {
        # don't have to wait as long for a because we already
        # would have waited for b
        .remote_create("github", id = tag_b, max_attempts = 5)
      }
      remote_exists_b <- if (!remote_exists_b) {
        .remote_check_exists("github", tag_b)
      } else {
        TRUE
      }
      remote_exists_archive <- .remote_check_exists("github", tag_archive)
      # Verify both releases exist
      expect_true(.remote_check_exists("github", tag_a))
      expect_true(.remote_check_exists("github", tag_b))
      expect_true(.remote_check_exists("github", tag_archive))
    }
  )
})

test_that(".remote_get works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  expect_identical(
    .remote_get("github", "abc"),
    c("tag" = "abc")
  )
})

test_that(".remote_get_final works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(is.null(dir_test), "No GitHub test environment available")

  usethis::with_project(
    path = dir_test,
    code = {
      # Test archive structure
      expect_identical(
        .remote_final_get(
          "github",
          id = "kablumph", label = "raw-data", structure = "archive"
        ),
        c("tag" = "kablumph", fn = "raw-data-v0.0.0-1.zip")
      )
      # Test archive structure
      expect_identical(
        .remote_final_get(
          "github",
          id = "kablumph", label = "raw-data", structure = "archive",
          empty = TRUE
        ),
        c("tag" = "kablumph", fn = "raw-data-v0.0.0-1-empty.zip")
      )
    }
  )
})

# =============================================================================
# File operations: add, list and remove
# =============================================================================

test_that("adding, listing and removing files works on GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"

      writeLines("test content", "abc.txt")

      # upload a single file
      .remote_file_add(
        "github",
        remote = c("tag" = tag, "fn" = "abc.zip"),
        path_dir_local = ".",
        fn = "abc.txt"
      )

      expect_true(
        "abc.txt" %in%
          .remote_file_ls("github", remote = c("tag" = tag, "fn" = "abc.zip"))
      )

      # Clear existing content
      .remote_final_empty("github", remote = c("tag" = tag, "fn" = "abc.zip"))

      # confirm asset does not exist
      expect_false(
        .remote_final_check_exists_direct(
          "github", c("tag" = tag, fn = "abc.zip")
        )
      )

      # add multiple files
      dir.create("subdir1/subdir2", showWarnings = FALSE, recursive = TRUE)
      writeLines("file def", "subdir1/def.txt")
      writeLines("file ghi", "subdir1/subdir2/ghi.txt")
      fn_vec <- c("abc.txt", "subdir1/def.txt", "subdir1/subdir2/ghi.txt")
      .remote_file_add(
        "github",
        fn = fn_vec,
        path_dir_local = ".",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      file_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% file_vec)

      # add a file
      writeLines("new file content", "newfile.txt")
      .remote_file_add(
        "github",
        fn = "newfile.txt",
        path_dir_local = ".",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      file_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% file_vec)
      expect_true("newfile.txt" %in% file_vec)

      # remove a file
      .remote_file_rm(
        "github",
        fn = c("newfile.txt", "subdir1/subdir2/ghi.txt"),
        remote = c("tag" = tag, "fn" = "abc.zip")
      )
      Sys.sleep(2) # wait for GitHub to process the deletion

      file_vec <- .remote_file_ls(
        "github",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      expect_false("newfile.txt" %in% file_vec)
      expect_false("subdir1/subdir2/ghi.txt" %in% file_vec)
      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)

      # download a single file
      path_dir_save <- .dir_create_tmp_random()
      .remote_file_get(
        "github",
        fn = "abc.txt",
        remote = c("tag" = tag, "fn" = "abc.zip"),
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
        "github",
        fn = "subdir1/def.txt",
        remote = c("tag" = tag, "fn" = "abc.zip"),
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
        "github",
        remote = c("tag" = tag, "fn" = "abc.zip")
      )
      expect_false(
        .remote_final_check_exists_direct(
          "github", c("tag" = tag, "fn" = "abc.zip")
        )
      )
      expect_true(
        length(.remote_ls_final("github", remote_pre = c("tag" = tag))) == 0L
      )

      expect_false(
        .remote_final_rm_if_empty(
          "github",
          remote = c("tag" = tag, "fn" = "abc.zip")
        )
      )

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

test_that("manifest round-trip works for GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"
      remote_pre <- c("tag" = tag)

      # Create a test manifest
      manifest <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("abc123", "def456"),
        stringsAsFactors = FALSE
      )

      # Write manifest to GitHub release
      .remote_write_manifest("github", remote_pre, manifest)
      Sys.sleep(2)
      expect_true(
        "manifest.csv" %in% .remote_file_ls(
          "github",
          remote = c("tag" = tag, "fn" = "manifest.zip")
        )
      )

      # Read manifest back
      manifest_retrieved <- .remote_get_manifest("github", remote_pre)

      # Verify round-trip
      expect_identical(nrow(manifest_retrieved), 2L)
      expect_true("file1.txt" %in% manifest_retrieved$fn)
      expect_true("file2.txt" %in% manifest_retrieved$fn)
      .remote_final_empty(
        "github",
        remote = c("tag" = tag, "fn" = "manifest.zip")
      )
    }
  )
})

test_that("VERSION file round-trip works for GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-b"
      remote_pre <- c("tag" = tag)

      # Write version to GitHub release
      version_to_write <- "v1.2.3"
      .remote_write_version_file(
        "github", remote_pre, version_to_write
      )
      Sys.sleep(2)
      expect_true(
        "VERSION" %in% .remote_file_ls(
          "github",
          remote = c("tag" = tag, "fn" = "VERSION.zip")
        )
      )

      # Read version back
      version_retrieved <- .remote_get_version_file("github", remote_pre)

      # Verify round-trip
      expect_identical(version_retrieved, version_to_write)

      .remote_final_empty(
        "github",
        remote = c("tag" = tag, "fn" = "VERSION.zip")
      )
    }
  )
})

# =============================================================================
# Set up
# ============================================================================

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
      if (.remote_check_exists("github", c("tag" = tag_name))) {
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
        "raw-data",
        empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      expect_false(version_no_build_label %in% remote_vec_final)
      expect_false(version_no_build_label_empty %in% remote_vec_final)

      # no files restored
      expect_identical(.file_ls(projr_path_get("raw-data")), character(0L))

      # --- Upload empty directory ----

      # Build to upload to GitHub
      projr::projr_build_patch(msg = "test")

      # Verify upload
      expect_true(
        .remote_check_exists("github", tag_name, max_attempts = 4)
      )
      remote_vec_final <- .remote_ls_final("github", c("tag" = tag_name))
      version_empty_build_label <- .test_label_version_get(
        "raw-data",
        empty = FALSE
      )
      version_empty_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_files_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_same_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
      if (.remote_check_exists("github", c("tag" = tag_name))) {
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
        "raw-data",
        empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
      if (.remote_check_exists("github", c("tag" = tag_name))) {
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
        "raw-data",
        empty = FALSE
      )
      version_no_build_label_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
      )
      version_empty_build_output <- .test_label_version_get(
        "output",
        empty = FALSE
      )
      version_empty_build_output_empty <- .test_label_version_get(
        "output",
        empty = TRUE
      )
      version_empty_build_docs <- .test_label_version_get(
        "docs",
        empty = FALSE
      )
      version_empty_build_docs_empty <- .test_label_version_get(
        "docs",
        empty = TRUE
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
      if (.remote_check_exists("github", c("tag" = tag_name))) {
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
        "raw-data",
        empty = FALSE
      )
      version_empty_build_raw_data_empty <- .test_label_version_get(
        "raw-data",
        empty = TRUE
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
      tag_name <- "projr-test-release-a"

      # remove all remotes
      .yml_dest_rm_type_all("default")


      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", c("tag" = tag_name))) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- clear _raw_data and add new content ---
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- sync-purge ---

      # set up sync-purge
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_local = path_dir_tmp_save,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_save_local = path_dir_tmp_check_2
      )
      fn_vec <- .file_ls(path_dir_tmp_check_2)
      expect_true(!"tempfile.txt" %in% fn_vec)
      expect_identical(content_vec_test_file, fn_vec)

      # --- upload-all ---

      # convert to upload all
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
# Test send_inspect parameter: file, none
# =============================================================================

test_that("various inspection methods run", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      tag_name <- "projr-test-release-a"

      # remove all remotes
      .yml_dest_rm_type_all("default")

      # --- ensure remote is cleared ---
      if (.remote_check_exists("github", c("tag" = tag_name))) {
        remote_final_vec <- .remote_ls_final("github", c("tag" = tag_name))
        for (fn in remote_final_vec) {
          .remote_final_empty(
            "github",
            remote = c("tag" = tag_name, "fn" = fn)
          )
        }
      }

      # --- clear _raw_data and add new content ---
      if (dir.exists(projr_path_get("raw-data"))) {
        unlink(projr_path_get("raw-data"), recursive = TRUE)
      }
      content_vec_test_file <- .test_content_setup_label("raw-data") |>
        .file_ls()

      # --- initial build ---

      # set up sync-diff with manifest inspection
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_local = path_dir_tmp_save,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_save_local = path_dir_tmp_check_2
      )
      fn_vec <- .file_ls(path_dir_tmp_check_2)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)


      # --- file ---

      # set up sync-diff with file inspection
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_local = path_dir_tmp_save_4,
        fn = "tempfile.txt"
      )
      path_dir_tmp_check <- .dir_create_tmp_random()
      on.exit(
        unlink(path_dir_tmp_check, recursive = TRUE),
        add = TRUE
      )
      .remote_file_get_all(
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
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
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
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
        "github",
        remote = c("tag" = tag_name, "fn" = "raw-data.zip"),
        path_dir_save_local = path_dir_tmp_check_5
      )
      fn_vec <- .file_ls(path_dir_tmp_check_5)
      expect_true("tempfile.txt" %in% fn_vec)
      expect_true(length(fn_vec) > 1)
      expect_true("no-inspect.txt" %in% fn_vec)

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
# GitHub API helper tests
# =============================================================================

# done
test_that("GITHUB_TOKEN is final fallback, GH_TOKEN takes precedence", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
    if (nzchar(old_github_token)) {
      Sys.setenv(GITHUB_TOKEN = old_github_token)
    } else {
      Sys.unsetenv("GITHUB_TOKEN")
    }
    if (nzchar(old_gh_token)) {
      Sys.setenv(GH_TOKEN = old_gh_token)
    } else {
      Sys.unsetenv("GH_TOKEN")
    }
  })

  # Test GH_TOKEN takes precedence over GITHUB_TOKEN
  Sys.unsetenv("GITHUB_PAT")
  Sys.setenv(GITHUB_TOKEN = "github_token_value")
  Sys.setenv(GH_TOKEN = "gh_token_value")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "gh_token_value")

  # Test GITHUB_TOKEN is used when GH_TOKEN not set
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GH_TOKEN")
  Sys.setenv(GITHUB_TOKEN = "github_token_value")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "github_token_value")
})

# done
test_that(".github_api_base resolves URLs correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Save original
  old_api_url <- Sys.getenv("GITHUB_API_URL", unset = "")
  on.exit({
    if (nzchar(old_api_url)) {
      Sys.setenv(GITHUB_API_URL = old_api_url)
    } else {
      Sys.unsetenv("GITHUB_API_URL")
    }
  })

  # Test default (no env var, no arg)
  Sys.unsetenv("GITHUB_API_URL")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.github.com")

  # Test env var
  Sys.setenv(GITHUB_API_URL = "https://api.example.com")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test trailing slash removal
  Sys.setenv(GITHUB_API_URL = "https://api.example.com/")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test multiple trailing slashes
  Sys.setenv(GITHUB_API_URL = "https://api.example.com///")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test explicit arg wins over env var
  Sys.setenv(GITHUB_API_URL = "https://api.example.com")
  result <- .github_api_base("https://custom.api.com")
  expect_identical(result, "https://custom.api.com")
})

# done
test_that(".gh_release_exists returns correct values for known repos", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(!requireNamespace("httr", quietly = TRUE))
  skip_if(is.null(dir_test), "No GitHub test environment available")

  usethis::with_project(
    path = dir_test,
    code = {
      # Test with a well-known public repo that has releases
      # Using a stable public repo as reference
      # Note: This test may fail if the repo changes or API is down

      # We'll use SATVILab/projr as test repo if we can access it
      repo <- "SATVILab/projr"

      # Try to check for a likely non-existent tag
      result <- tryCatch(
        .gh_release_exists(repo, "definitely-not-a-real-tag-xyz-12345"),
        error = function(e) {
          # If we get an error other than 404, skip the test
          if (!grepl("404", e$message, ignore.case = TRUE)) {
            skip("Cannot access GitHub API for testing")
          }
          FALSE
        }
      )
      # Should return FALSE for non-existent tag
      expect_false(result)
    }
  )
})

# done
test_that(".gh_repo_from_remote_url handles common remote formats", {
  skip_if(.is_test_cran())
  expect_equal(.gh_repo_from_remote_url("https://github.com/owner/repo.git"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("git@github.com:owner/repo.git"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("https://www.github.com/owner/repo"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("https://github.enterprise.com/owner/repo"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("ssh://git@github.com/owner/repo.git"), "owner/repo")
})
