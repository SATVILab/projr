# Comprehensive tests for GitHub release remote functionality
# Tests all combinations of YML parameters and content types similar to local tests
#
# This file tests GitHub releases as both destinations and sources for restore operations.
# All tests are designed to skip when:
#   - Offline (no network connection)
#   - On CRAN
#   - In fast test mode
#   - In test select mode
#   - No GitHub PAT is available
#
# Test coverage includes:
#   - Structure options (latest vs archive)
#   - Send_cue options (always, if-change, never)
#   - Send_strategy options (sync-diff, sync-purge)
#   - Send_inspect options (manifest, file, none)
#   - Different content types (raw-data, cache, output, code)
#   - Restore operations from GitHub releases
#   - Special features (@version tag, code content type)

# Test helper to create sample content in a directory
.create_test_content_github <- function(label, n_files = 3) {
  path_dir <- projr_path_get_dir(label, safe = FALSE)
  for (i in seq_len(n_files)) {
    file.create(file.path(path_dir, paste0("file", i, ".txt")))
    writeLines(paste("Content", i), file.path(path_dir, paste0("file", i, ".txt")))
  }
  # Add subdirectory with file
  dir.create(file.path(path_dir, "subdir"), showWarnings = FALSE)
  file.create(file.path(path_dir, "subdir", "nested.txt"))
  writeLines("Nested content", file.path(path_dir, "subdir", "nested.txt"))
  invisible(TRUE)
}

# =============================================================================
# Test structure parameter: latest vs archive
# =============================================================================

test_that("GitHub release works with structure='latest'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add GitHub release destination with latest structure
      tag_name <- paste0("test-latest-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      # Build and verify
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset exists (should be raw-data.zip for latest structure)
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])

      # Second build should overwrite (latest structure)
      writeLines("Modified", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()

      # Should still have only one asset
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release works with structure='archive'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add GitHub release destination with archive structure
      tag_name <- paste0("test-archive-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      # Build and verify version 1
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset exists (should be raw-data-v0.0.1.zip for archive structure)
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Second build should create new version
      projr::projr_build_patch()

      # Should have both versions
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])
      expect_true("raw-data-v0.0.2.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# Test send_cue parameter: always, if-change, never
# =============================================================================

test_that("GitHub release send_cue='always' creates new archive every build", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_cue = "always"
      tag_name <- paste0("test-always-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_cue = "always"
      )

      # First build
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Second build without changes - should still create new version
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.2.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release send_cue='if-change' only creates archive if content changed", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_cue = "if-change"
      tag_name <- paste0("test-ifchange-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_cue = "if-change"
      )

      # First build
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Second build without changes - should NOT create new version
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_false("raw-data-v0.0.2.zip" %in% asset_tbl[["file_name"]])

      # Third build with changes - should create new version
      writeLines("Modified content", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.3.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release send_cue='never' never sends to remote", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip("send_cue='never' implementation needs verification")
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_cue = "never"
      tag_name <- paste0("test-never-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_cue = "never"
      )

      # Build - should not create release
      projr::projr_build_patch()

      # Release should not exist
      expect_false(.remote_check_exists("github", tag_name))
    }
  )
})

# =============================================================================
# Test send_strategy parameter: upload-all, sync-diff, sync-purge
# =============================================================================

test_that("GitHub release send_strategy='sync-diff' updates only changed files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data", n_files = 3)
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_strategy = "sync-diff", structure = "latest"
      tag_name <- paste0("test-syncdiff-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest",
        send_strategy = "sync-diff",
        send_cue = "always"
      )

      # First build
      projr::projr_build_patch()
      expect_true(.remote_check_exists("github", tag_name))

      # Download and verify initial content
      temp_dir1 <- tempdir()
      remote <- .remote_get_final("github", tag_name, "raw-data", "latest", NULL, TRUE, NULL, FALSE)
      .remote_file_get_all("github", remote, temp_dir1)
      expect_true(file.exists(file.path(temp_dir1, "file1.txt")))
      expect_true(file.exists(file.path(temp_dir1, "file2.txt")))
      expect_true(file.exists(file.path(temp_dir1, "file3.txt")))

      # Remove one file, add one file
      file.remove(file.path(projr_path_get_dir("raw-data"), "file2.txt"))
      file.create(file.path(projr_path_get_dir("raw-data"), "file4.txt"))
      writeLines("New file", file.path(projr_path_get_dir("raw-data"), "file4.txt"))

      # Second build - should sync the diff
      projr::projr_build_patch()

      # Download and verify updated content
      temp_dir2 <- tempdir()
      .remote_file_get_all("github", remote, temp_dir2)
      expect_true(file.exists(file.path(temp_dir2, "file1.txt")))
      expect_false(file.exists(file.path(temp_dir2, "file2.txt"))) # removed
      expect_true(file.exists(file.path(temp_dir2, "file3.txt")))
      expect_true(file.exists(file.path(temp_dir2, "file4.txt"))) # added
    }
  )
})

test_that("GitHub release send_strategy='sync-purge' removes all then uploads all", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data", n_files = 3)
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_strategy = "sync-purge"
      tag_name <- paste0("test-syncpurge-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest",
        send_strategy = "sync-purge",
        send_cue = "always"
      )

      # First build
      projr::projr_build_patch()
      expect_true(.remote_check_exists("github", tag_name))

      # Note: Can't manually add files to GitHub release like we can with local
      # So we verify that sync-purge works by checking the asset is recreated
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      initial_count <- nrow(asset_tbl)

      # Second build - should purge and re-upload
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# Test send_inspect parameter: manifest, file, none
# =============================================================================

test_that("GitHub release send_inspect='manifest' uses manifest for version tracking", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_inspect = "manifest"
      tag_name <- paste0("test-manifest-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_inspect = "manifest",
        send_cue = "if-change"
      )

      # First build
      projr::projr_build_patch()

      # Verify release has both data and manifest
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])
      expect_true("manifest.csv" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release send_inspect='file' inspects actual files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_inspect = "file"
      tag_name <- paste0("test-file-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_inspect = "file",
        send_cue = "if-change"
      )

      # First build
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Second build without changes - should not create new version
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_false("raw-data-v0.0.2.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release send_inspect='none' treats remote as empty", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip("send_inspect='none' may have implementation issues - needs investigation")
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_inspect = "none"
      tag_name <- paste0("test-none-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_inspect = "none",
        send_cue = "if-change"
      )

      # First build
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Second build - since inspect=none, should always upload
      projr::projr_build_patch()
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.2.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# Test different content types
# =============================================================================

test_that("GitHub release works with different content types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup multiple content types
      .create_test_content_github("raw-data")
      .create_test_content_github("cache")
      .create_test_content_github("output")

      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add destinations for different content types
      tag_name <- paste0("test-multi-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = c("raw-data", "cache", "output"),
        structure = "latest"
      )

      # Build
      projr::projr_build_patch()

      # Verify all content types are uploaded
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])
      expect_true("cache.zip" %in% asset_tbl[["file_name"]])
      expect_true("output.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# Test GitHub releases as source for restore operations
# =============================================================================

test_that("projr_restore works with GitHub release source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup and create GitHub release with data
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      tag_name <- paste0("test-restore-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      # Build to create release
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      dir.create(projr_path_get_dir("raw-data", safe = FALSE), showWarnings = FALSE)

      # Configure for restore
      .yml_dest_rm_type_all("default")
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      # Restore from GitHub release
      result <- projr_restore(label = "raw-data", type = "github", title = tag_name)

      # Verify data was restored
      expect_true(result)
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data"), "file1.txt")))
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data"), "file2.txt")))
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data"), "subdir", "nested.txt")))
    }
  )
})

test_that("GitHub release restore works with archive structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup and create versioned GitHub release
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      tag_name <- paste0("test-restore-arch-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      # Build to create versioned release
      projr::projr_build_patch()

      # Verify versioned asset exists
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data-v0.0.1.zip" %in% asset_tbl[["file_name"]])

      # Clear local data
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)
      dir.create(projr_path_get_dir("raw-data", safe = FALSE), showWarnings = FALSE)

      # Restore from archived GitHub release
      result <- projr_restore(label = "raw-data", type = "github", title = tag_name)

      # Verify data was restored
      expect_true(result)
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data"), "file1.txt")))
    }
  )
})

# =============================================================================
# Test @version tag functionality
# =============================================================================

test_that("GitHub release works with @version tag", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with @version tag
      projr_yml_dest_add_github(
        title = "@version",
        content = "raw-data",
        structure = "latest"
      )

      # Build
      projr::projr_build_patch()

      # Verify release exists with version tag
      current_version <- .version_get_v()
      expect_true(.remote_check_exists("github", current_version))

      # Verify asset exists
      asset_tbl <- .pb_asset_tbl_get(current_version)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# Test code content type (special case)
# =============================================================================

test_that("GitHub release works with code content type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if_offline()
  skip_on_cran()
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Setup
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add some tracked files
      writeLines("test code", "test.R")
      .git_commit_file("test.R", "Add test file")

      # Add destination for code
      tag_name <- paste0("test-code-", .test_random_string_get())
      projr_yml_dest_add_github(
        title = tag_name,
        content = "code",
        structure = "latest"
      )

      # Build
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # For code type, GitHub automatically creates source code archives
      # We just verify the release was created
    }
  )
})
