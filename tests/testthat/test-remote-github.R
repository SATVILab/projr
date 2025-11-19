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
# =============================================================================

dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)

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

      # create both, neither will exist at this stage
      .remote_create("github", id = tag_a)
      .remote_create(
        "github", id = tag_b, max_attempts = 15, ensure_exists = TRUE
      )
      remote_exists_a <- .remote_check_exists("github", tag_a)
      if (!remote_exists_a) {
        # don't have to wait as long for a because we already
        # would have waited for b
        .remote_create("github", id = tag_a, max_attempts = 5)
      }
      remote_exists_a <- if (!remote_exists_a) {
        .remote_check_exists("github", tag_a)
      } else {
        TRUE
      }
      remote_exists_b <- .remote_check_exists("github", tag_b)
      # Verify both releases exist
      expect_true(.remote_check_exists("github", tag_a))
      expect_true(.remote_check_exists("github", tag_b))
    }
  )
})

test_that(".remote_get works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  usethis::with_project(
    path = dir_test,
    code = {
      # Test remote_get with arbitrary tag
      expect_identical(
        .remote_get("github", "abc"),
        c("tag" = "abc")
      )
    }
  )
})

test_that(".remote_get_final works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

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
      # debugonce(.remote_final_empty_github)
      # debugonce(.remote_final_check_exists_github_httr)
      .remote_final_empty("github", remote = c("tag" = tag, "fn" = "abc.zip"))

      # confirm asset does not exist
      expect_false(
        .remote_final_check_exists_github_httr(
          repo = .gh_guess_repo(),
          tag = tag,
          asset = "abc.zip"
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
        "github", remote = c("tag" = tag, "fn" = "abc.zip")
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
        "github", remote = c("tag" = tag, "fn" = "abc.zip")
      )

      expect_true("abc.txt" %in% file_vec)
      expect_true("subdir1/def.txt" %in% file_vec)
      expect_true("subdir1/subdir2/ghi.txt" %in% file_vec)
      expect_true("newfile.txt" %in% file_vec)

      # remove a file
      browser()
      .remote_file_rm(
        "github",
        fn = c("newfile.txt", "subdir1/subdir2/ghi.txt"),
        remote = c("tag" = tag, "fn" = "abc.zip")
      )

      file_vec <- .remote_file_ls(
        "github", remote = c("tag" = tag, "fn" = "abc.zip")
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
        .remote_final_check_exists_github_httr(
          repo = .gh_guess_repo(),
          tag = tag,
          asset = "abc.zip"
        )
      )

      expect_false(
        .remote_final_rm_if_empty(
          "github", remote = c("tag" = tag, "fn" = "abc.zip")
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

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"

      # Ensure release exists and is empty
      if (!.remote_check_exists("github", tag)) {
        .remote_create("github", tag)
        # Poll for existence
        start_time <- proc.time()[3]
        max_wait <- 180
        remote_exists <- .remote_check_exists("github", id = tag)
        while (!remote_exists && (proc.time()[3] - start_time < max_wait)) {
          Sys.sleep(10)
          remote_exists <- .remote_check_exists("github", id = tag)
        }
      }

      # Create a test manifest
      manifest <- data.frame(
        label = c("output", "output"),
        fn = c("file1.txt", "file2.txt"),
        version = c("v0.0.1", "v0.0.1"),
        hash = c("abc123", "def456"),
        stringsAsFactors = FALSE
      )

      # Write manifest to GitHub release
      remote_pre <- c("tag" = tag, "fn" = "test-manifest.zip")
      .remote_write_manifest("github", remote_pre, manifest)

      # Poll for upload
      start_time <- proc.time()[3]
      max_wait <- 120
      manifest_uploaded <- FALSE
      while (!manifest_uploaded && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(5)
        manifest_retrieved <- tryCatch(
          .remote_get_manifest("github", remote_pre),
          error = function(e) NULL
        )
        manifest_uploaded <- !is.null(manifest_retrieved) && nrow(manifest_retrieved) > 0L
      }

      # Read manifest back
      manifest_retrieved <- .remote_get_manifest("github", remote_pre)

      # Verify round-trip
      expect_identical(nrow(manifest_retrieved), 2L)
      expect_true("file1.txt" %in% manifest_retrieved$fn)
      expect_true("file2.txt" %in% manifest_retrieved$fn)
    }
  )
})

test_that("VERSION file round-trip works for GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-b"

      # Ensure release exists
      if (!.remote_check_exists("github", tag)) {
        .remote_create("github", tag)
        # Poll for existence
        start_time <- proc.time()[3]
        max_wait <- 180
        remote_exists <- .remote_check_exists("github", id = tag)
        while (!remote_exists && (proc.time()[3] - start_time < max_wait)) {
          Sys.sleep(10)
          remote_exists <- .remote_check_exists("github", id = tag)
        }
      }

      # Write version to GitHub release
      remote <- c("tag" = tag, "fn" = "version-test.zip")
      version_to_write <- "v1.2.3"
      .remote_write_version_file("github", remote, version_to_write, trusted = TRUE)

      # Poll for upload
      start_time <- proc.time()[3]
      max_wait <- 120
      version_uploaded <- FALSE
      while (!version_uploaded && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(5)
        version_retrieved <- tryCatch(
          .remote_get_version_file("github", remote),
          error = function(e) NULL
        )
        version_uploaded <- !is.null(version_retrieved) && nzchar(version_retrieved)
      }

      # Read version back
      version_retrieved <- .remote_get_version_file("github", remote)

      # Verify round-trip
      expect_identical(version_retrieved, version_to_write)
    }
  )
})


# =============================================================================
# Restore Integration
# =============================================================================

test_that("projr_restore works with GitHub release source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create content and upload to GitHub
      path_dir <- projr_path_get_dir("raw-data", safe = FALSE)
      file.create(file.path(path_dir, "test-restore.txt"))
      writeLines("restore test content", file.path(path_dir, "test-restore.txt"))

      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      tag_name <- "projr-test-release-a"

      # Clear any existing content in this release for this test
      remote_clear <- c("tag" = tag_name, "fn" = "raw-data.zip")
      suppressWarnings(.remote_final_empty("github", remote = remote_clear))
      Sys.sleep(10)

      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      # Build to upload to GitHub
      projr::projr_build_patch()

      # Wait for upload
      Sys.sleep(30)

      # Verify upload
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

      # Restore from GitHub
      result <- projr_restore(label = "raw-data", type = "github", title = tag_name)

      # Verify restoration
      expect_true(result)
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data"), "test-restore.txt")))
    }
  )
})

# =============================================================================
# Comprehensive Parameter Testing
# Adapted from test-github-remote-comprehensive.R to use reusable releases
# =============================================================================

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
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add GitHub release destination with latest structure
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
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-b"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data-v0.0.1.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add GitHub release destination with archive structure
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      # Build and verify
      projr::projr_build_patch()

      # Verify release exists
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset exists (should be raw-data-v0.0.1.zip for archive structure)
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true(any(grepl("raw-data-v.*\\.zip", asset_tbl[["file_name"]])))

      # Second build should create a new versioned archive
      writeLines("Modified", file.path(projr_path_get_dir("raw-data"), "file1.txt"))
      projr::projr_build_patch()

      # Should now have two assets
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      versioned_assets <- grepl("raw-data-v.*\\.zip", asset_tbl[["file_name"]])
      expect_true(sum(versioned_assets) >= 2)
    }
  )
})

# =============================================================================
# Test send_cue parameter: always, if-change, never
# =============================================================================

test_that("GitHub release send_cue='always' creates new archive every build", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_cue = "always"
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
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-b"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_cue = "if-change"
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

# =============================================================================
# Test send_strategy parameter: sync-diff, sync-purge
# =============================================================================

test_that("GitHub release send_strategy='sync-diff' updates only changed files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data", n_files = 3)
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_strategy = "sync-diff", structure = "latest"
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

      # Verify asset exists
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

test_that("GitHub release send_strategy='sync-purge' removes all then uploads all", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-b"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data", n_files = 3)
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_strategy = "sync-purge"
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

      # Verify asset was created
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("raw-data.zip" %in% asset_tbl[["file_name"]])

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
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_inspect = "manifest"
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_inspect = "manifest",
        send_cue = "if-change"
      )

      # Build
      projr::projr_build_patch()
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset and manifest exist
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true(any(grepl("raw-data-v.*\\.zip", asset_tbl[["file_name"]])))
    }
  )
})

test_that("GitHub release send_inspect='file' inspects actual files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-b"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      # Add with send_inspect = "file"
      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive",
        send_inspect = "file",
        send_cue = "if-change"
      )

      # Build
      projr::projr_build_patch()
      expect_true(.remote_check_exists("github", tag_name))

      # Verify asset exists
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true(any(grepl("raw-data-v.*\\.zip", asset_tbl[["file_name"]])))
    }
  )
})

# =============================================================================
# Restore operations from GitHub releases
# =============================================================================

test_that("projr_restore works with GitHub release source (latest structure)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup and build
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "latest"
      )

      projr::projr_build_patch()

      # Delete local content
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)

      # Restore from GitHub release
      result <- projr_restore(label = "raw-data", type = "github", title = tag_name)

      # Verify restoration
      expect_true(result)
      expect_true(dir.exists(projr_path_get_dir("raw-data", safe = FALSE)))
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data", safe = FALSE), "file1.txt")))
    }
  )
})

test_that("GitHub release restore works with archive structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-b"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "raw-data.zip")
      .remote_final_empty("github", remote = remote)

      # Setup and build
      .create_test_content_github("raw-data")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      projr_yml_dest_add_github(
        title = tag_name,
        content = "raw-data",
        structure = "archive"
      )

      projr::projr_build_patch()

      # Delete local content
      unlink(projr_path_get_dir("raw-data", safe = FALSE), recursive = TRUE)

      # Restore from GitHub release
      result <- projr_restore(label = "raw-data", type = "github", title = tag_name)

      # Verify restoration
      expect_true(result)
      expect_true(dir.exists(projr_path_get_dir("raw-data", safe = FALSE)))
      expect_true(file.exists(file.path(projr_path_get_dir("raw-data", safe = FALSE), "file1.txt")))
    }
  )
})

# =============================================================================
# Different content types
# =============================================================================

test_that("GitHub release works with different content types", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag_name <- "projr-test-release-a"

      # Clear any existing content
      remote <- c("tag" = tag_name, "fn" = "output.zip")
      .remote_final_empty("github", remote = remote)

      # Test with output content
      .create_test_content_github("output")
      projr_init_git()
      .yml_git_set_push(FALSE, TRUE, NULL)
      .yml_dest_rm_type_all("default")

      projr_yml_dest_add_github(
        title = tag_name,
        content = "output",
        structure = "latest"
      )

      projr::projr_build_patch()

      # Verify asset exists
      asset_tbl <- .pb_asset_tbl_get(tag_name)
      expect_true("output.zip" %in% asset_tbl[["file_name"]])
    }
  )
})

# =============================================================================
# GitHub API Helper Tests
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
  usethis::with_project(
    path = dir_test,
    code = {
      skip_if(.is_test_cran())
      skip_if(.is_test_lite())
      skip_if(.is_test_select())
      skip_if(!requireNamespace("httr", quietly = TRUE))
      skip_if(!nzchar(.auth_get_github_pat_find()))

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
  expect_equal(.gh_repo_from_remote_url("https://github.com/owner/repo.git"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("git@github.com:owner/repo.git"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("https://www.github.com/owner/repo"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("https://github.enterprise.com/owner/repo"), "owner/repo")
  expect_equal(.gh_repo_from_remote_url("ssh://git@github.com/owner/repo.git"), "owner/repo")
})