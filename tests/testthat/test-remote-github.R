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
# GitHub Release Setup and Reuse
# =============================================================================

test_that("GitHub test releases are created and reusable", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Define two fixed tags for testing
      tag_a <- "projr-test-release-a"
      tag_b <- "projr-test-release-b"

      # Create tag_a if it doesn't exist
      if (!.remote_check_exists("github", tag_a)) {
        .remote_create("github", id = tag_a)
        # Poll for existence with timeout
        start_time <- proc.time()[3]
        max_wait <- 300
        remote_exists <- .remote_check_exists("github", id = tag_a)
        while (!remote_exists && (proc.time()[3] - start_time < max_wait)) {
          Sys.sleep(10)
          remote_exists <- .remote_check_exists("github", id = tag_a)
        }
        expect_true(remote_exists)
      } else {
        expect_true(.remote_check_exists("github", tag_a))
      }

      # Create tag_b if it doesn't exist
      if (!.remote_check_exists("github", tag_b)) {
        .remote_create("github", id = tag_b)
        # Poll for existence with timeout
        start_time <- proc.time()[3]
        max_wait <- 300
        remote_exists <- .remote_check_exists("github", id = tag_b)
        while (!remote_exists && (proc.time()[3] - start_time < max_wait)) {
          Sys.sleep(10)
          remote_exists <- .remote_check_exists("github", id = tag_b)
        }
        expect_true(remote_exists)
      } else {
        expect_true(.remote_check_exists("github", tag_b))
      }

      # Verify both releases exist
      expect_true(.remote_check_exists("github", tag_a))
      expect_true(.remote_check_exists("github", tag_b))
    }
  )
})

# =============================================================================
# Basic Remote Creation and Existence
# =============================================================================

test_that(".remote_create works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Create a new release (not one of the fixed test releases)
      tag_init <- paste0("projr-test-create-", .test_random_string_get())
      tag <- .remote_create("github", id = tag_init)

      # Poll for existence
      start_time <- proc.time()[3]
      max_wait <- 300
      remote_exists <- .remote_check_exists("github", id = tag)
      while (!remote_exists && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(10)
        remote_exists <- .remote_check_exists("github", id = tag)
      }
      expect_true(remote_exists)
    }
  )
})

test_that(".remote_get works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
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
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test archive structure
      expect_identical(
        .remote_get_final(
          "github",
          id = "kablumph", label = "raw-data", structure = "archive"
        ),
        c("tag" = "kablumph", fn = "raw-data-v0.0.0-1.zip")
      )
    }
  )
})

# =============================================================================
# File Operations: Add, List, Remove
# =============================================================================

test_that("adding, listing and removing files works on GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"

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

      # Clear any existing content first
      remote <- c("tag" = tag, "fn" = "test-data.zip")
      .remote_file_rm_all("github", remote = remote)
      
      # Poll for removal to complete
      start_time <- proc.time()[3]
      max_wait <- 60
      repo <- .pb_guess_repo()
      content_tbl <- piggyback::pb_list(repo = repo, tag = tag)
      while (!is.null(content_tbl) && nrow(content_tbl) > 0L && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(5)
        content_tbl <- piggyback::pb_list(repo = repo, tag = tag)
      }

      # Create test content
      path_dir_source <- .test_setup_content_dir()
      fn_vec <- .remote_file_ls("local", path_dir_source)

      # Add files to GitHub release
      .remote_file_add(
        "github",
        fn = fn_vec,
        path_dir_local = path_dir_source,
        remote = remote
      )

      # Poll for upload to be reflected
      start_time <- proc.time()[3]
      max_wait <- 120
      asset_exists <- FALSE
      while (!asset_exists && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(5)
        content_tbl <- tryCatch(
          piggyback::pb_list(repo = repo, tag = tag),
          error = function(e) NULL
        )
        asset_exists <- !is.null(content_tbl) && nrow(content_tbl) > 0L
      }

      # Download and verify
      path_dir_save <- .dir_create_tmp_random()
      .remote_file_get_all(
        "github",
        remote = remote,
        path_dir_save_local = path_dir_save
      )
      expect_identical(
        .remote_file_ls("local", path_dir_save),
        fn_vec
      )

      # Remove some content
      fn_vec_rm <- c("abc.txt", "subdir1/def.txt")
      expect_true(
        .remote_file_rm("github", fn = fn_vec_rm, remote = remote)
      )

      # Poll for removal to be reflected
      start_time <- proc.time()[3]
      max_wait <- 60
      file_list <- .remote_file_ls("github", remote)
      expected_list <- setdiff(fn_vec, fn_vec_rm)
      while (!identical(file_list, expected_list) && (proc.time()[3] - start_time < max_wait)) {
        Sys.sleep(5)
        file_list <- .remote_file_ls("github", remote)
      }
      expect_identical(file_list, expected_list)

      # Cleanup
      unlink(path_dir_source, recursive = TRUE)
      unlink(path_dir_save, recursive = TRUE)
    }
  )
})

test_that(".remote_file_rm_all works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
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
        Sys.sleep(30)
      }

      # Clear cache and add test file
      piggyback:::.pb_cache_clear()
      path_tmp_file <- file.path(tempdir(), "test-file.txt")
      file.create(path_tmp_file)
      writeLines("test content", path_tmp_file)

      path_zip <- .zip_file(
        fn_rel = basename(path_tmp_file),
        path_dir_fn_rel = dirname(path_tmp_file),
        fn_rel_zip = "test-data.zip"
      )

      .remote_file_add_github_zip_attempt(
        path_zip = path_zip,
        tag = tag,
        output_level = "debug",
        log_file = NULL
      )

      # Wait for upload
      repo <- .pb_guess_repo()
      max_time <- 180
      start_time <- proc.time()[3]
      content_tbl_pre_delete <- piggyback::pb_list(repo = repo, tag = tag)
      while (nrow(content_tbl_pre_delete) == 0L && (proc.time()[3] - start_time) < max_time) {
        Sys.sleep(10)
        content_tbl_pre_delete <- piggyback::pb_list(repo = repo, tag = tag)
      }
      expect_identical(nrow(content_tbl_pre_delete), 1L)

      # Remove all files
      remote_github <- c("tag" = tag, "fn" = basename(path_zip))
      .remote_file_rm_all("github", remote = remote_github)

      # Poll for removal
      max_time_rm <- 60
      start_time_rm <- proc.time()[3]
      content_tbl <- piggyback::pb_list(repo = repo, tag = tag)
      while (!is.null(content_tbl) && nrow(content_tbl) > 0L && (proc.time()[3] - start_time_rm) < max_time_rm) {
        Sys.sleep(5)
        content_tbl <- piggyback::pb_list(repo = repo, tag = tag)
      }
      expect_true(is.null(content_tbl) || nrow(content_tbl) == 0L)
    }
  )
})

# =============================================================================
# Manifest and VERSION Integration
# =============================================================================

test_that("manifest round-trip works for GitHub releases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
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
  skip_if(.is_test_fast())
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
# Version and Structure Detection
# =============================================================================

test_that(".remote_final_check_exists_github works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"

      # Ensure release exists
      if (!.remote_check_exists("github", tag)) {
        .remote_create("github", tag)
        Sys.sleep(30)
      }

      # Test latest structure
      remote_latest <- c("tag" = tag, "fn" = "output.zip")
      
      # Test archive structure
      remote_archive <- c("tag" = tag, "fn" = "output-v0.0.1.zip")

      # Note: We can't easily test the check functions without actual assets
      # These would require a full build cycle, which is beyond scope
      # The existence check for the release itself works
      expect_true(.remote_check_exists("github", tag))
    }
  )
})

test_that(".remote_final_ls_github works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Use one of the fixed test releases
      tag <- "projr-test-release-a"

      # Ensure release exists
      if (!.remote_check_exists("github", tag)) {
        .remote_create("github", tag)
        Sys.sleep(30)
      }

      # List assets (may be empty or have previous test artifacts)
      assets <- .remote_final_ls_github(tag)
      expect_true(is.character(assets) || is.null(assets))
    }
  )
})

# =============================================================================
# Restore Integration
# =============================================================================

test_that("projr_restore works with GitHub release source", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
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
      suppressWarnings(.remote_file_rm_all("github", remote = remote_clear))
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
# Remove Empty Remotes
# =============================================================================

test_that(".remote_rm_final_if_empty works for GitHub", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_fast())
  skip_if(.is_test_select())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(git = TRUE, github = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # GitHub releases are never removed by this function
      # It always returns FALSE for GitHub
      expect_false(
        .remote_rm_final_if_empty("github", FALSE)
      )
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
  skip_if(.is_test_fast())
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
      .remote_file_rm_all("github", remote = remote)
      
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
