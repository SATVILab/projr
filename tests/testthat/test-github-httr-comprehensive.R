# Comprehensive tests for GitHub httr functions
# This test file validates all httr-based GitHub API functions

test_that("GitHub httr functions work correctly - comprehensive check", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(.is_gha())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  # Setup test directory
  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Test configuration
      test_tag <- "httr-test-release"
      test_asset_name <- "test-asset.zip"
      repo <- .gh_repo_get()

      # Clean up any existing test release
      if (.remote_check_exists_github_httr(repo = repo, tag = test_tag)) {
        .remote_rm_github_httr(repo = repo, tag = test_tag)
      }

      # ================================================================
      # Test 1: .remote_check_exists_github_httr - Release does not exist
      # ================================================================
      exists_before <- .remote_check_exists_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_false(exists_before, info = "Release should not exist initially")

      # ================================================================
      # Test 2: .gh_release_create_httr - Create release
      # ================================================================
      release_obj <- .gh_release_create_httr(
        repo = repo,
        tag = test_tag,
        description = "Test release for httr function validation"
      )
      expect_true(is.list(release_obj), info = "Should return release object")
      expect_true(!is.null(release_obj$id), info = "Release should have ID")

      # ================================================================
      # Test 3: .remote_check_exists_github_httr - Release exists
      # ================================================================
      exists_after <- .remote_check_exists_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_true(exists_after, info = "Release should exist after creation")

      # ================================================================
      # Test 4: .remote_ls_final_github_httr - List assets (empty)
      # ================================================================
      assets_empty <- .remote_ls_final_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_identical(
        length(assets_empty), 0L,
        info = "New release should have no assets"
      )

      # ================================================================
      # Test 5: .remote_get_info_github_httr - Get release info
      # ================================================================
      release_info <- .remote_get_info_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_true(is.list(release_info), info = "Should return assets list")
      expect_identical(
        length(release_info), 0L,
        info = "Assets list should be empty"
      )

      # ================================================================
      # Test 6: .remote_final_check_exists_github_httr - Asset does not exist
      # ================================================================
      asset_exists_before <- .remote_final_check_exists_github_httr(
        repo = repo,
        tag = test_tag,
        asset = test_asset_name
      )
      expect_false(
        asset_exists_before,
        info = "Asset should not exist before upload"
      )

      # ================================================================
      # Test 7: .gh_release_asset_upload_httr - Upload asset
      # ================================================================
      # Create a test file to upload
      test_file <- tempfile(fileext = ".txt")
      writeLines("Test content for httr validation", test_file)

      # Create a zip file
      test_zip <- tempfile(fileext = ".zip")
      utils::zip(test_zip, test_file, flags = "-j -q")

      upload_result <- .gh_release_asset_upload_httr(
        repo = repo,
        tag = test_tag,
        file_path = test_zip,
        asset_name = test_asset_name,
        overwrite = TRUE
      )
      expect_true(is.list(upload_result), info = "Should return asset object")
      expect_identical(
        upload_result$name, test_asset_name,
        info = "Asset name should match"
      )

      # ================================================================
      # Test 8: .remote_ls_final_github_httr - List assets (with asset)
      # ================================================================
      assets_with_file <- .remote_ls_final_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_true(
        test_asset_name %in% assets_with_file,
        info = "Asset should be listed after upload"
      )

      # ================================================================
      # Test 9: .remote_final_check_exists_github_httr - Asset exists
      # ================================================================
      asset_exists_after <- .remote_final_check_exists_github_httr(
        repo = repo,
        tag = test_tag,
        asset = test_asset_name
      )
      expect_true(
        asset_exists_after,
        info = "Asset should exist after upload"
      )

      # ================================================================
      # Test 10: .remote_final_get_info_github_httr - Get asset info
      # ================================================================
      asset_info <- .remote_final_get_info_github_httr(
        repo = repo,
        tag = test_tag,
        asset_name = test_asset_name
      )
      expect_true(is.list(asset_info), info = "Should return asset info")
      expect_identical(
        asset_info$name, test_asset_name,
        info = "Asset name should match"
      )
      expect_true(
        !is.null(asset_info$id),
        info = "Asset should have ID"
      )

      # ================================================================
      # Test 11: .remote_file_get_all_github_httr - Download asset
      # ================================================================
      download_dir <- tempfile()
      dir.create(download_dir)

      downloaded <- .remote_file_get_all_github_httr(
        repo = repo,
        tag = test_tag,
        fn = test_asset_name,
        dest_dir = download_dir,
        overwrite = TRUE
      )
      expect_true(
        length(downloaded) > 0,
        info = "Should download at least one file"
      )
      expect_true(
        file.exists(downloaded[1]),
        info = "Downloaded file should exist"
      )

      # ================================================================
      # Test 12: .remote_final_rm_github_httr - Delete asset
      # ================================================================
      delete_result <- .remote_final_rm_github_httr(
        repo = repo,
        tag = test_tag,
        fn = test_asset_name
      )
      expect_true(delete_result, info = "Asset deletion should succeed")

      # Verify asset is deleted
      asset_exists_after_delete <- .remote_final_check_exists_github_httr(
        repo = repo,
        tag = test_tag,
        asset = test_asset_name
      )
      expect_false(
        asset_exists_after_delete,
        info = "Asset should not exist after deletion"
      )

      # ================================================================
      # Test 13: .remote_rm_github_httr - Delete release
      # ================================================================
      delete_release_result <- .remote_rm_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_true(delete_release_result, info = "Release deletion should succeed")

      # Verify release is deleted
      exists_after_delete <- .remote_check_exists_github_httr(
        repo = repo,
        tag = test_tag
      )
      expect_false(
        exists_after_delete,
        info = "Release should not exist after deletion"
      )

      # Cleanup
      unlink(test_file)
      unlink(test_zip)
      unlink(download_dir, recursive = TRUE)
    }
  )
})

test_that(".github_api_base handles different inputs correctly", {
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

  # Test default
  Sys.unsetenv("GITHUB_API_URL")
  expect_identical(
    .github_api_base(NULL),
    "https://api.github.com"
  )

  # Test environment variable
  Sys.setenv(GITHUB_API_URL = "https://api.enterprise.com")
  expect_identical(
    .github_api_base(NULL),
    "https://api.enterprise.com"
  )

  # Test trailing slash removal
  Sys.setenv(GITHUB_API_URL = "https://api.example.com/")
  expect_identical(
    .github_api_base(NULL),
    "https://api.example.com"
  )

  # Test multiple trailing slashes
  Sys.setenv(GITHUB_API_URL = "https://api.example.com///")
  expect_identical(
    .github_api_base(NULL),
    "https://api.example.com"
  )

  # Test explicit argument overrides environment
  Sys.setenv(GITHUB_API_URL = "https://env.api.com")
  expect_identical(
    .github_api_base("https://arg.api.com"),
    "https://arg.api.com"
  )
})

test_that(".gh_repo_from_remote_url parses various URL formats", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # HTTPS with .git
  expect_identical(
    .gh_repo_from_remote_url("https://github.com/owner/repo.git"),
    "owner/repo"
  )

  # HTTPS without .git
  expect_identical(
    .gh_repo_from_remote_url("https://github.com/owner/repo"),
    "owner/repo"
  )

  # SSH format
  expect_identical(
    .gh_repo_from_remote_url("git@github.com:owner/repo.git"),
    "owner/repo"
  )

  # SSH without .git
  expect_identical(
    .gh_repo_from_remote_url("git@github.com:owner/repo"),
    "owner/repo"
  )

  # With www
  expect_identical(
    .gh_repo_from_remote_url("https://www.github.com/owner/repo"),
    "owner/repo"
  )

  # Enterprise GitHub
  expect_identical(
    .gh_repo_from_remote_url("https://github.enterprise.com/owner/repo"),
    "owner/repo"
  )

  # SSH URL format
  expect_identical(
    .gh_repo_from_remote_url("ssh://git@github.com/owner/repo.git"),
    "owner/repo"
  )

  # Error on invalid URL with only one part
  expect_error(
    .gh_repo_from_remote_url("invalid"),
    "parse owner/repo"
  )
})

test_that(".gh_guess_repo works in git repository", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(.is_gha())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      # Should successfully guess repo from git remote
      repo <- .gh_guess_repo()
      expect_true(is.character(repo))
      expect_true(nzchar(repo))
      expect_true(grepl("/", repo))

      # Should have format "owner/repo"
      parts <- strsplit(repo, "/")[[1]]
      expect_identical(length(parts), 2L)
    }
  )
})

test_that("GitHub httr error handling works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())
  skip_if(.is_gha())
  skip_if_offline()
  .test_skip_if_cannot_modify_github()

  dir_test <- .test_setup_project(
    git = TRUE, github = TRUE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      repo <- .gh_repo_get()

      # Test error when release doesn't exist
      expect_error(
        .remote_get_info_github_httr(
          repo = repo,
          tag = "definitely-does-not-exist-xyz-123"
        ),
        "Release not found"
      )

      # Test error when getting info for non-existent asset
      # First create a release
      test_tag <- "httr-error-test"
      if (.remote_check_exists_github_httr(repo = repo, tag = test_tag)) {
        .remote_rm_github_httr(repo = repo, tag = test_tag)
      }
      .gh_release_create_httr(
        repo = repo,
        tag = test_tag,
        description = "Test release for error handling"
      )

      # Try to get info for non-existent asset
      expect_error(
        .remote_final_get_info_github_httr(
          repo = repo,
          tag = test_tag,
          asset_name = "non-existent-asset.zip"
        )
      )

      # Cleanup
      .remote_rm_github_httr(repo = repo, tag = test_tag)
    }
  )
})

test_that("GitHub httr handles authentication correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())

  # Save original token
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
  })

  # Test with no authentication - should return error for private repos
  # or succeed for public repos
  Sys.unsetenv("GITHUB_PAT")

  # Test checking a well-known public repo release
  result <- tryCatch(
    .remote_check_exists_github_httr(
      repo = "actions/checkout",
      tag = "v4.0.0",
      token = NULL
    ),
    error = function(e) e
  )

  # Should either succeed (public access) or fail with auth error
  if (inherits(result, "error")) {
    expect_true(
      grepl("authentication", result$message, ignore.case = TRUE) ||
        grepl("401|403", result$message),
      info = "Error should be about authentication"
    )
  } else {
    # If it succeeds, it should be logical
    expect_true(is.logical(result))
  }
})
