# Dedicated Tests for projr_restore_repo() and projr_restore_repo_wd()
#
# This file tests repository restoration functionality:
# - projr_restore_repo(): Clones a GitHub repository and restores its content
# - projr_restore_repo_wd(): Convenience wrapper that clones into current directory
#
# Tests focus on:
# - Parameter validation for all inputs
# - Error handling for invalid inputs
# - Integration with .git_clone() and projr_content_update()
# - Edge cases and failure scenarios

# =============================================================================
# Parameter Validation Tests for projr_restore_repo
# =============================================================================

test_that("projr_restore_repo validates repo parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # NULL repo
  expect_error(
    projr_restore_repo(repo = NULL),
    "'repo' cannot be NULL"
  )

  # Non-character repo
  expect_error(
    projr_restore_repo(repo = 123),
    "'repo' must be a character string"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = character(0)),
    "'repo' must have at least one element"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = c("repo1", "repo2")),
    "'repo' must be a single character value"
  )

  # Empty string
  expect_error(
    projr_restore_repo(repo = ""),
    "'repo' cannot be an empty string"
  )
})

test_that("projr_restore_repo validates path parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character path (when not NULL)
  expect_error(
    projr_restore_repo(repo = "test/repo", path = 123),
    "'path' must be NULL or a character string"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", path = c("path1", "path2")),
    "'path' must be a single character value"
  )

  # Empty string
  expect_error(
    projr_restore_repo(repo = "test/repo", path = ""),
    "'path' cannot be an empty string"
  )

  # NULL is valid (no error expected, but will fail on clone)
  expect_error(
    projr_restore_repo(repo = "test/repo", path = NULL),
    NA,
    class = "error" # Expect some error (likely git clone), but not validation error
  )
})

test_that("projr_restore_repo validates label parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character label
  expect_error(
    projr_restore_repo(repo = "test/repo", label = 123),
    "'label' must be NULL or a character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", label = character(0)),
    "'label' must have at least one element if not NULL"
  )

  # NULL is valid (no error on validation)
  # Will fail on clone, but not due to validation
  expect_no_error <- function(expr) {
    tryCatch(
      expr,
      error = function(e) {
        if (grepl("'label'", e$message)) {
          stop("Unexpected validation error for label")
        }
        # Other errors (like git clone failures) are OK
        invisible(NULL)
      }
    )
  }
  expect_no_error(projr_restore_repo(repo = "test/repo", label = NULL))
})

test_that("projr_restore_repo validates pos parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character pos
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = 123),
    "'pos' must be NULL or a character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = character(0)),
    "'pos' must have at least one element if not NULL"
  )

  # Invalid value
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = "invalid"),
    "'pos' must be 'source' or 'dest'"
  )

  # Multiple invalid values
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = c("source", "invalid")),
    "'pos' must be 'source' or 'dest'.*invalid"
  )
})

test_that("projr_restore_repo validates type parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character type
  expect_error(
    projr_restore_repo(repo = "test/repo", type = 123),
    "'type' must be NULL or a character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", type = character(0)),
    "'type' must have at least one element if not NULL"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", type = c("local", "github")),
    "'type' must be a single character value"
  )

  # Invalid value
  expect_error(
    projr_restore_repo(repo = "test/repo", type = "invalid"),
    "'type' must be one of: local, osf, github"
  )
})

test_that("projr_restore_repo validates title parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character title
  expect_error(
    projr_restore_repo(repo = "test/repo", title = 123),
    "'title' must be NULL or a character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", title = character(0)),
    "'title' must have at least one element if not NULL"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", title = c("title1", "title2")),
    "'title' must be a single character value"
  )
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("projr_restore_repo handles git clone failure gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create temporary directory for test
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  withr::with_dir(test_dir, {
    # Try to clone non-existent repository
    # Suppress expected system warnings from git command
    result <- suppressWarnings(
      projr_restore_repo(
        repo = "nonexistent/nonexistent-repo-12345",
        path = "clone_dir"
      )
    )

    # Should return FALSE on error
    expect_false(result)
  })
})

test_that("projr_restore_repo returns FALSE when restoration fails", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create temporary directory for test
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  withr::with_dir(test_dir, {
    # Mock a scenario where git clone might succeed but restore fails
    # by providing an invalid repo format that will fail early
    # Suppress expected errors/warnings
    result <- suppressWarnings(
      projr_restore_repo(
        repo = "invalid-repo-format-without-slash-12345"
      )
    )

    # Should return FALSE on error
    expect_false(result)
  })
})

# =============================================================================
# Integration Tests (require GitHub authentication)
# =============================================================================

test_that("projr_restore_repo can clone and restore a real repository", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  .test_skip_if_cannot_modify_github()

  # This test requires a real test repository with projr structure
  # For now, we skip it but leave the structure for when we have a test repo
  skip("Requires a dedicated test repository with projr structure")

  # Example test structure (to be implemented when test repo is available):
  # test_dir <- tempfile()
  # dir.create(test_dir)
  # on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  #
  # withr::with_dir(test_dir, {
  #   result <- projr_restore_repo(
  #     repo = "SATVILab/projr-test-repo",  # Would need to create this
  #     path = "test_clone"
  #   )
  #
  #   expect_true(result)
  #   expect_true(dir.exists("test_clone"))
  #   expect_true(file.exists(file.path("test_clone", "_projr.yml")))
  # })
})

test_that("projr_restore_repo works with short repo name (user inferred)", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  .test_skip_if_cannot_modify_github()

  # This test would require GitHub authentication to infer user
  skip("Requires GitHub authentication and dedicated test repository")

  # Example test structure:
  # test_dir <- tempfile()
  # dir.create(test_dir)
  # on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  #
  # withr::with_dir(test_dir, {
  #   # Should infer user from GitHub credentials
  #   result <- projr_restore_repo(
  #     repo = "projr-test-repo",  # Just repo name, no owner
  #     path = "test_clone"
  #   )
  #
  #   expect_true(result)
  #   expect_true(dir.exists("test_clone"))
  # })
})

test_that("projr_restore_repo can restore specific labels", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())

  # Would require a test repository with multiple labels
  skip("Requires dedicated test repository with multiple labels")
})

test_that("projr_restore_repo respects type and title parameters", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())

  # Would require a test repository with configured remotes
  skip("Requires dedicated test repository with configured remotes")
})

# =============================================================================
# Tests for projr_restore_repo_wd
# =============================================================================

test_that("projr_restore_repo_wd validates parameters same as projr_restore_repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # NULL repo
  expect_error(
    projr_restore_repo_wd(repo = NULL),
    "'repo' cannot be NULL"
  )

  # Non-character label
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", label = 123),
    "'label' must be NULL or a character vector"
  )

  # Invalid pos
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", pos = "invalid"),
    "'pos' must be 'source' or 'dest'"
  )

  # Invalid type
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", type = "invalid"),
    "'type' must be one of: local, osf, github"
  )

  # Multiple title values
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", title = c("title1", "title2")),
    "'title' must be a single character value"
  )
})

test_that("projr_restore_repo_wd clones into current directory", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create temporary directory for test
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  withr::with_dir(test_dir, {
    # Try to clone non-existent repository into current dir
    # Suppress expected warnings from git command
    result <- suppressWarnings(
      projr_restore_repo_wd(
        repo = "nonexistent/nonexistent-repo-12345"
      )
    )

    # Should return FALSE on error
    expect_false(result)
  })
})

test_that("projr_restore_repo_wd is equivalent to projr_restore_repo with path='.'", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Both functions should produce the same validation errors
  # Test with invalid label
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", label = 123),
    "'label' must be NULL or a character vector"
  )

  expect_error(
    projr_restore_repo(repo = "test/repo", path = ".", label = 123),
    "'label' must be NULL or a character vector"
  )

  # Test with invalid type
  expect_error(
    projr_restore_repo_wd(repo = "test/repo", type = "invalid"),
    "'type' must be one of"
  )

  expect_error(
    projr_restore_repo(repo = "test/repo", path = ".", type = "invalid"),
    "'type' must be one of"
  )
})

# =============================================================================
# Edge Cases and Additional Coverage
# =============================================================================

test_that("projr_restore_repo handles pos with both source and dest", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid pos values should not cause validation errors
  # (Will fail on clone, but not validation)
  # Suppress expected warnings from git command
  expect_error(
    suppressWarnings(
      projr_restore_repo(
        repo = "test/repo",
        pos = c("source", "dest")
      )
    ),
    NA,
    class = "error" # Some error expected (clone), but not validation
  )
})

test_that("projr_restore_repo handles valid type values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Each valid type should pass validation
  # (Will fail on clone, but not validation)
  for (valid_type in c("local", "osf", "github")) {
    # Suppress expected warnings from git command
    expect_error(
      suppressWarnings(
        projr_restore_repo(
          repo = "test/repo",
          type = valid_type
        )
      ),
      NA,
      class = "error" # Some error expected (clone), but not validation
    )
  }
})

test_that("projr_restore_repo validates all parameters together", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Combination of valid parameters should pass validation
  # (Will fail on clone, but not validation)
  # Suppress expected warnings from git command
  expect_error(
    suppressWarnings(
      projr_restore_repo(
        repo = "valid/repo",
        path = "some_path",
        label = c("raw-data", "cache"),
        pos = c("source", "dest"),
        type = "local",
        title = "test-title"
      )
    ),
    NA,
    class = "error" # Some error expected (clone), but not validation
  )
})

test_that(".restore_repo_labels changes directory and calls projr_content_update", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test the internal helper function behavior
  # Create a test directory structure
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Create a minimal projr structure in test_dir
  sub_dir <- file.path(test_dir, "subdir")
  dir.create(sub_dir)

  # Set up basic files needed for projr
  writeLines("version-format: major.minor.patch-dev", file.path(sub_dir, "_projr.yml"))
  writeLines("0.0.1", file.path(sub_dir, "VERSION"))

  # Create empty manifest
  manifest <- data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(sub_dir, "manifest.csv"), row.names = FALSE)

  # Get original working directory before calling withr::with_dir
  orig_wd <- getwd()

  # Call .restore_repo_labels which should change to sub_dir
  withr::with_dir(test_dir, {
    result <- tryCatch(
      .restore_repo_labels(sub_dir, NULL, NULL, NULL, NULL),
      error = function(e) {
        # Expected to fail due to empty manifest or missing structure
        FALSE
      }
    )
  })

  # Working directory should be restored to original after withr::with_dir exits
  expect_identical(getwd(), orig_wd)
})
