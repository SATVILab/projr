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
    "repo must be given"
  )

  # Non-character repo
  expect_error(
    projr_restore_repo(repo = 123),
    "repo must be a non-empty string"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = character(0)),
    "repo must be a non-empty string"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = c("repo1", "repo2")),
    "repo must be a non-empty string"
  )

  # Empty string
  expect_error(
    projr_restore_repo(repo = ""),
    "repo must be a non-empty string"
  )
})

test_that("projr_restore_repo validates path parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character path (when not NULL)
  expect_error(
    projr_restore_repo(repo = "test/repo", path = 123),
    "path must be a non-empty string"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", path = c("path1", "path2")),
    "path must be a non-empty string"
  )

  # Empty string
  expect_error(
    projr_restore_repo(repo = "test/repo", path = ""),
    "path must be a non-empty string"
  )
})

test_that("projr_restore_repo validates label parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character label
  expect_error(
    projr_restore_repo(repo = "test/repo", label = 123),
    "label must be a non-empty character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", label = character(0)),
    "label must be a non-empty character vector"
  )
})

test_that("projr_restore_repo validates pos parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character pos
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = 123),
    "pos must be a non-empty character vector"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = character(0)),
    "pos must be a non-empty character vector"
  )

  # Invalid value
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = "invalid"),
    "pos must be one of source, dest"
  )

  # Multiple invalid values
  expect_error(
    projr_restore_repo(repo = "test/repo", pos = c("source", "invalid")),
    "pos must be one of source, dest.*invalid"
  )
})

test_that("projr_restore_repo validates type parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character type
  expect_error(
    projr_restore_repo(repo = "test/repo", type = 123),
    "type must be a non-empty string"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", type = character(0)),
    "type must be a non-empty string"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", type = c("local", "github")),
    "type must be a non-empty string"
  )

  # Invalid value
  expect_error(
    projr_restore_repo(repo = "test/repo", type = "invalid"),
    "type must be one of local, github"
  )
})

test_that("projr_restore_repo validates title parameter correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Non-character title
  expect_error(
    projr_restore_repo(repo = "test/repo", title = 123),
    "title must be a non-empty string"
  )

  # Empty vector
  expect_error(
    projr_restore_repo(repo = "test/repo", title = character(0)),
    "title must be a non-empty string"
  )

  # Multiple values
  expect_error(
    projr_restore_repo(repo = "test/repo", title = c("title1", "title2")),
    "title must be a non-empty string"
  )
})

test_that("repo restoration clones into the requested path and forwards selections", {
  skip_if(.is_test_select())
  skip_if_not_installed("gert")
  remote <- .test_setup_project()
  parent <- withr::local_tempdir()
  expect_error(projr_restore_repo_wd(NULL), "repo must be given")
  labels <- c("raw-data", "output")
  positions <- c("source", "dest")
  received <- NULL
  testthat::local_mocked_bindings(
    .git_clone = function(repo, path) {
      stopifnot(repo == "fixture/project")
      gert::git_clone(remote, path)
    },
    projr_content_update = function(label, pos, type, title, destination) {
      expect_true(file.exists("_projr.yml"))
      received <<- list(label, pos, type, title, destination, getwd())
      invisible(TRUE)
    }
  )
  withr::with_dir(parent, {
    expect_true(projr_restore_repo("fixture/project", "one", labels, positions, "local", "archive", destination = "bundles"))
    expect_identical(received, list(labels, positions, "local", "archive", "bundles", normalizePath("one", winslash = "/")))
    dir.create("two")
    withr::with_dir("two", {
      expect_true(projr_restore_repo_wd("fixture/project", type = "github"))
      expect_identical(received, list(NULL, NULL, "github", NULL, NULL, getwd()))
    })
  })
})

test_that("repo restoration reports failures and restores the working directory", {
  skip_if(.is_test_select())
  parent <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .git_clone = function(repo, path) {
      if (repo == "fixture/missing") stop("repository missing")
    },
    projr_content_update = function(...) stop("restore failed")
  )
  withr::with_dir(parent, {
    expect_false(projr_restore_repo("fixture/missing"))
    dir.create("clone")
    expect_false(projr_restore_repo("fixture/project", "clone"))
    expect_identical(getwd(), normalizePath(parent, winslash = "/"))
  })
})

test_that("short GitHub clone names use the authenticated owner", {
  skip_if(.is_test_select())
  skip_if_not_installed("gh")
  user <- "fixture"
  testthat::local_mocked_bindings(gh_whoami = function() list(login = user), .package = "gh")
  testthat::local_mocked_bindings(
    .auth_check_github = function(...) TRUE,
    .git_system_get = function() "git",
    .git_clone_git = function(repo, path) c(repo, path)
  )
  expect_identical(.git_clone("project", "clone"), c("fixture/project", "clone"))
  user <- NULL
  expect_error(.git_clone("project"), "GitHub user not found", fixed = TRUE)
})
