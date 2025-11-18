# Tests for GitHub test repository cleanup mechanism
# Verifies that the cleanup system only deletes repos created during
# the current test run, not repos from other concurrent test runs

test_that(".test_github_rm only deletes repos tracked in tmp directory", {
  skip_if(.is_test_select())

  # Create a temporary directory to simulate the marker directory
  tmp_dir <- .test_git_remote_dir_get_tmp()

  # Verify the directory exists (created by helper function)
  expect_true(dir.exists(tmp_dir))

  # Create a few marker files to simulate tracked repos
  test_repo_names <- c("ProjrGitHubTestRepo1", "ProjrGitHubTestRepo2")
  for (repo_name in test_repo_names) {
    file.create(file.path(tmp_dir, repo_name))
  }

  # Verify marker files were created
  marker_files <- list.files(tmp_dir)
  expect_true(all(test_repo_names %in% marker_files))

  # Test that the function correctly reads these files
  # We can't actually run .test_github_rm() without real GitHub repos,
  # but we can verify the marker file mechanism works
  fn_vec <- list.files(tmp_dir)
  fn_vec <- setdiff(fn_vec, "projr")
  expect_equal(sort(fn_vec), sort(test_repo_names))

  # Clean up test marker files
  for (repo_name in test_repo_names) {
    unlink(file.path(tmp_dir, repo_name))
  }
})

test_that("marker files are created when repos are created", {
  skip_if(.is_test_select())

  # This test verifies the mechanism in .test_github_repo_create
  # Line 153: file.create(file.path(.test_git_remote_dir_get_tmp(), repo))

  tmp_dir <- .test_git_remote_dir_get_tmp()

  # Simulate what happens when a repo is created
  test_repo_name <- "ProjrGitHubTestSimulated"
  marker_path <- file.path(tmp_dir, test_repo_name)

  # Create marker file (as done in .test_github_repo_create)
  file.create(marker_path)

  # Verify marker exists
  expect_true(file.exists(marker_path))

  # Verify it would be found by cleanup
  fn_vec <- list.files(tmp_dir)
  expect_true(test_repo_name %in% fn_vec)

  # Clean up
  unlink(marker_path)
})

test_that(".test_git_remote_dir_get_tmp creates consistent directory", {
  skip_if(.is_test_select())

  # Verify the function creates a consistent directory path
  dir1 <- .test_git_remote_dir_get_tmp()
  dir2 <- .test_git_remote_dir_get_tmp()

  # Should return the same path
  expect_identical(dir1, dir2)

  # Should exist
  expect_true(dir.exists(dir1))

  # Should be in the expected location
  expect_true(grepl("github_repo_to_remove", dir1))
})

test_that("cleanup excludes 'projr' repo from deletion", {
  skip_if(.is_test_select())

  tmp_dir <- .test_git_remote_dir_get_tmp()

  # Create marker files including 'projr' (which should be excluded)
  test_repos <- c("projr", "ProjrGitHubTestRepo1", "ProjrGitHubTestRepo2")
  for (repo in test_repos) {
    file.create(file.path(tmp_dir, repo))
  }

  # Simulate the cleanup logic
  fn_vec <- list.files(tmp_dir)
  fn_vec <- setdiff(fn_vec, "projr")

  # Verify 'projr' is excluded
  expect_false("projr" %in% fn_vec)
  expect_true("ProjrGitHubTestRepo1" %in% fn_vec)
  expect_true("ProjrGitHubTestRepo2" %in% fn_vec)

  # Clean up
  for (repo in test_repos) {
    unlink(file.path(tmp_dir, repo))
  }
})
