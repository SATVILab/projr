# remove any nodes and repos that were created

# directories to record them in


# function to delete them
# NOTE: These functions only delete repos created during THIS test run.
# When a test creates a GitHub repo, it writes a marker file
# to a temporary directory (.test_git_remote_dir_get_tmp()). 
# The cleanup function reads these marker files and only deletes those 
# specific repos. This ensures we don't accidentally delete repos created 
# by other concurrent test runs (e.g., on different CI/CD platforms).
.test_github_rm <- function() {
  # Only attempt cleanup if we can actually modify GitHub repos
  # This prevents errors during cleanup when only GITHUB_TOKEN is available
  can_modify <- tryCatch(.test_can_modify_github(), error = function(e) FALSE)
  if (!can_modify) {
    # Just clean up the temporary directory without trying to delete repos
    unlink(.test_git_remote_dir_get_tmp(), recursive = TRUE)
    return(invisible(TRUE))
  }

  fn_vec <- list.files(.test_git_remote_dir_get_tmp())
  fn_vec <- setdiff(fn_vec, "projr")
  for (i in seq_along(fn_vec)) {
    host <- c("repo" = fn_vec[i])
    # Wrap in try to avoid errors if repo doesn't exist or can't be checked
    repo_exists <- tryCatch(
      .test_remote_host_exists_github(host),
      error = function(e) FALSE
    )
    if (repo_exists) {
      try(
        .test_remote_host_rm(type = "github", host = host),
        silent = TRUE
      )
    }
  }
  unlink(.test_git_remote_dir_get_tmp(), recursive = TRUE)
}

.test_set()

.set_no_git_prompt()


# instruct deletion upon completion of all tests
withr::defer(
  {
    .test_github_rm()
    .test_unset()
    # .test_unset_select()
    try(.test_rm_random_abc(), silent = TRUE)
    try(.test_cleanup_ignore_files(), silent = TRUE)
  },
  envir = teardown_env()
)

.test_rm_random_abc <- function() {
  path_random <- file.path(dirname(tempdir()), "abc")
  if (file.exists(path_random)) {
    if (fs::is_dir(path_random)) {
      .dir_rm(path_random)
    }
    if (fs::is_file(path_random)) {
      file.remove(path_random)
    }
  }
  invisible(TRUE)
}

.test_cleanup_ignore_files <- function() {
  # Remove .gitignore and .Rbuildignore from check directory root
  # These may be created during test execution and cause R CMD check NOTEs

  # Get check directory root (parent of parent of testthat directory)
  # During testing, wd is typically projr.Rcheck/tests/testthat
  # We want to clean projr.Rcheck/
  wd <- getwd()

  # Try to find the check directory root
  # It should have .gitignore and .Rbuildignore at the root level
  check_root <- NULL

  # If we're in a .Rcheck directory structure
  if (grepl("\\.Rcheck", wd)) {
    # Split path and find the .Rcheck part
    path_parts <- strsplit(wd, .Platform$file.sep)[[1]]
    rcheck_idx <- grep("\\.Rcheck$", path_parts)
    if (length(rcheck_idx) > 0) {
      check_root <- paste(path_parts[1:rcheck_idx[1]], collapse = .Platform$file.sep)
    }
  }

  # If we found a check root, try to remove the files
  if (!is.null(check_root) && dir.exists(check_root)) {
    gitignore_path <- file.path(check_root, ".gitignore")
    rbuildignore_path <- file.path(check_root, ".Rbuildignore")

    if (file.exists(gitignore_path)) {
      file.remove(gitignore_path)
    }
    if (file.exists(rbuildignore_path)) {
      file.remove(rbuildignore_path)
    }
  }

  invisible(TRUE)
}

