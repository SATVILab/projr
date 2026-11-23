# Test functions in R/remote-misc.R
#
# This file tests the utility functions for remote operations:
# - .remote_ls() - List all configured remotes
# - .remote_ls_source() - List remotes from directory configuration
# - .remote_ls_dest() - List remotes from build destination configuration
# - .gh_repo_get() - Get GitHub repository name
# - .gh_guess_repo() - Parse GitHub repository from git remote URL

# =============================================================================
# .remote_ls_dest
# =============================================================================

test_that(".remote_ls_dest returns empty when no destinations configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # By default, no remote destinations should be configured
      remote_vec <- .remote_ls_dest()
      expect_identical(remote_vec, character(0))
    }
  )
})

test_that(".remote_ls_dest returns github when github destination configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add a GitHub destination
      projr_yml_dest_add_github(
        title = "test-release",
        content = "output"
      )

      remote_vec <- .remote_ls_dest()
      expect_true("github" %in% remote_vec)
    }
  )
})

# =============================================================================
# .remote_ls_source
# =============================================================================

test_that(".remote_ls_source returns empty when no source remotes configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # By default, no directory source remotes should be configured
      remote_vec <- .remote_ls_source()
      expect_identical(remote_vec, character(0))
    }
  )
})

test_that(".remote_ls_source returns github when github source configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add GitHub as a source for a directory
      yml_projr <- .yml_get("default")
      yml_projr$directories$`raw-data`$github <- list(id = "test-tag")
      .yml_set(yml_projr, "default")

      remote_vec <- .remote_ls_source()
      expect_true("github" %in% remote_vec)
    }
  )
})

test_that(".remote_ls_source returns unique remotes from multiple directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Add GitHub as sources for different directories
      yml_projr <- .yml_get("default")
      yml_projr$directories$`raw-data`$github <- list(id = "test-tag")
      yml_projr$directories$cache$github <- list(id = "test-tag2")
      yml_projr$directories$output$github <- list(id = "test-tag3")
      .yml_set(yml_projr, "default")

      remote_vec <- .remote_ls_source()
      # Should be unique (only github)
      expect_true("github" %in% remote_vec)
      expect_identical(length(remote_vec), 1L)
    }
  )
})

# =============================================================================
# .remote_ls
# =============================================================================

test_that(".remote_ls includes a configured GitHub destination", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Configure GitHub destination
      projr_yml_dest_add_github(
        title = "test-release",
        content = "output"
      )

      remote_vec <- .remote_ls()
      expect_true("github" %in% remote_vec)
      # Should be unique (only one "github")
      expect_identical(sum(remote_vec == "github"), 1L)
    }
  )
})

test_that(".remote_ls returns unique remotes when duplicates present", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add GitHub as both source and destination
      yml_projr <- .yml_get("default")
      yml_projr$directories$`raw-data`$github <- list(id = "test-tag")
      .yml_set(yml_projr, "default")

      projr_yml_dest_add_github(
        title = "test-release",
        content = "output"
      )

      # Should have unique remotes
      remote_vec <- .remote_ls()
      expect_true("github" %in% remote_vec)
      # GitHub should appear only once despite being used as both source and destination
      expect_identical(sum(remote_vec == "github"), 1L)
    }
  )
})

test_that("Git pushes do not require GitHub API authentication", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      local_mocked_bindings(.git_gh_check_auth = function(...) stop("Unexpected API check"))
      for (push in c(TRUE, FALSE)) {
        projr_yml_git_set(push = push, simplify_default = FALSE)
        expect_false("github" %in% .remote_ls())
        expect_true(.build_check_auth_remote())
      }
    }
  )
})

test_that(".remote_ls filters out empty strings", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Default configuration with git push disabled
      projr_yml_git_set(
        push = FALSE,
        simplify_default = FALSE
      )

      remote_vec <- .remote_ls()
      # Should not contain any empty strings
      expect_true(all(nzchar(remote_vec)))
    }
  )
})

# =============================================================================
# .gh_guess_repo
# =============================================================================

test_that(".gh_guess_repo reads each supported origin URL format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  project <- .test_setup_project()
  usethis::with_project(project, {
    urls <- c(
      "git@github.com:owner/repo.git",
      "https://github.com/owner/repo.git",
      "https://github.com/owner/repo",
      "ssh://git@github.com/owner/repo.git",
      "owner/repo",
      "https://github.com/owner/repo/",
      "https://github.com/owner/repo.git/"
    )
    for (url in urls) {
      expect_identical(system2("git", c("config", "remote.origin.url", url)), 0L)
      expect_identical(.gh_guess_repo(), "owner/repo", info = url)
    }
  })
})

test_that(".gh_guess_repo throws error when no remotes exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # No remotes added
      expect_error(
        .gh_guess_repo(),
        "remote"
      )
    }
  )
})

test_that(".gh_guess_repo falls back to first remote when origin doesn't exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote named 'upstream' instead of 'origin'
      system2("git", c("remote", "add", "upstream", "git@github.com:owner/repo.git"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo throws error for invalid URL format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_lite())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with invalid format (missing owner or repo)
      system2("git", c("remote", "add", "origin", "invalid-url"))

      expect_error(
        .gh_guess_repo(),
        "Failed to parse owner/repo from remote URL"
      )
    }
  )
})

test_that(".gh_guess_repo works with custom path argument", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create a separate git repo in a subdirectory
  sub_dir <- file.path(tempdir(), "test_gh_guess_repo_subdir")
  if (dir.exists(sub_dir)) unlink(sub_dir, recursive = TRUE)
  .dir_create(sub_dir)
  withr::defer(unlink(sub_dir, recursive = TRUE))

  # Initialize git in subdirectory
  withr::with_dir(sub_dir, {
    system2("git", c("init"), stdout = FALSE, stderr = FALSE)
    system2("git", c("config", "user.name", "Test User"))
    system2("git", c("config", "user.email", "test@example.com"))
    system2("git", c("remote", "add", "origin", "git@github.com:test/repo.git"))
  })

  # Call .gh_guess_repo with the subdirectory path
  repo <- .gh_guess_repo(sub_dir)
  expect_identical(repo, "test/repo")
})

# =============================================================================
# .gh_repo_get
# =============================================================================

test_that(".gh_repo_get works in normal git repository", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .test_setup_project()
  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote
      system2("git", c("remote", "add", "origin", "git@github.com:owner/repo.git"))

      repo <- .gh_repo_get()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_repo_get works in git worktree", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Create a main git repository
  main_repo <- file.path(tempdir(), "main_repo_test")
  if (dir.exists(main_repo)) unlink(main_repo, recursive = TRUE)
  .dir_create(main_repo)
  withr::defer(unlink(main_repo, recursive = TRUE))

  withr::with_dir(main_repo, {
    system2("git", c("init"), stdout = FALSE, stderr = FALSE)
    system2("git", c("config", "user.name", "Test User"))
    system2("git", c("config", "user.email", "test@example.com"))
    system2("git", c("remote", "add", "origin", "git@github.com:owner/repo.git"))

    # The project marker is shared by both checkouts.
    writeLines("directories: {}", "_projr.yml")
    system2("git", c("add", "_projr.yml"))
    system2("git", c("commit", "-m", "Initial"), stdout = FALSE, stderr = FALSE)

    # Create a worktree
    worktree_dir <- file.path(tempdir(), "worktree_test")
    if (dir.exists(worktree_dir)) unlink(worktree_dir, recursive = TRUE)
    withr::defer(unlink(worktree_dir, recursive = TRUE))
    system2("git", c("worktree", "add", worktree_dir, "-b", "feature"))

    # Test from worktree
    withr::with_dir(worktree_dir, {
      repo <- .gh_repo_get()
      expect_identical(repo, "owner/repo")
    })
  })
})

test_that(".gh_repo_get throws error when gitdir does not exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = FALSE, github = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test2,
    code = {
      # Remove .git directory if it exists
      git_dir <- file.path(dir_test2, ".git")
      if (dir.exists(git_dir)) {
        unlink(git_dir, recursive = TRUE)
      }

      # Create a fake .git file pointing to non-existent directory
      writeLines("gitdir: /nonexistent/path", git_dir)

      expect_error(
        .gh_repo_get(),
        "Cannot find gitdir"
      )
    }
  )
})
