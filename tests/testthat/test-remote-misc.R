# Test functions in R/remote-misc.R
#
# This file tests the utility functions for remote operations:
# - .remote_ls() - List all configured remotes
# - .remote_ls_source() - List remotes from directory configuration
# - .remote_ls_dest() - List remotes from build destination configuration
# - .git_push_check() - Check if git push is enabled
# - .gh_repo_get() - Get GitHub repository name
# - .gh_guess_repo() - Parse GitHub repository from git remote URL

# =============================================================================
# Setup
# =============================================================================

dir_test <- .test_setup_project(
  git = TRUE, github = FALSE, set_env_var = TRUE
)

# =============================================================================
# .remote_ls_dest
# =============================================================================

test_that(".remote_ls_dest returns empty when no destinations configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

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

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add GitHub as a source for a directory
      yml_projr <- .yml_get("default")
      if (is.null(yml_projr$directories)) {
        yml_projr$directories <- list()
      }
      yml_projr$directories$`raw-data` <- list(github = list(id = "test-tag"))
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
      if (is.null(yml_projr$directories)) {
        yml_projr$directories <- list()
      }
      yml_projr$directories$`raw-data` <- list(github = list(id = "test-tag"))
      yml_projr$directories$cache <- list(github = list(id = "test-tag2"))
      yml_projr$directories$output <- list(github = list(id = "test-tag3"))
      .yml_set(yml_projr, "default")

      remote_vec <- .remote_ls_source()
      # Should be unique (only github)
      expect_true("github" %in% remote_vec)
      expect_identical(length(remote_vec), 1L)
    }
  )
})

# =============================================================================
# .git_push_check
# =============================================================================

test_that(".git_push_check returns TRUE when git setting is NULL (default)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Explicitly set git to NULL to test default behavior
      yml_projr <- .yml_get("default")
      yml_projr$build$git <- NULL
      .yml_set(yml_projr, "default")

      # When git setting is NULL, push should be TRUE
      result <- .git_push_check()
      expect_true(result)
    }
  )
})

test_that(".git_push_check returns TRUE when git setting is TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Set git to TRUE
      projr_yml_git_set(all = TRUE, simplify_default = FALSE)

      result <- .git_push_check()
      expect_true(result)
    }
  )
})

test_that(".git_push_check returns FALSE when git setting is FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Set git to FALSE
      projr_yml_git_set(
        commit = FALSE,
        push = FALSE,
        add_untracked = FALSE,
        simplify_identical = TRUE,
        simplify_default = FALSE
      )

      result <- .git_push_check()
      expect_false(result)
    }
  )
})

test_that(".git_push_check returns FALSE when push is FALSE in list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Set only push to FALSE
      projr_yml_git_set(
        push = FALSE,
        simplify_default = FALSE
      )

      result <- .git_push_check()
      expect_false(result)
    }
  )
})

test_that(".git_push_check returns TRUE when push is not specified in list", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Manually set the YAML to have a list with only commit FALSE
      yml_projr <- .yml_get("default")
      yml_projr$build$git <- list(commit = FALSE)
      .yml_set(yml_projr, "default")

      # When push is NULL in the list, it should default to TRUE
      result <- .git_push_check()
      expect_true(result)
    }
  )
})

test_that(".git_push_check throws error for invalid git setting type", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Manually set an invalid type (e.g., character string)
      yml_projr <- .yml_get("default")
      yml_projr$build$git <- "invalid"
      .yml_set(yml_projr, "default")

      expect_error(
        .git_push_check(),
        "git setting.*not recognized"
      )
    }
  )
})

# =============================================================================
# .remote_ls
# =============================================================================

test_that(".remote_ls combines source, dest, and git push remotes", {
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

      # Should include github from dest and git push check
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

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add GitHub as both source and destination
      yml_projr <- .yml_get("default")
      if (is.null(yml_projr$directories)) {
        yml_projr$directories <- list()
      }
      yml_projr$directories$`raw-data` <- list(github = list(id = "test-tag"))
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

test_that(".remote_ls excludes github when git push is disabled", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test2 <- .test_setup_project(git = TRUE, github = FALSE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test2,
    code = {
      .init()

      # Disable git push
      projr_yml_git_set(
        push = FALSE,
        simplify_default = FALSE
      )

      # Should not include github from git push
      remote_vec <- .remote_ls()
      expect_false("github" %in% remote_vec)
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

test_that(".gh_guess_repo parses SSH scp-like URL format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a temporary git repo with SSH remote URL
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with SSH scp-like format
      system2("git", c("remote", "add", "origin", "git@github.com:owner/repo.git"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo parses HTTPS URL format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with HTTPS format
      system2("git", c("remote", "add", "origin", "https://github.com/owner/repo.git"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo parses HTTPS URL without .git extension", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with HTTPS format without .git
      system2("git", c("remote", "add", "origin", "https://github.com/owner/repo"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo parses SSH protocol URL format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with ssh:// protocol format
      system2("git", c("remote", "add", "origin", "ssh://git@github.com/owner/repo.git"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo handles owner/repo format directly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with just owner/repo
      system2("git", c("remote", "add", "origin", "owner/repo"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo removes trailing slashes", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  usethis::with_project(
    path = dir_test,
    code = {
      .git_init()
      .test_setup_project_git_config()

      # Add a remote with trailing slash
      system2("git", c("remote", "add", "origin", "https://github.com/owner/repo/"))

      repo <- .gh_guess_repo()
      expect_identical(repo, "owner/repo")
    }
  )
})

test_that(".gh_guess_repo throws error when no remotes exist", {
  skip_if(.is_test_cran())
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

    # Create initial commit
    writeLines("test", "test.txt")
    system2("git", c("add", "test.txt"))
    system2("git", c("commit", "-m", "Initial commit"), stdout = FALSE, stderr = FALSE)

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
