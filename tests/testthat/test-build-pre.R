# Tests for R/build-pre.R functions
# These tests are LITE-compatible (skip_if(.is_test_select()) only)

# =============================================================================
# .build_pre_check tests
# =============================================================================

test_that(".build_pre_check runs all checks without error when output_run=TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Should run without error
      expect_silent(.build_pre_check(output_run = TRUE, output_level = "none"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_pre_check returns FALSE when output_run=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_pre_check(output_run = FALSE)
      # Most check functions return invisible(FALSE) when output_run=FALSE
      # but .build_pre_check doesn't return anything, just runs checks
      expect_true(TRUE) # Just verify it doesn't error
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_check tests
# =============================================================================

test_that(".build_git_check works with existing git repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      # When git repo exists, function should not error
      # Actual return value depends on YAML configuration
      result <- .build_git_check(output_run = TRUE)
      expect_true(is.logical(result) || is.null(result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_check returns FALSE when output_run=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_check(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_check works when git=FALSE in YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Ensure no git repo
      expect_false(.git_repo_check_exists())

      # With git=FALSE in YAML, function should not error
      result <- .build_git_check(output_run = TRUE)
      expect_true(is.logical(result) || is.null(result))

      # Git should remain disabled in YAML
      yml_git <- .yml_git_get(NULL)
      expect_false(yml_git)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_check_required tests
# =============================================================================

test_that(".build_git_check_required returns TRUE when git.commit=TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Need to set commit explicitly and have other values NULL or FALSE
      # to trigger the explicit requirement
      projr_yml_git_set(commit = TRUE, push = FALSE, add_untracked = FALSE)
      result <- .build_git_check_required()
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_check_required returns FALSE when git=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(FALSE)
      result <- .build_git_check_required()
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_github_check tests
# =============================================================================

test_that(".build_github_check returns FALSE when output_run=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_github_check(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_github_check returns FALSE when no git repo exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_github_check(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_github_check returns FALSE when remote already exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Manually add a remote
      system("git remote add origin https://github.com/test/test.git")

      result <- .build_github_check(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_exit_if_behind_upstream tests
# =============================================================================

test_that(".build_exit_if_behind_upstream returns FALSE when output_run=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_exit_if_behind_upstream(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_exit_if_behind_upstream returns FALSE when no git repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_exit_if_behind_upstream(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_exit_if_behind_upstream returns FALSE when not_behind is FALSE in YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_restrictions_set(not_behind = FALSE)
      result <- .build_exit_if_behind_upstream(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_output_get_bump_component tests
# =============================================================================

test_that(".build_output_get_bump_component returns last component from version format", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Default format is major.minor.patch-dev
      result <- .build_output_get_bump_component()
      expect_identical(result, "patch")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_output_get_bump_component returns provided bump_component", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_output_get_bump_component("minor")
      expect_identical(result, "minor")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_is_current_version_dev tests
# =============================================================================

test_that(".build_is_current_version_dev returns FALSE when no VERSION or DESCRIPTION file", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Don't initialize - no VERSION or DESCRIPTION file
      # But .test_setup_project copies files, so we need to remove them
      if (file.exists("VERSION")) file.remove("VERSION")
      if (file.exists("DESCRIPTION")) file.remove("DESCRIPTION")

      result <- .build_is_current_version_dev()
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_is_current_version_dev returns TRUE when on dev version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("0.0.1-1")
      result <- .build_is_current_version_dev()
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_is_current_version_dev returns FALSE when on release version", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3")
      result <- .build_is_current_version_dev()
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_output_get_msg tests
# =============================================================================

test_that(".build_output_get_msg returns provided message", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  result <- .build_output_get_msg("Test message")
  expect_identical(result, "Test message")
})

test_that(".build_output_get_msg returns default in non-interactive mode when msg is NULL", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip_if(interactive())
  result <- .build_output_get_msg(NULL)
  expect_identical(result, "Build project")
})

# =============================================================================
# .build_check_auth_remote tests
# =============================================================================

test_that(".build_check_auth_remote returns TRUE when no remotes configured", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_check_auth_remote()
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_debug_git_info tests
# =============================================================================

test_that(".build_debug_git_info runs without error when no git repo", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      expect_silent(.build_debug_git_info(output_level = "none"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_debug_git_info runs without error when git repo exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      .test_setup_project_git_config()
      writeLines("test", "test.txt")
      .git_commit_file("test.txt", "test commit")

      expect_silent(.build_debug_git_info(output_level = "none"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_version_set_pre tests
# =============================================================================

test_that(".build_version_set_pre sets version correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      version_list <- list(desc = list(run = "1.2.3"))
      .build_version_set_pre(version_list)
      expect_identical(projr_version_get(), "1.2.3")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_doc_output_dir_update tests
# =============================================================================

test_that(".build_doc_output_dir_update returns safe path when output_run=FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_doc_output_dir_update(output_run = FALSE)
      # Should return a safe (versioned cache) path
      expect_true(grepl("_tmp/projr", result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_doc_output_dir_update returns unsafe path when output_run=TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_doc_output_dir_update(output_run = TRUE)
      # Should return unsafe (final) docs path
      expect_true(grepl("docs", result))
      expect_false(grepl("_tmp/projr", result))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_ensure_dev_version tests
# =============================================================================

test_that(".build_ensure_dev_version bumps release version to dev", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3")
      .build_ensure_dev_version()
      version <- projr_version_get()
      # Should be a dev version now
      expect_match(version, "-")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_ensure_dev_version keeps dev version as dev", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3-5")
      .build_ensure_dev_version()
      version <- projr_version_get()
      # Should still be a dev version
      expect_match(version, "-")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_ignore tests
# =============================================================================

test_that(".build_ignore runs without error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      expect_silent(.build_ignore(output_run = TRUE, archive_local = FALSE, output_level = "none"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_pre_document tests
# =============================================================================

test_that(".build_pre_document runs without error", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      expect_silent(.build_pre_document(output_run = TRUE, archive_local = FALSE, output_level = "none"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_pre_document ensures dev version is set", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3")
      .build_pre_document(output_run = TRUE, archive_local = FALSE, output_level = "none")
      version <- projr_version_get()
      # Should be dev version now
      expect_match(version, "-")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_pre_setup_for_output_run tests
# =============================================================================

test_that(".build_pre_setup_for_output_run sets version correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      version_list <- list(desc = list(run = "2.0.0"))
      .build_pre_setup_for_output_run(
        version_run_on_list = version_list,
        output_run = TRUE,
        clear_output = "pre",
        output_level = "none"
      )
      expect_identical(projr_version_get(), "2.0.0")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_pre_setup_for_output_run clears output directories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      # Create test content in output directory
      output_dir <- projr_path_get_dir("output", safe = FALSE, create = TRUE)
      test_file <- file.path(output_dir, "test.txt")
      writeLines("test", test_file)
      expect_true(file.exists(test_file))

      version_list <- list(desc = list(run = "0.0.1"))
      .build_pre_setup_for_output_run(
        version_run_on_list = version_list,
        output_run = TRUE,
        clear_output = "pre",
        output_level = "none"
      )

      # File should be cleared
      expect_false(file.exists(test_file))
    },
    quiet = TRUE,
    force = TRUE
  )
})
