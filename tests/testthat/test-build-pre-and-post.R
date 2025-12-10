# Tests for R/build-pre_and_post.R functions
# These tests are LITE-compatible (skip_if(.is_test_select()) only)

# =============================================================================
# .build_renv_snapshot_check tests
# =============================================================================

test_that(".build_renv_snapshot_check returns FALSE when output_run is FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_renv_snapshot_check(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_renv_snapshot_check returns FALSE in test environment", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Even with output_run=TRUE, should return FALSE because .is_test() is TRUE
      result <- .build_renv_snapshot_check(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_renv_snapshot_check returns FALSE when renv not detected", {
  skip_if(.is_test_select())
  dir_test <- tempfile("test_no_renv")
  dir.create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create minimal DESCRIPTION without renv
      writeLines(c("Package: test", "Version: 0.0.1", "Title: Test"), "DESCRIPTION")

      # Temporarily unset test environment
      old_is_test <- Sys.getenv("R_PKG_IS_TEST")
      Sys.unsetenv("R_PKG_IS_TEST")
      withr::defer(if (nzchar(old_is_test)) Sys.setenv(R_PKG_IS_TEST = old_is_test))

      result <- .build_renv_snapshot_check(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# .build_renv_snapshot tests
# =============================================================================

test_that(".build_renv_snapshot returns FALSE when output_run is FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_renv_snapshot(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_renv_snapshot returns FALSE in test environment", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_renv_snapshot(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_check tests
# =============================================================================

test_that(".build_git_check returns FALSE when output_run is FALSE", {
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

test_that(".build_git_check returns FALSE when git commit is FALSE in YAML", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(commit = FALSE)
      result <- .build_git_check(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_check returns TRUE when git commit is TRUE in YAML", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(commit = TRUE)
      result <- .build_git_check(output_run = TRUE)
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_commit_pre_warn tests
# =============================================================================

test_that(".build_git_commit_pre_warn runs without error when not pre stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_commit_pre_warn(stage = "post", output_run = TRUE)
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit_pre_warn runs without error when output_run is FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_commit_pre_warn(stage = "pre", output_run = FALSE)
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit_pre_warn runs without error when git repo exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_commit_pre_warn(stage = "pre", output_run = TRUE)
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_commit_get_msg tests
# =============================================================================

test_that(".build_git_commit_get_msg returns pre-build message for pre stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      result <- .build_git_commit_get_msg(msg = "test message", stage = "pre")
      expect_identical(result, "Snapshot pre-build")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit_get_msg returns post-build message for post stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3")
      result <- .build_git_commit_get_msg(msg = "test message", stage = "post")
      expect_match(result, "Build v1.2.3: test message")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit_get_msg handles empty message for post stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("0.0.1")
      result <- .build_git_commit_get_msg(msg = "", stage = "post")
      expect_match(result, "Build v0.0.1:")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_msg_get tests
# =============================================================================

test_that(".build_git_msg_get returns correct message for pre stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_msg_get(
        stage = "pre",
        version_run_on_list = NULL,
        bump_component = "patch",
        msg = "test"
      )
      expect_identical(result, "Snapshot pre-patch build")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_msg_get returns correct message for post stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      version_list <- list(desc = list(success = "1.2.3"))
      result <- .build_git_msg_get(
        stage = "post",
        version_run_on_list = version_list,
        bump_component = "minor",
        msg = "test message"
      )
      expect_identical(result, "Record minor v1.2.3 build: test message")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_msg_get returns correct message for dev stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("2.0.0-1")
      version_list <- list(desc = list(success = "2.0.0"))
      result <- .build_git_msg_get(
        stage = "dev",
        version_run_on_list = version_list,
        bump_component = NULL,
        msg = NULL
      )
      expect_identical(result, "Begin v2.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_msg_get_pre tests
# =============================================================================

test_that(".build_git_msg_get_pre returns correct format", {
  skip_if(.is_test_select())
  result <- .build_git_msg_get_pre(bump_component = "patch")
  expect_identical(result, "Snapshot pre-patch build")

  result <- .build_git_msg_get_pre(bump_component = "minor")
  expect_identical(result, "Snapshot pre-minor build")

  result <- .build_git_msg_get_pre(bump_component = "major")
  expect_identical(result, "Snapshot pre-major build")
})

# =============================================================================
# .build_git_msg_get_post tests
# =============================================================================

test_that(".build_git_msg_get_post returns correct format with message", {
  skip_if(.is_test_select())
  version_list <- list(desc = list(success = "1.2.3"))
  result <- .build_git_msg_get_post(
    bump_component = "patch",
    version_run_on_list = version_list,
    msg = "Added new feature"
  )
  expect_identical(result, "Record patch v1.2.3 build: Added new feature")
})

test_that(".build_git_msg_get_post returns correct format without message", {
  skip_if(.is_test_select())
  version_list <- list(desc = list(success = "0.0.1"))
  result <- .build_git_msg_get_post(
    bump_component = "minor",
    version_run_on_list = version_list,
    msg = ""
  )
  expect_identical(result, "Record minor v0.0.1 build")
})

# =============================================================================
# .build_git_msg_get_dev tests
# =============================================================================

test_that(".build_git_msg_get_dev returns correct format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_version_set("1.2.3-1")
      version_list <- list(desc = list(success = "1.2.3"))
      result <- .build_git_msg_get_dev(version_run_on_list = version_list)
      expect_identical(result, "Begin v1.2.3-1")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_commit tests
# =============================================================================

test_that(".build_git_commit returns FALSE when output_run is FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_commit(
        output_run = FALSE,
        bump_component = "patch",
        version_run_on_list = NULL,
        stage = "pre",
        msg = "test"
      )
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit returns FALSE when git commit is disabled", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(commit = FALSE)
      result <- .build_git_commit(
        output_run = TRUE,
        bump_component = "patch",
        version_run_on_list = NULL,
        stage = "pre",
        msg = "test"
      )
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit errors when git repo missing but commits requested", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(commit = TRUE)
      expect_error(
        .build_git_commit(
          output_run = TRUE,
          bump_component = "patch",
          version_run_on_list = NULL,
          stage = "pre",
          msg = "test"
        ),
        "Git commits requested but no Git directory found"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_commit works with valid git repo", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      .test_setup_project_git_config()
      projr_yml_git_set(commit = TRUE)

      # Create a file to commit
      writeLines("test content", "test.txt")

      # Should not error
      expect_silent(.build_git_commit(
        output_run = TRUE,
        bump_component = "patch",
        version_run_on_list = NULL,
        stage = "pre",
        msg = "test"
      ))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_git_push tests
# =============================================================================

test_that(".build_git_push returns FALSE when output_run is FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .build_git_push(output_run = FALSE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_push returns FALSE when git push is disabled in YAML", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(push = FALSE)
      result <- .build_git_push(output_run = TRUE)
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_push errors when no git repo exists", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      projr_yml_git_set(push = TRUE)
      expect_error(
        .build_git_push(output_run = TRUE),
        "No git repository detected but needed based on settings"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_git_push warns when no upstream remote", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      .test_setup_project_git_config()
      projr_yml_git_set(push = TRUE)

      # No remote configured, should warn
      expect_warning(
        .build_git_push(output_run = TRUE),
        "No upstream remote detected"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})
