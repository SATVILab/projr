# Tests for R/build-hooks.R functions
# These tests cover hook execution for both production and dev builds

# =============================================================================
# .hook_run tests
# =============================================================================

test_that(".hook_run executes script in isolated environment", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a hook that sets a variable
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("test_var <- 'hook_executed'"),
        "hooks/test-hook.R"
      )

      # Run the hook
      .hook_run("hooks/test-hook.R")

      # Variable should not pollute global environment
      expect_false(exists("test_var", envir = .GlobalEnv))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".hook_run errors on non-existent file", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(
        .hook_run("nonexistent-hook.R"),
        "Hook 'nonexistent-hook.R' does not exist"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".hook_run executes script successfully", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a hook that writes to a file
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('executed', 'hook-output.txt')"),
        "hooks/write-hook.R"
      )

      # Run the hook
      .hook_run("hooks/write-hook.R")

      # Check that hook executed
      expect_true(file.exists("hook-output.txt"))
      expect_identical(readLines("hook-output.txt"), "executed")
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_hooks_run tests - production builds
# =============================================================================

test_that(".build_hooks_run executes pre hooks for production builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('pre-hook-1', 'pre-log.txt')"),
        "hooks/pre1.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('pre-log.txt')) readLines('pre-log.txt') else character(0)",
          "writeLines(c(log, 'pre-hook-2'), 'pre-log.txt')"
        ),
        "hooks/pre2.R"
      )

      # Configure hooks in YAML
      projr_yml_hooks_add_pre(c("hooks/pre1.R", "hooks/pre2.R"))

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")

      # Verify both hooks executed in order
      expect_true(file.exists("pre-log.txt"))
      log <- readLines("pre-log.txt")
      expect_identical(log, c("pre-hook-1", "pre-hook-2"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run executes post hooks for production builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('post-executed', 'post-log.txt')"),
        "hooks/post.R"
      )

      # Configure hooks in YAML
      projr_yml_hooks_add_post("hooks/post.R")

      # Run post hooks
      .build_hooks_run("post", is_dev_build = FALSE, output_level = "none")

      # Verify hook executed
      expect_true(file.exists("post-log.txt"))
      expect_identical(readLines("post-log.txt"), "post-executed")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run executes both hooks in pre and post stages", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('both-log.txt')) readLines('both-log.txt') else character(0)",
          "writeLines(c(log, 'both-hook'), 'both-log.txt')"
        ),
        "hooks/both.R"
      )

      # Configure both hook
      projr_yml_hooks_add("hooks/both.R", stage = "both")

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")

      # Verify hook executed during pre
      expect_true(file.exists("both-log.txt"))
      expect_length(readLines("both-log.txt"), 1)

      # Run post hooks
      .build_hooks_run("post", is_dev_build = FALSE, output_level = "none")

      # Verify hook executed during post as well
      log <- readLines("both-log.txt")
      expect_length(log, 2)
      expect_identical(log, c("both-hook", "both-hook"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run handles missing hooks gracefully", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No hooks configured
      result <- .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")
      expect_true(result)

      result <- .build_hooks_run("post", is_dev_build = FALSE, output_level = "none")
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_hooks_run tests - dev builds
# =============================================================================

test_that(".build_hooks_run uses dev.hooks for dev builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('build-hook', 'build-log.txt')"),
        "hooks/build.R"
      )
      writeLines(
        c("writeLines('dev-hook', 'dev-log.txt')"),
        "hooks/dev.R"
      )

      # Configure both build.hooks and dev.hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = "hooks/build.R"
            )
          ),
          dev = list(
            hooks = list(
              pre = "hooks/dev.R"
            )
          )
        ),
        "_projr.yml"
      )

      # Run dev build hooks
      .build_hooks_run("pre", is_dev_build = TRUE, output_level = "none")

      # Only dev hook should execute
      expect_true(file.exists("dev-log.txt"))
      expect_false(file.exists("build-log.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run ignores build.hooks for dev builds", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('build-hook', 'build-log.txt')"),
        "hooks/build.R"
      )

      # Configure only build.hooks (no dev.hooks)
      projr_yml_hooks_add_pre("hooks/build.R")

      # Run dev build hooks
      .build_hooks_run("pre", is_dev_build = TRUE, output_level = "none")

      # build.hooks should not execute for dev builds
      expect_false(file.exists("build-log.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run handles dev.hooks with both stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('both-log.txt')) readLines('both-log.txt') else character(0)",
          "writeLines(c(log, 'both-executed'), 'both-log.txt')"
        ),
        "hooks/both.R"
      )

      # Configure dev.hooks with both stage
      yaml::write_yaml(
        list(
          dev = list(
            hooks = list(
              both = "hooks/both.R"
            )
          )
        ),
        "_projr.yml"
      )

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = TRUE, output_level = "none")
      expect_true(file.exists("both-log.txt"))
      expect_length(readLines("both-log.txt"), 1)

      # Run post hooks
      .build_hooks_run("post", is_dev_build = TRUE, output_level = "none")
      expect_length(readLines("both-log.txt"), 2)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run handles dev.hooks with multiple hooks per stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('log.txt')) readLines('log.txt') else character(0)",
          "writeLines(c(log, 'hook1'), 'log.txt')"
        ),
        "hooks/hook1.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('log.txt')) readLines('log.txt') else character(0)",
          "writeLines(c(log, 'hook2'), 'log.txt')"
        ),
        "hooks/hook2.R"
      )

      # Configure dev.hooks with multiple pre hooks
      yaml::write_yaml(
        list(
          dev = list(
            hooks = list(
              pre = c("hooks/hook1.R", "hooks/hook2.R")
            )
          )
        ),
        "_projr.yml"
      )

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = TRUE, output_level = "none")

      # Both hooks should execute in order
      log <- readLines("log.txt")
      expect_identical(log, c("hook1", "hook2"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run handles no dev.hooks gracefully", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # No dev.hooks configured
      result <- .build_hooks_run("pre", is_dev_build = TRUE, output_level = "none")
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_pre_hooks_run and .build_post_hooks_run tests
# =============================================================================

test_that(".build_pre_hooks_run calls .build_hooks_run with pre stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('pre', 'stage-log.txt')"),
        "hooks/pre.R"
      )

      # Configure hook
      projr_yml_hooks_add_pre("hooks/pre.R")

      # Run pre hooks via wrapper
      .build_pre_hooks_run(is_dev_build = FALSE, output_level = "none")

      # Verify execution
      expect_true(file.exists("stage-log.txt"))
      expect_identical(readLines("stage-log.txt"), "pre")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_post_hooks_run calls .build_hooks_run with post stage", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('post', 'stage-log.txt')"),
        "hooks/post.R"
      )

      # Configure hook
      projr_yml_hooks_add_post("hooks/post.R")

      # Run post hooks via wrapper
      .build_post_hooks_run(is_dev_build = FALSE, output_level = "none")

      # Verify execution
      expect_true(file.exists("stage-log.txt"))
      expect_identical(readLines("stage-log.txt"), "post")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_pre_hooks_run passes is_dev_build parameter correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('dev-pre', 'dev-log.txt')"),
        "hooks/dev-pre.R"
      )

      # Configure dev.hooks
      yaml::write_yaml(
        list(
          dev = list(
            hooks = list(
              pre = "hooks/dev-pre.R"
            )
          )
        ),
        "_projr.yml"
      )

      # Run pre hooks as dev build
      .build_pre_hooks_run(is_dev_build = TRUE, output_level = "none")

      # Verify dev hook executed
      expect_true(file.exists("dev-log.txt"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# .build_hooks_run_title tests (legacy format)
# =============================================================================

test_that(".build_hooks_run_title executes legacy format hooks", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('legacy', 'legacy-log.txt')"),
        "hooks/legacy.R"
      )

      # Create legacy format hook object
      hook_obj <- list(
        stage = "pre",
        path = "hooks/legacy.R"
      )

      # Run legacy hook
      .build_hooks_run_title(hook_obj, stage = "pre", output_level = "none")

      # Verify execution
      expect_true(file.exists("legacy-log.txt"))
      expect_identical(readLines("legacy-log.txt"), "legacy")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run_title skips when stage doesn't match", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c("writeLines('should-not-run', 'skip-log.txt')"),
        "hooks/skip.R"
      )

      # Create legacy format hook object with stage="pre"
      hook_obj <- list(
        stage = "pre",
        path = "hooks/skip.R"
      )

      # Try to run with stage="post" (should not execute)
      result <- .build_hooks_run_title(hook_obj, stage = "post", output_level = "none")

      # Verify hook did not execute
      expect_false(file.exists("skip-log.txt"))
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run_title executes multiple paths in legacy format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('multi-log.txt')) readLines('multi-log.txt') else character(0)",
          "writeLines(c(log, 'hook1'), 'multi-log.txt')"
        ),
        "hooks/multi1.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('multi-log.txt')) readLines('multi-log.txt') else character(0)",
          "writeLines(c(log, 'hook2'), 'multi-log.txt')"
        ),
        "hooks/multi2.R"
      )

      # Create legacy format hook object with multiple paths
      hook_obj <- list(
        stage = "pre",
        path = c("hooks/multi1.R", "hooks/multi2.R")
      )

      # Run legacy hooks
      .build_hooks_run_title(hook_obj, stage = "pre", output_level = "none")

      # Verify both executed
      log <- readLines("multi-log.txt")
      expect_identical(log, c("hook1", "hook2"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Integration tests - hook execution order
# =============================================================================

test_that("hooks execute in correct order: stage-specific then both", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('order-log.txt')) readLines('order-log.txt') else character(0)",
          "writeLines(c(log, 'pre-specific'), 'order-log.txt')"
        ),
        "hooks/pre-specific.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('order-log.txt')) readLines('order-log.txt') else character(0)",
          "writeLines(c(log, 'both'), 'order-log.txt')"
        ),
        "hooks/both.R"
      )

      # Configure hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = "hooks/pre-specific.R",
              both = "hooks/both.R"
            )
          )
        ),
        "_projr.yml"
      )

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")

      # Verify order: pre-specific, then both
      log <- readLines("order-log.txt")
      expect_identical(log, c("pre-specific", "both"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("multiple hooks in same stage execute in YAML order", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hooks
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "log <- if(file.exists('order-log.txt')) readLines('order-log.txt') else character(0)",
          "writeLines(c(log, 'first'), 'order-log.txt')"
        ),
        "hooks/first.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('order-log.txt')) readLines('order-log.txt') else character(0)",
          "writeLines(c(log, 'second'), 'order-log.txt')"
        ),
        "hooks/second.R"
      )
      writeLines(
        c(
          "log <- if(file.exists('order-log.txt')) readLines('order-log.txt') else character(0)",
          "writeLines(c(log, 'third'), 'order-log.txt')"
        ),
        "hooks/third.R"
      )

      # Configure hooks in specific order
      projr_yml_hooks_add_pre(c("hooks/first.R", "hooks/second.R", "hooks/third.R"))

      # Run pre hooks
      .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")

      # Verify order matches YAML
      log <- readLines("order-log.txt")
      expect_identical(log, c("first", "second", "third"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

# =============================================================================
# Edge cases and error handling
# =============================================================================

test_that(".build_hooks_run handles empty hooks list gracefully", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Configure empty hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list()
          )
        ),
        "_projr.yml"
      )

      # Should not error
      result <- .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".build_hooks_run handles NULL in hooks gracefully", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Configure NULL hooks
      yaml::write_yaml(
        list(
          build = list(
            hooks = list(
              pre = NULL,
              post = NULL
            )
          )
        ),
        "_projr.yml"
      )

      # Should not error
      result <- .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")
      expect_true(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("hooks can access project functions and paths", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Create hook that uses projr functions
      dir.create("hooks", showWarnings = FALSE)
      writeLines(
        c(
          "output_path <- projr_path_get_dir('output', safe = FALSE)",
          "writeLines(output_path, 'path-log.txt')"
        ),
        "hooks/path-hook.R"
      )

      # Configure and run hook
      projr_yml_hooks_add_pre("hooks/path-hook.R")
      .build_hooks_run("pre", is_dev_build = FALSE, output_level = "none")

      # Verify hook could access projr functions
      expect_true(file.exists("path-log.txt"))
      path_from_hook <- readLines("path-log.txt")
      expect_true(grepl("_output", path_from_hook))
    },
    quiet = TRUE,
    force = TRUE
  )
})
