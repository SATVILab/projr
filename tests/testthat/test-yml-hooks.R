# Tests for R/yml-hooks.R functions
# These tests cover the yml hooks configuration functions

# =============================================================================
# projr_yml_hooks_add tests
# =============================================================================

test_that("projr_yml_hooks_add works with basic usage", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add a pre hook
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre.R")

      # Add a post hook
      projr_yml_hooks_add("hooks/post.R", stage = "post")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["post"]], "hooks/post.R")

      # Add a both hook
      projr_yml_hooks_add("hooks/both.R", stage = "both")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["both"]], "hooks/both.R")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_add works with multiple paths", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add multiple pre hooks
      projr_yml_hooks_add(c("hooks/pre1.R", "hooks/pre2.R"), stage = "pre")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], c("hooks/pre1.R", "hooks/pre2.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_add overwrite parameter works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add initial hooks
      projr_yml_hooks_add("hooks/pre1.R", stage = "pre")

      # Overwrite with new hooks (default behavior)
      projr_yml_hooks_add("hooks/pre2.R", stage = "pre", overwrite = TRUE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre2.R")

      # Append without overwriting
      projr_yml_hooks_add("hooks/pre3.R", stage = "pre", overwrite = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], c("hooks/pre2.R", "hooks/pre3.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_add prevents duplicates when appending", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add initial hooks
      projr_yml_hooks_add("hooks/pre1.R", stage = "pre")

      # Try to append the same hook (should not create duplicate)
      projr_yml_hooks_add("hooks/pre1.R", stage = "pre", overwrite = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre1.R")

      # Append multiple paths with some duplicates
      projr_yml_hooks_add(c("hooks/pre1.R", "hooks/pre2.R"), stage = "pre", overwrite = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], c("hooks/pre1.R", "hooks/pre2.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_add validates inputs", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Invalid path (not character)
      expect_error(
        projr_yml_hooks_add(123, stage = "pre"),
        "character"
      )

      # Invalid stage
      expect_error(
        projr_yml_hooks_add("hooks/pre.R", stage = "invalid"),
        "one of"
      )

      # Invalid overwrite (not logical)
      expect_error(
        projr_yml_hooks_add("hooks/pre.R", stage = "pre", overwrite = "yes"),
        "non-NA flag"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_hooks_add_pre and projr_yml_hooks_add_post tests
# =============================================================================

test_that("projr_yml_hooks_add_pre works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add pre hook using convenience function
      projr_yml_hooks_add_pre("hooks/pre.R")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre.R")

      # Test overwrite parameter
      projr_yml_hooks_add_pre("hooks/pre2.R", overwrite = TRUE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre2.R")

      # Test append
      projr_yml_hooks_add_pre("hooks/pre3.R", overwrite = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], c("hooks/pre2.R", "hooks/pre3.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_add_post works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add post hook using convenience function
      projr_yml_hooks_add_post("hooks/post.R")
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["post"]], "hooks/post.R")

      # Test overwrite parameter
      projr_yml_hooks_add_post("hooks/post2.R", overwrite = TRUE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["post"]], "hooks/post2.R")

      # Test append
      projr_yml_hooks_add_post("hooks/post3.R", overwrite = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["post"]], c("hooks/post2.R", "hooks/post3.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_hooks_rm_all tests
# =============================================================================

test_that("projr_yml_hooks_rm_all removes all hooks", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add various hooks
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")
      projr_yml_hooks_add("hooks/post.R", stage = "post")
      projr_yml_hooks_add("hooks/both.R", stage = "both")

      yml <- .yml_get("default")
      expect_true(!is.null(yml[["build"]][["hooks"]]))

      # Remove all hooks
      projr_yml_hooks_rm_all()

      yml <- .yml_get("default")
      expect_true(is.null(yml[["build"]][["hooks"]]))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_hooks_rm_all handles no existing hooks gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Remove hooks when none exist (should not error)
      result <- projr_yml_hooks_rm_all()

      # Should return FALSE when nothing to remove
      expect_false(result)

      yml <- .yml_get("default")
      expect_true(is.null(yml[["build"]][["hooks"]]))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# .yml_hooks_check tests
# =============================================================================

test_that(".yml_hooks_check validates all parameters", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Valid inputs should not error
  expect_silent(.yml_hooks_check("hooks/test.R", "pre", TRUE, "default"))
  expect_silent(.yml_hooks_check(c("hooks/pre.R", "hooks/post.R"), "both", FALSE, "default"))

  # Invalid path (not character)
  expect_error(
    .yml_hooks_check(123, "pre", TRUE, "default"),
    "character"
  )

  # Invalid stage (not in allowed values)
  expect_error(
    .yml_hooks_check("hooks/test.R", "invalid", TRUE, "default"),
    "one of"
  )

  # Invalid overwrite (not logical)
  expect_error(
    .yml_hooks_check("hooks/test.R", "pre", "yes", "default"),
    "non-NA flag"
  )
})

# =============================================================================
# .yml_hooks_add tests
# =============================================================================

test_that(".yml_hooks_add creates new hook list when none exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Ensure no hooks exist
      yml <- .yml_get("default")
      yml[["build"]][["hooks"]] <- NULL
      .yml_set(yml, "default")

      # Add hook (internal function)
      result <- .yml_hooks_add("hooks/pre.R", "pre", "default", overwrite = TRUE)

      expect_equal(result[["pre"]], "hooks/pre.R")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_hooks_add handles overwrite correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set initial hooks
      projr_yml_hooks_add("hooks/pre1.R", stage = "pre")

      # Overwrite with new path (then set it)
      result <- .yml_hooks_add("hooks/pre2.R", "pre", "default", overwrite = TRUE)
      .yml_hooks_set(result, "default")
      expect_equal(result[["pre"]], "hooks/pre2.R")

      # Append without overwrite
      result <- .yml_hooks_add("hooks/pre3.R", "pre", "default", overwrite = FALSE)
      expect_equal(result[["pre"]], c("hooks/pre2.R", "hooks/pre3.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_hooks_add removes duplicates when appending", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set initial hooks
      projr_yml_hooks_add("hooks/pre1.R", stage = "pre")

      # Try to append duplicate
      result <- .yml_hooks_add("hooks/pre1.R", "pre", "default", overwrite = FALSE)
      expect_equal(result[["pre"]], "hooks/pre1.R")

      # Append new and duplicate paths
      result <- .yml_hooks_add(c("hooks/pre1.R", "hooks/pre2.R"), "pre", "default", overwrite = FALSE)
      expect_equal(result[["pre"]], c("hooks/pre1.R", "hooks/pre2.R"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# .yml_hooks_get tests
# =============================================================================

test_that(".yml_hooks_get retrieves hooks from YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add hooks
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")
      projr_yml_hooks_add("hooks/post.R", stage = "post")

      # Retrieve hooks
      hooks <- .yml_hooks_get("default")
      expect_equal(hooks[["pre"]], "hooks/pre.R")
      expect_equal(hooks[["post"]], "hooks/post.R")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_hooks_get returns NULL when no hooks exist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Ensure no hooks exist
      yml <- .yml_get("default")
      yml[["build"]][["hooks"]] <- NULL
      .yml_set(yml, "default")

      # Retrieve hooks (should be NULL or empty)
      hooks <- .yml_hooks_get("default")
      expect_true(is.null(hooks) || length(hooks) == 0)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# .yml_hooks_set tests
# =============================================================================

test_that(".yml_hooks_set writes hooks to YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Create hook structure
      hooks <- list(
        pre = c("hooks/pre1.R", "hooks/pre2.R"),
        post = "hooks/post.R"
      )

      # Set hooks
      .yml_hooks_set(hooks, "default")

      # Verify they were written
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], c("hooks/pre1.R", "hooks/pre2.R"))
      expect_equal(yml[["build"]][["hooks"]][["post"]], "hooks/post.R")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_hooks_set can set NULL to remove hooks", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add hooks first
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")

      # Remove by setting NULL
      .yml_hooks_set(NULL, "default")

      yml <- .yml_get("default")
      expect_true(is.null(yml[["build"]][["hooks"]]))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# Profile support tests
# =============================================================================

test_that("hooks functions work with custom profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Create a custom profile
      profile_name <- "test-profile"
      projr_profile_create(profile_name)

      # Add hooks to custom profile
      projr_yml_hooks_add("hooks/pre.R", stage = "pre", profile = profile_name)

      # Verify hooks in custom profile
      yml <- .yml_get(profile_name)
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre.R")

      # Verify default profile unchanged
      yml_default <- .yml_get("default")
      expect_true(is.null(yml_default[["build"]][["hooks"]]))

      # Remove hooks from custom profile
      projr_yml_hooks_rm_all(profile = profile_name)
      yml <- .yml_get(profile_name)
      expect_true(is.null(yml[["build"]][["hooks"]]))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# Edge cases and integration tests
# =============================================================================

test_that("hooks work with all three stages simultaneously", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add hooks to all stages
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")
      projr_yml_hooks_add("hooks/post.R", stage = "post")
      projr_yml_hooks_add("hooks/both.R", stage = "both")

      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["hooks"]][["pre"]], "hooks/pre.R")
      expect_equal(yml[["build"]][["hooks"]][["post"]], "hooks/post.R")
      expect_equal(yml[["build"]][["hooks"]][["both"]], "hooks/both.R")

      # Verify .yml_hooks_get_stage retrieves correct hooks
      pre_hooks <- .yml_hooks_get_stage("pre", "default")
      expect_true("hooks/pre.R" %in% pre_hooks)
      expect_true("hooks/both.R" %in% pre_hooks)

      post_hooks <- .yml_hooks_get_stage("post", "default")
      expect_true("hooks/post.R" %in% post_hooks)
      expect_true("hooks/both.R" %in% post_hooks)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_check validates hook structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Valid hooks should pass
      projr_yml_hooks_add("hooks/pre.R", stage = "pre")
      expect_true(projr_yml_check())

      # Invalid hook structure should fail
      yml <- .yml_get("default")
      yml[["build"]][["hooks"]][["pre"]] <- list(name = "invalid")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "character vector")

      # Invalid stage name should fail
      yml <- .yml_get("default")
      yml[["build"]][["hooks"]][["invalid_stage"]] <- "hooks/test.R"
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "invalid")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("hooks functions handle empty character vectors", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Adding empty character vector should work
      # (though might not be useful in practice)
      expect_error(
        projr_yml_hooks_add(character(0), stage = "pre"),
        "character"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})
