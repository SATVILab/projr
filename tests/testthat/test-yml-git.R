# Tests for R/yml-git.R functions
# These tests cover the yml git configuration functions

# =============================================================================
# projr_yml_git_set tests - basic usage
# =============================================================================

test_that("projr_yml_git_set works with all parameter", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to TRUE
      projr_yml_git_set(all = TRUE)
      expect_true(.yml_git_get_commit("default"))
      expect_true(.yml_git_get_add_untracked("default"))
      expect_true(.yml_git_get_push("default"))

      # Set all to FALSE
      projr_yml_git_set(all = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_commit("default"))
      expect_false(.yml_git_get_add_untracked("default"))
      expect_false(.yml_git_get_push("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set works with individual parameters", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set commit to FALSE
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)
      yml_git <- .yml_git_get("default")
      expect_false(yml_git[["commit"]])

      # Set add_untracked to FALSE
      projr_yml_git_set(add_untracked = FALSE, simplify_identical = FALSE)
      yml_git <- .yml_git_get("default")
      expect_false(yml_git[["add-untracked"]])

      # Set push to FALSE
      projr_yml_git_set(push = FALSE, simplify_identical = FALSE)
      yml_git <- .yml_git_get("default")
      expect_false(yml_git[["push"]])
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set works with multiple parameters at once", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set commit to TRUE, push to FALSE, add_untracked to TRUE
      projr_yml_git_set(
        commit = TRUE,
        push = FALSE,
        add_untracked = TRUE,
        simplify_identical = FALSE
      )
      expect_true(.yml_git_get_commit("default"))
      expect_true(.yml_git_get_add_untracked("default"))
      expect_false(.yml_git_get_push("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_git_set tests - simplify_identical behavior
# =============================================================================

test_that("projr_yml_git_set simplifies when all values are TRUE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to TRUE with simplify_identical = TRUE
      projr_yml_git_set(all = TRUE, simplify_identical = TRUE)
      yml_git <- .yml_git_get("default")

      # Should be simplified to TRUE or NULL (due to simplify_default)
      expect_true(is.null(yml_git) || isTRUE(yml_git))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set simplifies when all values are FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to FALSE with simplify_identical = TRUE
      projr_yml_git_set(all = FALSE, simplify_identical = TRUE, simplify_default = FALSE)
      yml_git <- .yml_git_get("default")

      # Should be simplified to FALSE
      expect_false(yml_git)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set does not simplify when simplify_identical = FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to TRUE with simplify_identical = FALSE
      projr_yml_git_set(all = TRUE, simplify_identical = FALSE, simplify_default = FALSE)
      yml_git <- .yml_git_get("default")

      # Should be a list with individual values
      expect_true(is.list(yml_git))
      expect_true(yml_git[["commit"]])
      expect_true(yml_git[["add-untracked"]])
      expect_true(yml_git[["push"]])
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_git_set tests - simplify_default behavior
# =============================================================================

test_that("projr_yml_git_set removes settings when they are default (TRUE)", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to TRUE with simplify_default = TRUE
      projr_yml_git_set(all = TRUE, simplify_default = TRUE)
      yml_git <- .yml_git_get("default")

      # Should be NULL (removed because it's the default)
      expect_true(is.null(yml_git))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set keeps settings when simplify_default = FALSE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to TRUE with simplify_default = FALSE
      projr_yml_git_set(all = TRUE, simplify_default = FALSE, simplify_identical = TRUE)
      yml_git <- .yml_git_get("default")

      # Should be TRUE (not removed)
      expect_true(isTRUE(yml_git))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_git_set tests - profile support
# =============================================================================

test_that("projr_yml_git_set works with different profiles", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Check default profile starts with default values (NULL/TRUE)
      expect_true(.yml_git_get_commit("default"))

      # Create a test profile
      projr_profile_create("test-profile")

      # Set git options in test profile
      projr_yml_git_set(
        commit = FALSE,
        push = FALSE,
        profile = "test-profile",
        simplify_identical = FALSE
      )

      # Check it's set in the test profile
      expect_false(.yml_git_get_commit("test-profile"))
      expect_false(.yml_git_get_push("test-profile"))

      # Check default profile is still unchanged (should be default TRUE)
      expect_true(.yml_git_get_commit("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# projr_yml_git_set_default tests
# =============================================================================

test_that("projr_yml_git_set_default sets all options to TRUE", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set all to FALSE first
      projr_yml_git_set(all = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_commit("default"))

      # Reset to default
      projr_yml_git_set_default()

      # All should be TRUE and simplified/removed
      yml_git <- .yml_git_get("default")
      expect_true(is.null(yml_git) || isTRUE(yml_git))
      expect_true(.yml_git_get_commit("default"))
      expect_true(.yml_git_get_add_untracked("default"))
      expect_true(.yml_git_get_push("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# Getter function tests
# =============================================================================

test_that(".yml_git_get_commit returns correct values", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Default should be TRUE
      expect_true(.yml_git_get_commit("default"))

      # After setting to FALSE
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_commit("default"))

      # After setting back to TRUE
      projr_yml_git_set(commit = TRUE)
      expect_true(.yml_git_get_commit("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_git_get_add_untracked returns correct values with override", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Default should be TRUE
      expect_true(.yml_git_get_add_untracked("default"))

      # Set add_untracked to TRUE explicitly
      projr_yml_git_set(add_untracked = TRUE, simplify_default = FALSE)
      expect_true(.yml_git_get_add_untracked("default"))

      # Set commit to FALSE - add_untracked should be overridden to FALSE
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_add_untracked("default"))

      # Even if add_untracked is set to TRUE, it should be FALSE when commit is FALSE
      projr_yml_git_set(
        commit = FALSE,
        add_untracked = TRUE,
        simplify_identical = FALSE
      )
      expect_false(.yml_git_get_add_untracked("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_git_get_push returns correct values with override", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Note: .test_setup_project sets push=FALSE by default
      # So we first set everything back to defaults
      projr_yml_git_set(all = TRUE)

      # Now default should be TRUE
      expect_true(.yml_git_get_push("default"))

      # Set push to FALSE (commit is still TRUE by default)
      projr_yml_git_set(push = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_push("default"))

      # Set commit to FALSE - push should be overridden to FALSE
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_push("default"))

      # Even if push is set to TRUE, it should be FALSE when commit is FALSE
      projr_yml_git_set(
        commit = FALSE,
        push = TRUE,
        simplify_identical = FALSE
      )
      # Push should be FALSE due to override logic
      expect_false(.yml_git_get_push("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_git_get returns correct structure", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Default should be NULL (no git section in default _projr.yml)
      # However, .test_setup_project modifies git settings to set push=FALSE
      # So we check what the actual initial state is
      yml_git <- .yml_git_get("default")
      # After .test_setup_project, git should have push set to FALSE
      expect_true(is.list(yml_git) || is.null(yml_git))

      # Set some specific values
      projr_yml_git_set(
        commit = FALSE,
        push = TRUE,
        simplify_identical = FALSE,
        simplify_default = FALSE
      )
      yml_git <- .yml_git_get("default")
      expect_true(is.list(yml_git))
      expect_false(yml_git[["commit"]])
      expect_true(yml_git[["push"]])

      # After setting all to FALSE and simplifying
      projr_yml_git_set(all = FALSE, simplify_identical = TRUE, simplify_default = FALSE)
      yml_git <- .yml_git_get("default")
      expect_false(yml_git)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# Input validation tests
# =============================================================================

test_that("projr_yml_git_set validates inputs correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Invalid all parameter (not logical)
      expect_error(
        projr_yml_git_set(all = "yes"),
        "must be.*flag"
      )

      # Invalid commit parameter (not logical)
      expect_error(
        projr_yml_git_set(commit = "yes"),
        "must be.*flag"
      )

      # Invalid add_untracked parameter (not logical)
      expect_error(
        projr_yml_git_set(add_untracked = 1),
        "must be.*flag"
      )

      # Invalid push parameter (not logical)
      expect_error(
        projr_yml_git_set(push = c(TRUE, FALSE)),
        "must be.*flag"
      )

      # Invalid simplify_identical parameter
      expect_error(
        projr_yml_git_set(commit = TRUE, simplify_identical = "yes"),
        "must be.*flag"
      )

      # Invalid simplify_default parameter
      expect_error(
        projr_yml_git_set(commit = TRUE, simplify_default = 1),
        "must be.*flag"
      )

      # Invalid profile parameter (not string)
      expect_error(
        projr_yml_git_set(commit = TRUE, profile = c("a", "b")),
        "must be.*string"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set handles NULL parameters correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Note: .test_setup_project sets push=FALSE by default
      # Reset to defaults first
      projr_yml_git_set(all = TRUE)

      # Calling with all = NULL and all individual params = NULL does nothing
      # This should not error - it just leaves settings unchanged
      expect_silent(
        projr_yml_git_set(all = NULL, commit = NULL, add_untracked = NULL, push = NULL)
      )

      # Verify settings are still defaults (TRUE)
      expect_true(.yml_git_get_commit("default"))
      expect_true(.yml_git_get_add_untracked("default"))
      expect_true(.yml_git_get_push("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# Edge cases and integration tests
# =============================================================================

test_that("projr_yml_git_set preserves other build settings", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set some other build settings
      yml <- .yml_get("default")
      yml[["build"]][["some_other_setting"]] <- "test_value"
      .yml_set(yml, "default")

      # Modify git settings
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)

      # Check that other build settings are preserved
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["some_other_setting"]], "test_value")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_git_set works when called multiple times", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # First call
      projr_yml_git_set(commit = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_commit("default"))

      # Second call - different parameter
      projr_yml_git_set(push = FALSE, simplify_identical = FALSE)
      expect_false(.yml_git_get_push("default"))
      expect_false(.yml_git_get_commit("default")) # Should still be FALSE

      # Third call - overwrite
      projr_yml_git_set(commit = TRUE, simplify_identical = FALSE)
      expect_true(.yml_git_get_commit("default"))
      expect_false(.yml_git_get_push("default")) # Should still be FALSE
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("yml git settings work with mixed TRUE/FALSE values", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set mixed values - should not simplify
      projr_yml_git_set(
        commit = TRUE,
        add_untracked = FALSE,
        push = TRUE,
        simplify_identical = TRUE,
        simplify_default = FALSE
      )

      yml_git <- .yml_git_get("default")
      expect_true(is.list(yml_git))
      expect_true(yml_git[["commit"]])
      expect_false(yml_git[["add-untracked"]])
      expect_true(yml_git[["push"]])
    },
    force = TRUE,
    quiet = TRUE
  )
})
