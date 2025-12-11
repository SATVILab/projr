test_that(".yml_restrictions_check_config validates restrictions structure", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Valid restrictions with TRUE should pass
      yml <- .yml_get("default")
      yml[["build"]][["restrictions"]] <- list(branch = TRUE)
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Valid restrictions with FALSE should pass
      yml[["build"]][["restrictions"]] <- list(branch = FALSE)
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Valid restrictions with character vector should pass
      yml[["build"]][["restrictions"]] <- list(branch = c("main", "dev"))
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Valid restrictions with empty list (YAML representation) should pass
      yml[["build"]][["restrictions"]] <- list(branch = list())
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # No restrictions (NULL) should pass
      yml[["build"]][["restrictions"]] <- NULL
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Invalid restrictions (wrong type) should fail
      yml[["build"]][["restrictions"]] <- list(branch = 123)
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "logical or character")

      # Invalid restrictions (invalid key) should fail
      yml[["build"]][["restrictions"]] <- list(invalid_key = TRUE)
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "invalid_key")

      # Valid not_behind restriction should pass
      yml[["build"]][["restrictions"]] <- list(not_behind = TRUE)
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      yml[["build"]][["restrictions"]] <- list(not_behind = FALSE)
      .yml_set(yml, "default")
      expect_true(projr_yml_check())

      # Invalid not_behind (non-logical) should fail
      yml[["build"]][["restrictions"]] <- list(not_behind = "yes")
      .yml_set(yml, "default")
      expect_error(projr_yml_check())

      # Valid combination of restrictions should pass
      yml[["build"]][["restrictions"]] <- list(branch = "main", not_behind = FALSE)
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_restrictions_set works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set branch to TRUE (no restrictions)
      projr_yml_restrictions_set(branch = TRUE)
      yml <- .yml_get("default")
      # TRUE means no restrictions, so section should be removed or NULL
      expect_true(is.null(yml[["build"]][["restrictions"]]))

      # Set branch to character vector
      projr_yml_restrictions_set(branch = c("main", "dev"))
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["restrictions"]][["branch"]], c("main", "dev"))

      # Set branch to FALSE (restrict all branches)
      projr_yml_restrictions_set(branch = FALSE)
      yml <- .yml_get("default")
      # YAML serializes empty character vector as empty list
      expect_equal(yml[["build"]][["restrictions"]][["branch"]], list())

      # Set branch back to TRUE to remove restrictions
      projr_yml_restrictions_set(branch = TRUE)
      yml <- .yml_get("default")
      expect_true(is.null(yml[["build"]][["restrictions"]]))

      # Test not_behind setting
      projr_yml_restrictions_set(not_behind = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["restrictions"]][["not_behind"]], FALSE)

      # Set not_behind to TRUE (default, should remove)
      projr_yml_restrictions_set(not_behind = TRUE)
      yml <- .yml_get("default")
      expect_true(is.null(yml[["build"]][["restrictions"]]))

      # Test setting both branch and not_behind
      projr_yml_restrictions_set(branch = "main", not_behind = FALSE)
      yml <- .yml_get("default")
      expect_equal(yml[["build"]][["restrictions"]][["branch"]], "main")
      expect_equal(yml[["build"]][["restrictions"]][["not_behind"]], FALSE)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_restrictions_get_branch and get_not_behind return correct defaults", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Default should be TRUE (no restrictions)
      expect_true(.yml_restrictions_get_branch("default"))
      expect_true(.yml_restrictions_get_not_behind("default"))

      # After setting to character vector
      projr_yml_restrictions_set(branch = c("main"))
      expect_equal(.yml_restrictions_get_branch("default"), c("main"))

      # After setting to FALSE
      projr_yml_restrictions_set(branch = FALSE)
      expect_equal(.yml_restrictions_get_branch("default"), character(0))

      # Test not_behind getter
      projr_yml_restrictions_set(not_behind = FALSE)
      expect_false(.yml_restrictions_get_not_behind("default"))

      # After setting back to TRUE (default)
      projr_yml_restrictions_set(not_behind = TRUE)
      expect_true(.yml_restrictions_get_not_behind("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_check_branch_restriction works correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # No restrictions - should pass
      expect_silent(.build_check_branch_restriction())

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Restrict to different branch - should fail
      projr_yml_restrictions_set(branch = c("different-branch"))
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted to specific branches"
      )

      # Restrict to current branch - should pass
      projr_yml_restrictions_set(branch = current_branch)
      expect_silent(.build_check_branch_restriction())

      # Restrict to multiple branches including current - should pass
      projr_yml_restrictions_set(branch = c("other-branch", current_branch))
      expect_silent(.build_check_branch_restriction())

      # Restrict all branches (FALSE) - should fail
      projr_yml_restrictions_set(branch = FALSE)
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted on all branches"
      )

      # No restrictions (TRUE) - should pass
      projr_yml_restrictions_set(branch = TRUE)
      expect_silent(.build_check_branch_restriction())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".build_check_branch_restriction enforces logical FALSE from YAML", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Manually set branch to logical FALSE in YAML (simulating direct YAML edit)
      yml <- .yml_get("default")
      yml[["build"]][["restrictions"]] <- list(branch = FALSE)
      .yml_set(yml, "default")

      # Verify getter returns FALSE
      expect_false(.yml_restrictions_get_branch("default"))

      # Should fail with restriction error
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted on all branches"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})
