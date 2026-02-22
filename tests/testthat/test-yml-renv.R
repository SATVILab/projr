# Tests for R/yml-renv.R functions
# These tests cover the yml renv configuration functions

# =============================================================================
# projr_yml_renv_set tests - basic usage
# =============================================================================

test_that("projr_yml_renv_set sets renv to FALSE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set renv to FALSE
      projr_yml_renv_set(FALSE)
      expect_false(.yml_renv_get("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_renv_set sets renv to TRUE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # First set to FALSE
      projr_yml_renv_set(FALSE)
      expect_false(.yml_renv_get("default"))

      # Then set to TRUE (with simplify_default = FALSE to keep it in YAML)
      projr_yml_renv_set(TRUE, simplify_default = FALSE)
      expect_true(.yml_renv_get("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_renv_set default is TRUE when not set", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Default should be TRUE
      expect_true(.yml_renv_get("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_renv_set removes setting when set to TRUE with simplify_default", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set to FALSE
      projr_yml_renv_set(FALSE)
      yml_build <- .yml_build_get("default")
      expect_false(is.null(yml_build[["renv"]]))

      # Set to TRUE with simplify_default = TRUE (default)
      projr_yml_renv_set(TRUE, simplify_default = TRUE)
      yml_build <- .yml_build_get("default")
      expect_true(is.null(yml_build[["renv"]]))

      # Should still return TRUE as default
      expect_true(.yml_renv_get("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# .build_renv_snapshot_check tests with yml setting
# =============================================================================

test_that(".build_renv_snapshot_check respects yml renv setting", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # When renv is TRUE (default), should check other conditions
      # In test environment, should return FALSE due to .is_test()
      result <- .build_renv_snapshot_check(output_run = TRUE)
      expect_false(result)

      # Set renv to FALSE - should always return FALSE
      projr_yml_renv_set(FALSE)
      result <- .build_renv_snapshot_check(output_run = TRUE)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

# =============================================================================
# yml check validation tests
# =============================================================================

test_that("projr_yml_check accepts valid renv setting", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Set valid renv value
      projr_yml_renv_set(FALSE)
      expect_true(projr_yml_check("default"))

      projr_yml_renv_set(TRUE, simplify_default = FALSE)
      expect_true(projr_yml_check("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_check rejects invalid renv setting", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Manually set invalid value
      yml_projr <- .yml_get("default")
      yml_projr[["build"]][["renv"]] <- "invalid"
      .yml_set(yml_projr, "default")

      expect_error(projr_yml_check("default"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
