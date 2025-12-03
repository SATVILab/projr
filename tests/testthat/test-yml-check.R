test_that(".yml_metadata_check validates version-format", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Valid version format should pass
      yml <- .yml_get("default")
      yml[["metadata"]] <- list("version-format" = "major.minor.patch-dev")
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      # Invalid version format should fail
      yml[["metadata"]] <- list("version-format" = "invalid.format")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "version_format must be one of")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_scripts_check validates scripts structure", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Valid scripts should pass
      yml <- .yml_get("default")
      yml[["build"]][["scripts"]] <- c("script1.R", "script2.R")
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      # Invalid scripts (not character) should fail
      yml[["build"]][["scripts"]] <- list(name = "script1.R")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "character vector")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_hooks_check_config validates hooks structure", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Valid hooks should pass
      yml <- .yml_get("default")
      yml[["build"]][["hooks"]] <- list(
        pre = c("pre-hook.R"),
        post = c("post-hook.R"),
        both = c("both-hook.R")
      )
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      # Invalid hooks (wrong stage name) should fail
      yml[["build"]][["hooks"]] <- list(
        invalid = c("hook.R")
      )
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "invalid")
      
      # Invalid hooks (not character) should fail
      yml[["build"]][["hooks"]] <- list(
        pre = list(name = "hook.R")
      )
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "character vector")
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_cite_check_config validates cite structure", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  
  usethis::with_project(
    path = dir_test,
    code = {
      .init()
      
      # Valid cite (logical) should pass
      yml <- .yml_get("default")
      yml[["build"]][["cite"]] <- TRUE
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      yml[["build"]][["cite"]] <- FALSE
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      # Valid cite (list) should pass
      yml[["build"]][["cite"]] <- list(
        codemeta = TRUE,
        cff = FALSE,
        `inst-citation` = TRUE
      )
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
      
      # Invalid cite (wrong key) should fail
      yml[["build"]][["cite"]] <- list(
        invalid_key = TRUE
      )
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "invalid_key")
      
      # Invalid cite (non-logical value) should fail
      yml[["build"]][["cite"]] <- list(
        codemeta = "yes"
      )
      .yml_set(yml, "default")
      expect_error(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_yml_check runs all validations", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Add various yml configurations
      yml <- .yml_get("default")
      yml[["metadata"]] <- list("version-format" = "major.minor.patch-dev")
      yml[["build"]][["scripts"]] <- c("script.R")
      yml[["build"]][["hooks"]] <- list(pre = c("hook.R"))
      yml[["build"]][["cite"]] <- TRUE
      .yml_set(yml, "default")

      # Should pass all checks
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".yml_dir_check rejects directory labels ending in -empty", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Valid directory labels should pass
      yml <- .yml_get("default")
      expect_true(projr_yml_check())

      # Directory label ending in "-empty" should fail
      yml <- .yml_get("default")
      yml[["directories"]][["output-empty"]] <- list(path = "_output_empty")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "-empty")

      # Restore valid config and verify another invalid label also fails
      yml <- .yml_get("default")
      yml[["directories"]][["output-empty"]] <- NULL
      yml[["directories"]][["cache-empty"]] <- list(path = "_cache_empty")
      .yml_set(yml, "default")
      expect_error(projr_yml_check(), "-empty")

      # Labels that contain "empty" but don't end in "-empty" should pass
      yml <- .yml_get("default")
      yml[["directories"]][["cache-empty"]] <- NULL
      yml[["directories"]][["empty-output"]] <- list(path = "_empty_output")
      .yml_set(yml, "default")
      expect_true(projr_yml_check())
    },
    force = TRUE,
    quiet = TRUE
  )
})
