test_that("build restrictions work in actual build", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize project
      .init()

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Create a simple Rmd file to build
      writeLines(c("---", "title: Test", "---", "", "Test content"), "test-script.Rmd")

      # Set up build scripts
      projr_yml_git_set(commit = FALSE, push = FALSE)
      yml <- .yml_get("default")
      yml[["build"]][["scripts"]] <- "test-script.Rmd"
      .yml_set(yml, "default")

      # Dev builds are not restricted (only output builds are)
      # So all these should work regardless of restrictions
      expect_no_error(projr_build_dev(file = "test-script.Rmd"))

      # Restrict to different branch - dev build still works
      projr_yml_restrictions_set(branch = "different-branch")
      expect_no_error(projr_build_dev(file = "test-script.Rmd"))

      # Remove restrictions
      projr_yml_restrictions_set(branch = TRUE)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("branch restriction check logic works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = FALSE)

  usethis::with_project(
    path = dir_test,
    code = {
      .init()

      # Get current branch
      current_branch <- .git_branch_get()
      expect_true(!is.null(current_branch))

      # Test 1: No restrictions - check should pass
      expect_silent(.build_check_branch_restriction())

      # Test 2: Restrict to current branch - check should pass
      projr_yml_restrictions_set(branch = current_branch)
      expect_silent(.build_check_branch_restriction())

      # Test 3: Restrict to different branch - check should fail
      projr_yml_restrictions_set(branch = "different-branch")
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted to specific branches"
      )

      # Test 4: Allow on multiple branches including current
      projr_yml_restrictions_set(branch = c("main", "dev", current_branch))
      expect_silent(.build_check_branch_restriction())

      # Test 5: Restrict all branches (FALSE/empty)
      projr_yml_restrictions_set(branch = FALSE)
      expect_error(
        .build_check_branch_restriction(),
        "Builds are restricted on all branches"
      )

      # Test 6: Remove restrictions - check should pass again
      projr_yml_restrictions_set(branch = TRUE)
      expect_silent(.build_check_branch_restriction())
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("build restrictions only apply to output builds", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize project
      .init()

      # Get current branch
      current_branch <- .git_branch_get()

      # Create a simple Rmd file to build
      writeLines(c("---", "title: Test", "---", "", "Test content"), "test-script.Rmd")

      # Set up git to not commit or push
      projr_yml_git_set(commit = FALSE, push = FALSE)

      # Restrict to different branch
      projr_yml_restrictions_set(branch = "different-branch")

      # Dev builds should still work (restrictions only apply to output_run)
      expect_no_error(projr_build_dev(file = "test-script.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )
})
