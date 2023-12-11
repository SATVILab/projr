test_that(".projr_yml_git_ functions work works", {
  # setup
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = FALSE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      projr_yml_git_set_default()
      expect_identical(
        .projr_yml_git_get("default"),
        NULL
      )

      # set one to FALSE
      projr_yml_git_set(push = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(push = FALSE)
      )
      # set two to FALSE
      projr_yml_git_set(commit = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(commit = FALSE, push = FALSE)
      )
      # set three to FALSE, no simplify identical
      projr_yml_git_set(add_untracked = FALSE, simplify_identical = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        list(commit = FALSE, `add-untracked` = FALSE, push = FALSE)
      )
      # set three to FALSE, simplify identical
      projr_yml_git_set(add_untracked = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        FALSE
      )
      # set three to TRUE, no simplify default
      projr_yml_git_set(all = TRUE, simplify_default = FALSE)
      expect_identical(
        .projr_yml_git_get("default"),
        TRUE
      )
      # set three to TRUE, simplify default
      projr_yml_git_set(all = TRUE)
      expect_identical(
        .projr_yml_git_get("default"),
        NULL
      )
    }
  )
})
