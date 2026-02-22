
test_that("projr_init_github works", {
  skip_if(.is_test_cran())
  # skip_if(.is_test_select())
  skip_if(.is_test_lite())
  skip_if(.is_gha())
  skip_if_offline()
  skip_if(!.test_can_modify_github())

  # Start from scratch
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )

  usethis::with_project(
    path = dir_test,
    code = {
      Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
      on.exit(Sys.setenv("R_PKG_TEST_IN_PROGRESS" = "TRUE"), add = TRUE)
      old_gh_pat <- Sys.getenv("GITHUB_PAT")
      on.exit(Sys.setenv("GITHUB_PAT" = old_gh_pat), add = TRUE)
      .set_github_pat_to_orgmiguelrodo()
      result <- projr_init_github(org = "OrgMiguelRodo")
      expect_true(result)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_std_github handles GitHub repo creation scenarios", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with github = FALSE
  dir_test <- .dir_get_tmp_random_path()
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      result <- .init_std_github(FALSE, FALSE, NULL)
      expect_false(result)
    },
    force = TRUE,
    quiet = TRUE
  )

  # Test when no git repo exists
  dir_test <- .dir_get_tmp_random_path()
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      result <- .init_std_github(TRUE, FALSE, NULL)
      expect_false(result)
      # Should not create GitHub repo without local git
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)

  # Test when git repo exists but remote already set
  dir_test <- .dir_get_tmp_random_path()
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git and add a fake remote
      .git_init()
      .test_setup_project_git_config()
      .dep_install_only("gert")
      gert::git_remote_add("https://github.com/test/repo.git", "origin")

      result <- .init_std_github(TRUE, FALSE, NULL)
      expect_false(result)
      # Should not create GitHub repo when remote already exists
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that(".init_git_github does nothing when remote already exists", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  dir_test <- .dir_get_tmp_random_path()
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git and add a remote
      .git_init()
      .test_setup_project_git_config()
      .dep_install_only("gert")
      gert::git_remote_add("https://github.com/test/repo.git", "origin")

      # Test .init_git_github - should do nothing
      result <- .init_git_github(username = NULL, public = FALSE)
      expect_null(result)

      # Verify remote still exists
      remotes <- gert::git_remote_list()
      expect_true(nrow(remotes) > 0)
      expect_true("origin" %in% remotes$name)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".init_git_github stops when auth check fails", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  skip("Skip to avoid warnings in R CMD CHECK - auth functionality tested elsewhere")

  dir_test <- .dir_get_tmp_random_path()
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  .dir_create(dir_test)
  .test_set()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))

  # Temporarily remove GitHub auth
  old_pat <- Sys.getenv("GITHUB_PAT")
  old_token <- Sys.getenv("GITHUB_TOKEN")
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_TOKEN")
  withr::defer({
    if (nzchar(old_pat)) Sys.setenv(GITHUB_PAT = old_pat)
    if (nzchar(old_token)) Sys.setenv(GITHUB_TOKEN = old_token)
  })

  usethis::with_project(
    path = dir_test,
    code = {
      # Initialize git first (no remote)
      .git_init()
      .test_setup_project_git_config()

      # Test .init_git_github - should stop due to auth failure
      # The function calls stop() which produces an empty error message
      result <- tryCatch(
        {
          .init_git_github(
            username = NULL, public = FALSE,
            use_gh_if_available = FALSE,
            use_gitcreds_if_needed = FALSE
          )
          "no_error"
        },
        error = function(e) {
          "error"
        }
      )
      # Should have errored due to missing auth
      expect_identical(result, "error")
    },
    force = TRUE,
    quiet = TRUE
  )
})