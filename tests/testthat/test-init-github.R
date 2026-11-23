test_that("GitHub initialization skips disabled, missing and already linked repositories", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(project, {
    expect_false(.init_github(FALSE, NULL, NULL, FALSE))
    expect_false(.init_github(TRUE, NULL, NULL, FALSE))

    .git_init()
    gert::git_remote_add("https://github.com/test/repo.git", "origin")
    expect_false(.init_github(TRUE, NULL, NULL, FALSE))
    expect_null(.init_git_github(username = NULL, public = FALSE))
    expect_identical(gert::git_remote_list()$name, "origin")
  })
})

test_that(".init_git_github stops before creating a repository when authentication fails", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  project <- .test_setup_project()
  testthat::local_mocked_bindings(
    .git_gh_check_auth = function(...) FALSE,
    .init_github_impl = function(...) testthat::fail("Attempted to create a repository")
  )
  usethis::with_project(project, {
    expect_error(.init_git_github(username = NULL, public = FALSE), "^$")
  })
})
