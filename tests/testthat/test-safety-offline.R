test_that("GitHub opt-in is required before any network discovery", {
  skip_if(.is_test_select())
  withr::local_envvar(c(
    PROJR_TEST_LIVE_GITHUB = "false",
    GITHUB_PAT = "fixture-token",
    GITHUB_TOKEN = "fixture-token"
  ))
  expect_false(.test_can_modify_github())
  expect_false(.has_internet())
  withr::local_envvar(c(PROJR_TEST_LIVE_GITHUB = "true"))
  expect_true(.test_can_modify_github())
})

test_that("GitHub deletion rejects every target without looking up credentials", {
  skip_if(.is_test_select())
  expect_error(
    .test_remote_host_rm_github(c(user = "fixture-owner", repo = "fixture-repo")),
    "Automatic GitHub repository deletion is disabled"
  )
})

test_that("test projects use the requested prefix in the session temp directory", {
  skip_if(.is_test_select())
  path <- .test_setup_project_dir("OfflineFixture", environment())
  expect_identical(dirname(path), tempdir())
  expect_match(basename(path), "^ProjrGitHubTestOfflineFixture")
  expect_true(dir.exists(path))
})
