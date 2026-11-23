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
  expect_identical(normalizePath(dirname(path)), normalizePath(tempdir()))
  expect_match(basename(path), "^ProjrGitHubTestOfflineFixture")
  expect_true(dir.exists(path))
})

test_that("user R configuration is created only in the test HOME", {
  skip_if(.is_test_select())
  expect_identical(Sys.getenv("HOME"), .test_home)
  expect_identical(Sys.getenv("R_USER_CONFIG_DIR"), .test_home)
  expect_identical(Sys.getenv("R_PROFILE_USER"), file.path(.test_home, ".Rprofile"))
  path <- .init_renviron_create()
  expect_identical(normalizePath(path), normalizePath(file.path(.test_home, ".Renviron")))
  expect_true(file.exists(path))
})

test_that("loading the installed package and running a first build confine their writes", {
  skip_if(.is_test_select())
  # A fresh R process needs the installed package, including its native code.
  skip_if_not(file.exists(system.file("Meta", "package.rds", package = "projr")))
  sandbox <- withr::local_tempdir()
  home <- file.path(sandbox, "home")
  scratch <- file.path(sandbox, "tmp")
  project <- file.path(sandbox, "analysis")
  fs::dir_create(c(home, scratch, project))
  withr::local_envvar(c(
    HOME = home, R_USER = home, TMPDIR = scratch, TMP = scratch, TEMP = scratch,
    R_USER_CONFIG_DIR = home, XDG_CONFIG_HOME = home,
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
  ))
  script <- file.path(sandbox, "check.R")
  writeLines(c(
    "project <- commandArgs(TRUE)[[1L]]",
    "setwd(project)",
    "library(projr)",
    "stopifnot(!length(list.files(all.files = TRUE, no.. = TRUE)))",
    "projr_build_patch('First build', project = project)",
    "stopifnot(file.exists('_projr.yml'), file.exists('BUILDLOG.md'))"
  ), script)
  exe <- file.path(R.home("bin"), "Rscript")
  output <- system2(exe, c("--vanilla", shQuote(script), shQuote(project)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_identical(list.files(home, all.files = TRUE, no.. = TRUE), character())
  expect_identical(list.files(scratch, all.files = TRUE, no.. = TRUE), character())
})
