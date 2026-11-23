test_that("build.renv defaults to auto and accepts profile overrides", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE, rm_engine = TRUE)
  usethis::with_project(project,
    {
      expect_identical(.yml_renv_get(NULL), "auto")
      projr_yml_renv_set(TRUE)
      expect_true(.yml_renv_get(NULL))
      projr_yml_renv_set(FALSE, profile = "local")
      expect_false(.yml_renv_get(NULL))
      projr_yml_renv_set("auto", profile = "local")
      expect_identical(.yml_renv_get(NULL), "auto")
      projr_yml_renv_set("auto")
      expect_null(.yml_get("default")$build$renv)
      expect_true(projr_yml_check())
      expect_error(projr_yml_renv_set("yes"), "build[.]renv")
      .yml_set(list(build = list(renv = 1L)), "local")
      expect_error(projr_yml_check(), "build[.]renv")
    },
    force = TRUE
  )
})

test_that("snapshot modes use project indicators and production builds", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(project,
    {
      writeLines("build: {}", "_projr.yml")
      testthat::local_mocked_bindings(.is_test = function() FALSE)
      expect_false(.build_renv_snapshot_check(TRUE))
      projr_yml_renv_set(TRUE)
      expect_true(.build_renv_snapshot_check(TRUE))
      expect_false(.build_renv_snapshot_check(FALSE))
      projr_yml_renv_set("auto")
      dir.create("renv")
      file.create("renv/activate.R")
      expect_true(.build_renv_snapshot_check(TRUE))
      unlink("renv/activate.R")
      dir.create("locks")
      withr::local_envvar(c(RENV_PATHS_LOCKFILE = "locks/"))
      file.create("locks/renv.lock")
      dir.create("nested")
      withr::with_dir("nested", expect_true(.build_renv_snapshot_check(TRUE)))
      projr_yml_renv_set(FALSE)
      expect_false(.build_renv_snapshot_check(TRUE))
    },
    force = TRUE
  )
})

test_that("snapshots create a first lockfile and report missing render dependencies without replacing it", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(project,
    {
      testthat::local_mocked_bindings(.is_test = function() FALSE)
      withr::local_envvar(c(RENV_PATHS_ROOT = file.path(tempdir(), "build-snapshot-cache")))
      writeLines("build: {renv: true}", "_projr.yml")
      writeLines("digest::digest('x')", "analysis.R")
      expect_true(.build_renv_snapshot(TRUE))
      expect_true("digest" %in% names(renv::lockfile_read("renv.lock")$Packages))
      expect_false(file.exists(".Rprofile"))
      projr_yml_renv_set("auto")
      before <- readBin("renv.lock", "raw", n = file.size("renv.lock"))
      writeLines("library(projrAbsentSnapshotFixture)", "analysis.R")
      expect_error(.build_pre_check(TRUE), "Missing renv snapshot dependencies: projrAbsentSnapshotFixture")
      # A dependency introduced while rendering also aborts the post-build snapshot.
      expect_error(.build_renv_snapshot(TRUE), "projrAbsentSnapshotFixture")
      expect_identical(readBin("renv.lock", "raw", n = file.size("renv.lock")), before)
      renv::settings$ignored.packages("projrAbsentSnapshotFixture", project = project)
      expect_true(.build_renv_snapshot(TRUE))
    },
    force = TRUE
  )
})

test_that("renv snapshot preflight honors explicit and custom package selection", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  usethis::with_project(project,
    {
      writeLines(c("Package: localfixture", "Version: 1.0", "Imports: digest"), "DESCRIPTION")
      writeLines("library(projrAbsentSnapshotFixture)", "outside.R")
      renv::settings$snapshot.type("explicit", project = project)
      expect_true("digest" %in% .build_renv_packages(project))
      renv::settings$snapshot.type("custom", project = project)
      withr::local_options(list(renv.snapshot.filter = function(project) "projrAbsentSnapshotFixture"))
      expect_error(.build_renv_packages(project), "Missing renv snapshot dependencies: projrAbsentSnapshotFixture")
      renv::settings$snapshot.type("all", project = project)
      expect_null(.build_renv_packages(project))
    },
    force = TRUE
  )
})
