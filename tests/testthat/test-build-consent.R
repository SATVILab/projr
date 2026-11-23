test_that("an unapproved unattended build cannot change an implicit checkout", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  user <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_CONFIG_DIR = user, XDG_CONFIG_HOME = user))
  withr::local_options(askYesNo = function(...) stop("unexpected prompt"))
  withr::local_dir(project)

  expect_error(projr_build_patch("first build"), "pass project =")
  expect_identical(list.files(project, all.files = TRUE, no.. = TRUE), character())
  fs::dir_create("_projr/config")
  writeLines("directories: {cache: {path: cache}}", "_projr/config/_projr.yml")
  dir.create("nested")
  expect_error(projr_build_dev(project = "nested"), "project root is")
  expect_true(file.exists("_projr/config/_projr.yml"))
  expect_identical(projr_yml_get()$directories$cache$path, "cache")
  expect_true(projr_build_check_packages()$available)
  expect_false(dir.exists(projr_path_get_dir("cache", create = FALSE)))
  expect_false(dir.exists(".projr"))
  expect_false(file.exists("VERSION"))
  expect_identical(list.files(user, all.files = TRUE, no.. = TRUE), character())
})

test_that("interactive approval covers one canonical checkout and can be declined", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  user <- withr::local_tempdir()
  other <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_CONFIG_DIR = user, XDG_CONFIG_HOME = user))
  interactive_run <- TRUE
  local_mocked_bindings(.build_is_interactive = function() interactive_run)
  prompts <- character()
  answer <- FALSE
  withr::local_options(askYesNo = function(msg, default, ...) {
    prompts <<- c(prompts, msg)
    expect_false(default)
    answer
  })
  withr::local_dir(project)
  expect_error(projr_build_dev(project = "."), "Build cancelled")
  expect_identical(list.files(project, all.files = TRUE, no.. = TRUE), character())
  expect_identical(list.files(user, all.files = TRUE, no.. = TRUE), character())
  expect_true(grepl(as.character(fs::path_real(project)), prompts[[1]], fixed = TRUE))
  expect_true(grepl("outside this checkout", prompts[[1]], fixed = TRUE))
  expect_true(grepl("remote destinations", prompts[[1]], fixed = TRUE))

  answer <- TRUE
  expect_true(projr_build_dev(scripts = character()))
  expect_true(file.exists("_projr.yml"))
  expect_length(list.files(user, recursive = TRUE), 1L)
  dir.create("nested")
  withr::with_dir("nested", {
    expect_true(projr_build_dev(scripts = character()))
    expect_identical(basename(getwd()), "nested")
    interactive_run <- FALSE
    expect_true(projr_build_dev(scripts = character()))
  })
  expect_length(prompts, 2L)
  file.copy("_projr.yml", other)
  withr::with_dir(other, expect_error(projr_build_dev(), "no saved build approval"))
  expect_identical(list.files(other, all.files = TRUE, no.. = TRUE), "_projr.yml")
})

test_that("all build entry points accept an unattended project without saving permission", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  project <- withr::local_tempdir()
  caller <- withr::local_tempdir()
  user <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_CONFIG_DIR = user, XDG_CONFIG_HOME = user))
  withr::local_options(askYesNo = function(...) stop("unexpected prompt"))
  withr::local_dir(caller)
  functions <- list(projr_build, projr_build_patch, projr_build_minor, projr_build_major)
  for (build in functions) {
    expect_true(build(msg = "Example", project = project))
    expect_identical(getwd(), caller)
  }
  writeLines("stop('configured script failed')", file.path(project, "broken.R"))
  expect_error(projr_build_dev(scripts = "broken.R", project = project), "configured script failed")
  expect_identical(getwd(), caller)
  expect_true(file.exists(file.path(project, "BUILDLOG.md")))
  expect_identical(list.files(user, all.files = TRUE, no.. = TRUE), character())
  expect_identical(list.files(caller, all.files = TRUE, no.. = TRUE), character())
})

test_that("files inside a checkout cannot authorize its build", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  withr::local_dir(project)
  # Even an externally chosen R config location can point inside the checkout.
  withr::local_envvar(c(R_USER_CONFIG_DIR = project))
  path <- file.path(tools::R_user_dir("projr", "config"), "build-approvals")
  fs::dir_create(path)
  key <- digest::digest(enc2utf8(as.character(fs::path_real(project))), algo = "sha256", serialize = FALSE)
  file.create(file.path(path, key))
  expect_error(projr_build_dev(), "no saved build approval")
  expect_false(file.exists("_projr.yml"))
})

test_that("running from an approved legacy config directory restores the original location", {
  skip_if(.is_test_select())
  project <- withr::local_tempdir()
  user <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_CONFIG_DIR = user))
  old <- file.path(project, "_projr", "config")
  fs::dir_create(old)
  writeLines("directories: {cache: {path: cache}}", file.path(old, "_projr.yml"))
  .build_project_approve(project)
  withr::with_dir(old, {
    expect_true(projr_build_dev(scripts = character()))
    expect_identical(getwd(), old)
  })
  expect_true(file.exists(file.path(project, ".projr", "config", "_projr.yml")))
})
