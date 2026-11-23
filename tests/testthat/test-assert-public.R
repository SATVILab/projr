test_that("public YAML setters validate before writing to disk", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(
    project,
    {
      path <- .yml_get_path("default")
      original <- readBin(path, "raw", file.size(path))
      for (add in list(projr_yml_hooks_add_pre, projr_yml_hooks_add_post)) {
        expect_error(add(123))
      }
      for (add in list(projr_yml_script_add_pre, projr_yml_script_add_post)) {
        expect_error(add("prepare.R", title = "prepare", cue = "bad"))
      }
      expect_error(projr_yml_dest_add_local(
        "test",
        "output",
        "_archive",
        structure = c("latest", "archive")
      ))
      expect_error(projr_yml_dest_add_github(
        "test",
        "output",
        send_inspect = c("none", "manifest")
      ))
      expect_identical(readBin(path, "raw", file.size(path)), original)
      projr_yml_script_add_pre(c("one.R", "two.R", "three.R"), "prepare")
      projr_yml_script_rm("prepare", c("one.R", "three.R"))
      expect_identical(
        .yml_script_get("default")[["prepare"]][["path"]],
        "two.R"
      )
      yml <- .yml_get("default")
      yml$settings <- list(hashing = list(cores = -1))
      .yml_set(yml, "default")
      expect_identical(projr_yml_get(check = FALSE)$settings, yml$settings)
      expect_error(projr_yml_get(check = TRUE), "cores")
      expect_error(projr_yml_get(check = NA))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("file queries require a filename and parameter keys are used literally", {
  skip_if(.is_test_select())
  for (query in list(
    projr_manifest_file_first,
    projr_manifest_file_last_change,
    projr_manifest_file_changed,
    projr_manifest_file_history
  )) {
    expect_error(query(NULL), "fn")
  }
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(
    project,
    {
      yml <- .yml_get("default")
      yml$parameters <- list("participant's age" = list(years = 56L))
      .yml_set(yml, "default")
      expect_identical(projr_par_get("participant's age", "years"), 56L)
      expect_null(projr_par_get("participant's age", "unrecorded"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("version and development cleanup flags are checked before mutation", {
  skip_if(.is_test_select())
  project <- .test_setup_project(git = FALSE)
  usethis::with_project(
    project,
    {
      writeLines(c("Package: example", "Version: 0.0.1"), "DESCRIPTION")
      before <- readLines("DESCRIPTION")
      expect_error(projr_version_set("1.2.3", only_if_exists = NA))
      expect_identical(readLines("DESCRIPTION"), before)
      expect_error(projr_build_dev(bump = 1))
      expect_error(projr_build_dev(old_dev_remove = NULL))
      local_mocked_bindings(.build = function(old_dev_remove, ...) {
        old_dev_remove
      })
      expect_false(projr_build_dev(old_dev_remove = FALSE))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("the changelog printer respects the requested count", {
  skip_if(.is_test_select())
  local_mocked_bindings(.changelog_read = function() c("first", "second"))
  expect_identical(capture.output(projr_cat_changelog(20)), "first second")
  expect_identical(capture.output(projr_cat_changelog(0)), character())
  expect_error(projr_cat_changelog(1.5))
})


test_that("an invalid environment file list preserves the cleanup record", {
  skip_if(.is_test_select())
  record <- withr::local_tempfile()
  writeLines("KEEP_RECORD", record)
  local_mocked_bindings(.env_file_get_path_list = function() record)
  expect_error(projr_env_set(c("absent", NA_character_)))
  expect_identical(readLines(record), "KEEP_RECORD")
})

test_that("downloads reject multiple remote types before creating a directory", {
  skip_if(.is_test_select())
  destination <- tempfile(tmpdir = withr::local_tempdir())
  expect_error(
    .remote_file_get_all(c("local", "github"), "example", destination),
    "type"
  )
  expect_false(dir.exists(destination))
})

test_that("GitHub setup validates repository options before checking the checkout", {
  skip_if(.is_test_select())
  local_mocked_bindings(.git_repo_check_exists = function() {
    stop("unexpected Git access")
  })
  expect_error(projr_init_github(public = NA), "public")
  expect_error(projr_init_github(user = c("a", "b")), "user")
})
