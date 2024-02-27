test_that("basic yml functions work", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))
  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- fn_vec

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_int <- .projr_yml_get_root_full()
      expect_identical(class(.projr_yml_get_root_full()), "list")
      expect_identical(class(.projr_yml_bd_get()), "list")
      expect_identical(class(.projr_desc_get()), c("matrix", "array"))
      yml_projr_min <- list(
        "directories" = NULL, "build" = list()
      )
      .projr_yml_set(yml_projr_min)
      expect_identical(.projr_yml_get_root_full(), yml_projr_min)
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_error(.projr_yml_get_root_full())
      expect_identical(.projr_yml_bd_get(), list())
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_yml_check works", {
  # skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      expect_true(projr_yml_check())
      projr_yml_check()
      projr_yml_dest_add_local(
        title = "test",
        content = "data-raw",
        path = "_outputting",
        send_version_source = "file",
        send_sync_approach = "upload-all",
        send_conflict = "overwrite"
      )
      expect_true(projr_yml_check())
    }
  )
})

test_that("projr_yml_dest_add_* functions work", {
  # skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      #  browser()
      .projr_test_yml_unset_remote()
      projr_yml_dest_add_local(
        title = "test", content = "data-raw", path = "_archive"
      )
      expect_true(!is.null(.projr_yml_dest_get_type("local", "default")))

      projr_yml_dest_add_osf(
        title = "test", content = "data-raw", category = "project"
      )
      expect_true(!is.null(.projr_yml_dest_get_type("osf", "default")))
      expect_true(
        .is_string(
          .projr_yml_dest_get_type("osf", "default")[["test"]][["id"]]
        )
      )
      projr_yml_dest_add_github(
        title = "test", content = "data-raw"
      )
      expect_true(!is.null(.projr_yml_dest_get_type("github", "default")))
    }
  )
})
