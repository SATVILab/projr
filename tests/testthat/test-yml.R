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
      yml.int <- .yml_get_default_raw()
      expect_identical(class(.yml_get_default_raw()), "list")
      expect_identical(class(.yml_bd_get()), "list")
      expect_identical(class(.desc_get()), c("matrix", "array"))
      yml.min <- list(
        "directories" = NULL, "build" = list()
      )
      .yml_set(yml.min)
      expect_identical(.yml_get_default_raw(), yml.min)
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_identical(class(.yml_get_default_raw()), "list")
      expect_identical(.yml_bd_get(), list())
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(.yml_check works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
     .init()
      expect_true.yml_check())
     .yml_check()
     .yml_dest_add_local(
        title = "test",
        content = "raw-data",
        path = "_outputting",
        send_inspect = "file",
        send_strategy = "upload-all",
        send_conflict = "overwrite"
      )
      expect_true.yml_check())
    }
  )
})

test_that(.yml_dest_add_* functions work", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      .test_yml_unset_remote()
     .yml_dest_add_local(
        title = "test", content = "raw-data", path = "_archive"
      )
      expect_true(!is.null(.yml_dest_get_type("local", "default")))

     .yml_dest_add_osf(
        title = "test", content = "raw-data", category = "project"
      )
      expect_true(!is.null(.yml_dest_get_type("osf", "default")))
      expect_true(
        .is_string(
          .yml_dest_get_type("osf", "default")[["test"]][["id"]]
        )
      )
     .yml_dest_add_github(
        title = "test", content = "raw-data"
      )
      expect_true(!is.null(.yml_dest_get_type("github", "default")))
    }
  )
})
