test_that(".pb_check_run works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))

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
      # automatic exits
      # ---------------------
      yml_projr_init <- .yml_get_default_raw()
      expect_false(.pb_check_run(output_run = FALSE))
      yml_projr <- yml_projr_init
      yml_projr[["build"]] <- yml_projr[["build"]][c("git", "dev-output")]
      .yml_set(yml_projr)
      expect_false(.pb_check_run(output_run = TRUE))
      .yml_set(yml_projr_init)
      # test zip creation
      file.create.path_get("cache", "test.txt"))
      expect_identical(
        .zip_dir_pb(
          tag = "v1.0.0", label = "cache", output_run = TRUE
        ),
        file.path(
          getwd(),
         .path_get(
            "cache", "projr", .version_get_v(),
            "gh_release", "v1.0.0", "cache.zip"
          )
        ) |>
          normalizePath(winslash = "/", mustWork = FALSE)
      )
      expect_true(file.exists.path_get(
        "cache", "projr", .version_get_v(),
        "gh_release", "v1.0.0", "cache.zip"
      )))
    },
    quiet = TRUE,
    force = TRUE
  )
})
