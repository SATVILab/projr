test_that(".projr_pb_check_run works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
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
      yml_projr_init <- .projr_yml_get_root_full()
      expect_false(.projr_pb_check_run(output_run = FALSE))
      yml_projr <- yml_projr_init
      yml_projr[["build"]] <- yml_projr[["build"]][c("git", "dev-output")]
      .projr_yml_set(yml_projr)
      expect_false(.projr_pb_check_run(output_run = TRUE))
      .projr_yml_set(yml_projr_init)
      # test zip creation
      file.create(projr_path_get("cache", "test.txt"))
      expect_identical(
        .projr_zip_dir_pb(
          tag = "v1.0.0", label = "cache", output_run = TRUE
        ),
        file.path(
          getwd(),
          projr_path_get(
            "cache", "projr", paste0("v", projr_version_get()),
            "gh_release", "v1.0.0", "cache.zip"
          )
        ) |>
          normalizePath(winslash = "/", mustWork = FALSE)
      )
      expect_true(file.exists(projr_path_get(
        "cache", "projr", paste0("v", projr_version_get()),
        "gh_release", "v1.0.0", "cache.zip"
      )))
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
