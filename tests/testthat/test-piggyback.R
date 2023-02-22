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
      testthat::skip_if(TRUE)
      # automatic exits
      # ---------------------
      yml_projr_init <- .projr_yml_get_root_full()
      expect_false(.projr_pb_check_run(
        output_run = FALSE, bump_component = "major"
      ))
      yml_projr <- yml_projr_init
      yml_projr[["build"]] <- list()
      .projr_yml_set(yml_projr)
      expect_false(.projr_pb_check_run(
        output_run = TRUE, bump_component = "major"
      ))
      .projr_yml_set(yml_projr_init)
      yml_projr <- yml_projr_init
      yml_projr[["build"]][["github-release"]] <- list()
      .projr_yml_set(yml_projr)
      expect_false(.projr_pb_check_run(
        output_run = TRUE, bump_component = "major"
      ))
      .projr_yml_set(yml_projr_init)

      # per-item exits
      # ---------------------
      yml_projr <- yml_projr_init
      yml_projr[["build"]][["github-release"]] <- list(
        "data-raw" = FALSE,
        "docs" = FALSE
      )
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
