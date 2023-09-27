test_that("projr_osf_source_add works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  withr::defer(unlink(dir_test))

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
  gert::git_init(dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_orig <- projr_yml_get_unchecked()
      suffix <- rnorm(1) |>
        signif(4) |>
        as.character()
      yml_projr_orig[["directories"]][
        [paste0("DataRawOSFTest", suffix)
        ]] <- list()
      projr_osf_source_add(label = "data-raw", category = "project")
      expect_error(
        projr_osf_source_add(label = "data-raw", category = "project")
      )
      yml_projr <- projr_yml_get_unchecked()
      yml_projr_dir_data_raw <-
        yml_projr[["directories"]][["data-raw"]]
      id_proj <- yml_projr_dir_data_raw[["osf"]][["data-raw"]][["id"]]
      expect_true(is.character(id_proj))
      projr_osf_source_add(label = "data-raw", category = "project")
      .projr_osf_rm_node_id(id_proj)
    },
    quiet = TRUE,
    force = TRUE
  )
})
