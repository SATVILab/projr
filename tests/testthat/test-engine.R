test_that("projr_engine_get works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

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
      expect_identical(.projr_engine_get(), "bookdown")
      unlink("_bookdown.yml")
      expect_identical(.projr_engine_get(), "rmd")
      unlink(list.files(pattern = "\\.Rmd$|\\.rmd$"))
      expect_error(.projr_engine_get())
      file.create("_quarto.yml")
      expect_identical(.projr_engine_get(), "quarto_project")
      unlink("_quarto.yml")
      invisible(file.create("index.qmd"))
      expect_identical(.projr_engine_get(), "quarto_document")
    },
    force = TRUE,
    quiet = TRUE
  )
})
