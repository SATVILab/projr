test_that("projr_init works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  if (!requireNamespace("withr", quietly = TRUE)) {
    install.packages("withr")
  }
  Sys.setenv("PROJR_INTERACTIVE" = "FALSE")
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(projr_init())
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("_projr.yml"))
      expect_true(file.exists(".git"))
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("index.Rmd"))
      expect_true(file.exists("appendix.Rmd"))
    },
    force = TRUE,
    quiet = TRUE
  )

  Sys.unsetenv("PROJR_INTERACTIVE")
  unlink(dir_test, recursive = TRUE)
})
