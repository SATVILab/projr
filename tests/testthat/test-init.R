test_that("projr_init works", {
  dir_test <- file.path(tempdir(), paste0("testProjr2"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(projr_init())
      expect_true(file.exists("_bookdown.yml"))
      expect_true(file.exists("_output.yml"))
      expect_true(file.exists("_projr.yml"))
      # expect_true(file.exists(".git"))
      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
      expect_true(file.exists("DESCRIPTION"))
      expect_true(file.exists("index.Rmd"))
      expect_true(file.exists("appendix.Rmd"))
      expect_error(projr_init(yml_path_from = "abcsadfkasdflkda"))
    },
    force = TRUE,
    quiet = TRUE
  )

  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})
