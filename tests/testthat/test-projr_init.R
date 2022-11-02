test_that("projr_init works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  if (!requireNamespace("withr", quietly = TRUE)) {
    install.packages("withr")
  }
  withr::with_dir(
    dir_test,
    {
      projr::projr_init(dir_proj = dir_test, renv_force = FALSE)
      expect_identical(
        list.files() |> sort(),
        c(
          "_bookdown.yml", "_output.yml", "_projr.yml", "appendix.Rmd",
          "DELETE-AFTER-DOING.md", "DESCRIPTION", "index.Rmd", "renv",
          "renv.lock"
        ) |>
          sort()
      )

      expect_true(file.exists(".gitignore"))
      expect_true(file.exists(".Rbuildignore"))
    }
  )

  # check that files are correct

  unlink(dir_test, recursive = TRUE)
})
test_that("projr_init prompted initiation works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  if (!requireNamespace("withr", quietly = TRUE)) {
    install.packages("withr")
  }
  withr::with_dir(
    dir_test,
    {
      projr::projr_init(dir_proj = dir_test, renv_force = FALSE)
      expect_identical(
        list.files() |> sort(),
        c(
          "_bookdown.yml", "_output.yml", "_projr.yml", "appendix.Rmd",
          "DELETE-AFTER-DOING.md", "DESCRIPTION", "index.Rmd", "renv",
          "renv.lock"
        ) |>
          sort()
      )

      expect_true(file.exists(".gitignore"))

      expect_true(file.exists(".Rbuildignore"))
      rprofile <- readLines(".Rprofile")
    }
  )

  # check that files are correct

  unlink(dir_test, recursive = TRUE)
})
