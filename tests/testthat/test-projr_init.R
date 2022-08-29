test_that("projr_init works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  if (!dir.exists(dir_test)) dir.create(dir_test)
  setwd(dir_test)
  # empty directory
  # unlink(list.files(dir_test), recursive = TRUE)
  dir_vec <- setdiff(list.dirs(dir_test), dir_test)
  for (i in seq_along(dir_vec)) {
    unlink(dir_vec[i], recursive = TRUE)
  }
  fn_vec <- list.files(dir_test)
  for (i in seq_along(fn_vec)) {
    unlink(fn_vec[i])
  }
  # check that directory is empty
  expect_identical(length(list.files()), 0L)
  projr::projr_init()

  # check that files are correct

  expect_identical(
    list.files() |> sort(),
    c(
      "_bookdown.yml", "_output.yml", "_projr.yml", "appendix.Rmd",
      "DELETE-AFTER-DOING.md", "DESCRIPTION", "index.Rmd", "renv",
      "renv.lock"
    ) |>
      sort()
  )
})
