
test_that("getting and setting metadat files works works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(class(.projr_yml_get()), "list")
      expect_identical(class(.projr_yml_bd_get()), "list")
      expect_identical(class(.projr_desc_get()), c("matrix", "array"))
      .projr_yml_set(list("directories-default" = NULL))
      expect_identical(.projr_yml_get(), list("directories-default" = NULL))
      .projr_yml_bd_set(list())
      expect_identical(.projr_yml_bd_get(), list())
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_error(.projr_yml_get())
      expect_error(.projr_yml_bd_get())
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
