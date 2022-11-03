test_that("projr_profile_get, _set and _create work", {
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
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical(projr_profile_get(), "default")
      Sys.unsetenv("PROJR_PROFILE")

      projr_profile_create()
      yml_projr <- .projr_yml_get()
      expect_true(
        paste0("directories-", normalizePath(getwd(), winslash = "/")) %in%
          names(yml_projr)
      )
      projr_profile <- projr_profile_get()
      expect_identical(
        projr_profile_get(), normalizePath(getwd(), winslash = "/")
      )
      Sys.setenv("PROJR_PROFILE" = "test_profile")
      projr_profile_create()
      yml_projr <- .projr_yml_get()
      expect_true("directories-test_profile" %in% names(yml_projr))
      expect_identical(projr_profile_get(), "test_profile")
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(
        projr_profile_get(), normalizePath(getwd(), winslash = "/")
      )
      projr_profile_delete(projr_profile_get())
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "test_profile")
      expect_identical(projr_profile_get(), "test_profile")
      projr_profile_delete(projr_profile_get())
      expect_identical(projr_profile_get(), "default")
      expect_error(projr_profile_delete(1))
      expect_true(projr_profile_delete())
      Sys.unsetenv("PROJR_PROFILE")
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
