
test_that("projr_build_dev works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  if (!requireNamespace("withr", quietly = TRUE)) {
    install.packages("withr")
  }
  Sys.setenv("PROJR_TEST" = "TRUE")
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      projr_build_dev()
      projr_version_get()
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.0-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-1")
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_output works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  if (!requireNamespace("withr", quietly = TRUE)) {
    install.packages("withr")
  }
  Sys.setenv("PROJR_TEST" = "TRUE")
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr <- projr_yml_get()
      dir.create("_archive")
      file.create("_archive/V0.0.1.zip")
      projr_build_output()
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)), "bookdown"
      )
      yml_projr <- projr_yml_get()
      browser()
      yml_bd <- .projr_yml_bd_get()
      yml_bd[["rmd_files"]] <- paste0(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      expect_error(projr_build_output())
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})
