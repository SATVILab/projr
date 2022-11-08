test_that("projr_build_dev works", {
  dir_test <- file.path(tempdir(), paste0("report"))
  if (dir.exists(dir_test)) unlink(dir_test, recursive = TRUE)
  if (!dir.exists(dir_test)) dir.create(dir_test)
  Sys.setenv("PROJR_TEST" = "TRUE")
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      projr_build_dev(quiet = TRUE)
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
  Sys.setenv("PROJR_TEST" = "TRUE")
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      yml_projr <- projr_yml_get()
      dir.create("_archive")
      file.create("_archive/V0.0.1.zip")
      projr_build_output(quiet = TRUE)
      yml_bd <- .projr_yml_bd_get()
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-1")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
      expect_identical(list.files(projr_dir_get("output")), character(0))
      expect_identical(
        list.files(projr_dir_get("output", output_safe = FALSE)), "bookdown"
      )
      # test that it runs correctly when there is an error
      yml_bd <- .projr_yml_bd_get()
      writeLines(
        c("# Error", "\n", "```{r }", "\n", "stop()", "```"),
        con = "error.Rmd"
      )
      yml_bd[["rmd_files"]] <- c(yml_bd[["rmd_files"]], "error.Rmd")
      .projr_yml_bd_set(yml_bd)
      expect_error(projr_build_output(quiet = TRUE))
      # reset after error
      yml_bd <- .projr_yml_bd_get()
      file.remove("error.Rmd")
      yml_bd[["rmd_files"]] <- c("index.Rmd", "appendix.Rmd")
      .projr_yml_bd_set(yml_bd)

      # test copying to other directories
      yml_projr <- projr_yml_get()
      dir.create("_archive")
      file.create("_archive/V0.0.1.zip")
      yml_projr <- .projr_yml_get()
      # check that copying non-default directories works as well
      copy_list <- list(
        data_raw = TRUE, cache = TRUE, bookdown = FALSE, package = TRUE
      )
      yml_projr[["build-output"]][["copy_to_output"]] <- copy_list
      print(yml_projr)
      .projr_yml_set(list_save = yml_projr)
      debugonce(.projr_output_copy)
      projr_build_output(quiet = TRUE)
    },
    quiet = TRUE,
    force = TRUE
  )
  Sys.unsetenv("PROJR_TEST")
  unlink(dir_test, recursive = TRUE)
})
