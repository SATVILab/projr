
test_that("projr_build_dev works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./projr_test"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")
  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./projr_test"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init(renv_force = FALSE)
      # debugonce(.projr_build)
      projr_build_dev()
      yml_bd <- yaml::read_yaml(file.path(dir_test, "_bookdown.yml"))
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.0-9000")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.0-9000")
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_output works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./projr_test"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")
  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./projr_test"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init(renv_force = FALSE)
      # debugonce(.projr_build)
      projr_build_output()
      yml_bd <- yaml::read_yaml(file.path(dir_test, "_bookdown.yml"))
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-9000")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_build_output works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./projr_test"))
  fn_vec <- c(fn_vec, ".gitignore", ".Rbuildignore")
  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./projr_test"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  yml_projr <- yaml::read_yaml(file.path(dir_test, "_projr.yml"))
  yml_projr[["package_build"]] <- TRUE
  yaml::write_yaml(yml_projr, file.path(dir_test, "_projr.yml"))
  usethis::with_project(
    path = dir_test,
    code = {
      projr_init(renv_force = FALSE)
      # debugonce(.projr_build)
      projr_build_output()
      yml_bd <- yaml::read_yaml(file.path(dir_test, "_bookdown.yml"))
      expect_identical(basename(yml_bd$output_dir), "reportV0.0.1-9000")
      desc_file <- read.dcf(file.path(dir_test, "DESCRIPTION"))
      expect_identical(desc_file[1, "Version"][[1]], "0.0.1")
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
