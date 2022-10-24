
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

test_that("projr_build_output copy works", {
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
      # browser()
      # test that nothing is copied
      projr_init(renv_force = FALSE)
      yml_projr <- yaml::read_yaml(file.path(dir_test, "_projr.yml"))
      yml_projr[["build-output"]][["copy_to_output"]] <- lapply(
        seq_along(yml_projr[["build-output"]][["copy_to_output"]]),
        function(i) FALSE
      ) |>
        stats::setNames(names(yml_projr[["build-output"]][["copy_to_output"]]))
      yaml::write_yaml(yml_projr, file.path(dir_test, "_projr.yml"))
      # debugonce(.projr_build)
      dir_output <- yml_projr[["directories-default"]][["output"]][["path"]]
      dir_archive <- yml_projr[["directories-default"]][["archive"]][["path"]]
      if (dir.exists(dir_output)) unlink(dir_output, recursive = TRUE)
      if (dir.exists(dir_archive)) unlink(dir_archive, recursive = TRUE)
      projr_build_output()
      expect_true(
        length(list.dirs(dir_output)) == 0
      )
      expect_true(!dir.exists(dir_archive))
      # test that everything is copied
      yml_projr <- yaml::read_yaml(file.path(dir_test, "_projr.yml"))
      yml_projr[["build-output"]][["copy_to_output"]] <- lapply(
        seq_along(yml_projr[["build-output"]][["copy_to_output"]]),
        function(i) TRUE
      ) |>
        stats::setNames(names(yml_projr[["build-output"]][["copy_to_output"]]))
      yaml::write_yaml(yml_projr, file.path(dir_test, "_projr.yml"))
      dir_data_raw <- yml_projr[["directories-default"]][["data_raw"]][["path"]]
      if (!dir.exists(dir_data_raw)) {
        dir.create(dir_data_raw, recursive = TRUE)
      }
      file.create(file.path(dir_data_raw, "test.txt"))
      dir_cache <- yml_projr[["directories-default"]][["cache"]][["path"]]
      if (!dir.exists(dir_cache)) {
        dir.create(dir_cache, recursive = TRUE)
      }
      file.create(file.path(dir_cache, "test.txt"))
      projr_build_output()
      expect_identical(
        length(list.dirs(dir_output)), 5L
      )
      expect_true(dir.exists(dir_archive))
      expect_true(file.exists(file.path(dir_archive, "V0.0.2.zip")))
      expect_identical(
        list.files(file.path(dir_output, "bookdown")),
        "reportV0.0.2.zip"
      )
      expect_identical(
        list.files(file.path(dir_output, "data_raw")),
        "dir_data_raw.zip"
      )
      expect_identical(
        list.files(file.path(dir_output, "cache")),
        "dir_cache.zip"
      )
      expect_identical(
        list.files(file.path(dir_output, "pkg")),
        "projr_0.0.2.tar.gz"
      )
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
