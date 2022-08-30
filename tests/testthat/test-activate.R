test_that("projr_set_up_dir works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  wd <- getwd()
  on.exit(setwd(wd))
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
  for (x in c(".gitignore", ".Rbuildignore", ".Rprofile")) {
    if (file.exists(file.path(dir_test, x))) {
      unlink(file.path(dir_test, x))
    }
  }
  # check that directory is empty
  expect_identical(length(list.files()), 0L)
  invisible(suppressWarnings(suppressMessages(
    invisible(projr::projr_init(renv_force = FALSE))
  )))

  yml_active <- yaml::read_yaml(
    system.file("project_structure", "_projr.yml", package = "projr")
  )$default

  projr_set_up_dir(
    yml_active = yml_active,
    create_var = TRUE,
    env = .GlobalEnv
  )
  expect_true(dir.exists("_archive"))
  expect_true(dir.exists("_output"))
  expect_true(dir.exists("inst/extdata"))
  expect_true(dir.exists("_tmp"))

  expect_true(exists("dir_data_raw", envir = .GlobalEnv))
  expect_true(exists("dir_cache", envir = .GlobalEnv))
  expect_true(exists("dir_output", envir = .GlobalEnv))
  expect_true(exists("dir_archive", envir = .GlobalEnv))

  gitignore <- read.table(".gitignore")
  expect_identical(
    length(which(gitignore$V1 == "_output/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore$V1 == "_archive/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore$V1 == "inst/extdata/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore$V1 == "_tmp/**/*")), 1L
  )

  rbuildignore <- read.table(".Rbuildignore")
  expect_identical(
    length(which(rbuildignore$V1 == "^inst/extdata")), 1L
  )
  expect_identical(
    length(which(rbuildignore$V1 == "^_tmp")), 1L
  )
  expect_identical(
    length(which(rbuildignore$V1 == "^_output")), 1L
  )
  expect_identical(
    length(which(rbuildignore$V1 == "^_tmp")), 1L
  )
})
