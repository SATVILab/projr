dir_test <- file.path(tempdir(), paste0("test_projr"))

if (!dir.exists(dir_test)) dir.create(dir_test)
wd <- getwd()

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

stopifnot(length(list.files()) == 0L)
invisible(suppressWarnings(suppressMessages(
  invisible(projr::projr_init(renv_force = FALSE))
)))

test_that("projr_get_yml_active works", {
  yml_active <- projr_get_yml_active(
    wd_var = "LOCAL_WORKSPACE_FOLDER",
    path_yml = system.file(
      "project_structure", "_projr.yml",
      package = "projr"
    ),
    silent = TRUE
  )
  expect_true(sum(grepl("^directories", names(yml_active))) == 1)
  projr_usr_add()
  yml <- yaml::read_yaml("_projr.yml")
  expect_true(sum(grepl("^directories", names(yml))) == 2)
})

test_that("projr_set_up_dir works", {

  # check that directory is empty

  yml_active <- yaml::read_yaml(
    system.file("project_structure", "_projr.yml", package = "projr")
  )
  names(yml_active) <- gsub("directories\\-default", "directories", names(yml_active))

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
  gitignore <- readLines(".gitignore")
  expect_identical(
    length(which(gitignore == "_output/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore == "_archive/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore == "inst/extdata/**/*")), 1L
  )
  expect_identical(
    length(which(gitignore == "_tmp/**/*")), 1L
  )

  rbuildignore <- readLines(".Rbuildignore")
  expect_identical(
    length(which(rbuildignore == "^inst/extdata")), 1L
  )
  expect_identical(
    length(which(rbuildignore == "^_tmp")), 1L
  )
  expect_identical(
    length(which(rbuildignore == "^_output")), 1L
  )
  expect_identical(
    length(which(rbuildignore == "^_tmp")), 1L
  )
})

setwd(wd)

dir_test <- file.path(tempdir(), paste0("test_projr"))

if (!dir.exists(dir_test)) dir.create(dir_test)
wd <- getwd()

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

stopifnot(length(list.files()) == 0L)
invisible(suppressWarnings(suppressMessages(
  invisible(projr::projr_init(renv_force = FALSE))
)))


test_that("projr_activate runs", {
  # debugonce("projr_activate")
  # debugonce("projr_get_yml_active")
  expect_identical(class(projr::projr_activate()), "list")
})

setwd(wd)
