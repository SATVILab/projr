test_that("projr_get_yml_active works", {

  # using directories-default
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
      yml_active <- projr_get_yml_active(
        wd_var = "PROJR_WORKING_DIRECTORY",
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
      yml_active <- projr_get_yml_active(
        wd_var = "PROJR_WORKING_DIRECTORY",
        path_yml = system.file(
          "project_structure", "_projr.yml",
          package = "projr"
        ),
        silent = TRUE
      )
      yml_active_dir <- yml_active[["directories"]]
      # ignore is imputed
      expect_true(
        sapply(yml_active_dir, function(x) "ignore" %in% names(x)) |> all()
      )
      # empty path is imputed
      expect_true(
        sapply(yml_active_dir, function(x) nzchar(x[["path"]])) |> all()
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_set_up_dir works", {
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
      yml_active <- yaml::read_yaml(
        system.file("project_structure", "_projr.yml", package = "projr")
      )
      names(yml_active) <- gsub(
        "directories\\-default", "directories", names(yml_active)
      )
      version_format_list <- .get_version_format_list(
        version_format = yml_active[["version"]]
      )
      yml_bd <- yaml::read_yaml(
        system.file("project_structure", "_bookdown.yml", package = "projr")
      )
      proj_nm <- .get_proj_nm(
        fn = yml_bd$book_filename,
        version_format = yml_active[["version"]]
      )
      version_current <- gsub(
        paste0("^", proj_nm), "", yml_bd$book_filename
      )
      projr_set_up_dir(
        yml_active = yml_active,
        version_current = version_current,
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
      expect_true(!exists("dir_archive", envir = .GlobalEnv))
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
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_activate runs", {
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
      expect_identical(class(projr_activate()), "list")
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
