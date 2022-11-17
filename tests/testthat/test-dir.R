
test_that("projr_dir_get works", {
  dir_test <- file.path(tempdir(), paste0("report"))

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


  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(projr_dir_get("abc"))
      expect_identical(projr_dir_get("data_raw"), "_data_raw")
      expect_identical(projr_dir_get("output"), "_tmp/projr_output/0.0.0-1")
      expect_identical(projr_dir_get(
        "output",
        output_safe = TRUE,
        create = FALSE,
        path_relative_force = FALSE
      ), "_tmp/projr_output/0.0.0-1")
      expect_identical(projr_dir_get("output", output_safe = FALSE), "_output")
      expect_identical(projr_dir_get("archive"), "_archive")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_dir_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_dir_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get()
      yml_projr[["directories-default"]][["data_raw"]] <- list(
        path = "/tmp/testDataRawABC1902", ignore = TRUE
      )
      .projr_yml_set(yml_projr)

      expect_identical(projr_dir_get("data_raw"), "/tmp/testDataRawABC1902")
      expect_identical(
        projr_dir_get("data_raw", path_relative_force = TRUE),
        as.character(fs::path_rel("/tmp/testDataRawABC1902", dir_test))
      )
      expect_identical(
        projr_dir_get("bookdown", "abc"), "docs/reportV0.0.0-1/abc"
      )
      expect_identical(
        projr_dir_get("data_raw", "abc"), "/tmp/testDataRawABC1902/abc"
      )
      expect_identical(
        projr_dir_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_dir_get("archive", "abc"), "_archive/abc"
      )
      expect_identical(
        projr_dir_get("output", "abc", output_safe = TRUE),
        "_tmp/projr_output/0.0.0-1/abc"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_path_get works", {
  dir_test <- file.path(tempdir(), paste0("report"))

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


  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(projr_path_get("abc"))
      expect_identical(projr_path_get("data_raw"), "_data_raw")
      expect_identical(projr_path_get("output"), "_tmp/projr_output/0.0.0-1")
      expect_identical(projr_path_get(
        "output",
        output_safe = TRUE,
        create = FALSE,
        path_relative_force = FALSE
      ), "_tmp/projr_output/0.0.0-1")
      expect_identical(projr_path_get("output", output_safe = FALSE), "_output")
      expect_identical(projr_path_get("archive"), "_archive")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_path_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_path_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get()
      yml_projr[["directories-default"]][["data_raw"]] <- list(
        path = "/tmp/testDataRawABC1902", ignore = TRUE
      )
      .projr_yml_set(yml_projr)

      expect_identical(projr_path_get("data_raw"), "/tmp/testDataRawABC1902")
      expect_identical(
        projr_path_get("data_raw", path_relative_force = TRUE),
        as.character(fs::path_rel("/tmp/testDataRawABC1902", dir_test))
      )
      expect_identical(
        projr_path_get("bookdown", "abc"), "docs/reportV0.0.0-1/abc"
      )
      expect_identical(
        projr_path_get("data_raw", "abc"), "/tmp/testDataRawABC1902/abc"
      )
      expect_identical(
        projr_path_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_path_get("archive", "abc"), "_archive/abc"
      )
      expect_true(fs::is_dir(dirname((projr_path_get("archive", "abc")))))
      expect_true(dir.exists(dirname(projr_path_get("archive", "abc"))))
      expect_true(!file.exists(projr_path_get("archive", "abc")))
      expect_identical(
        projr_path_get("output", "abc", output_safe = TRUE),
        "_tmp/projr_output/0.0.0-1/abc"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})


test_that("projr_dir_create works", {
  dir_test <- file.path(tempdir(), paste0("report"))

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
  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(projr_dir_create())
      expect_error(projr_dir_create("abc"))
      expect_error(projr_dir_create(TRUE))

      projr_dir_create("data_raw")
      expect_true(dir.exists("_data_raw"))
      projr_dir_create("archive")
      expect_true(dir.exists("_archive"))
      expect_true(dir.exists(file.path("_archive", "v0.0.0-1")))
      projr_dir_create("output")
      expect_true(dir.exists("_output"))
      projr_dir_create("cache")
      expect_true(dir.exists("_tmp"))

      unlink("_tmp", recursive = TRUE)
      unlink("_output", recursive = TRUE)
      projr_dir_create(c("cache", "output"))
      expect_true(dir.exists("_tmp"))
      expect_true(dir.exists("_output"))

      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("projr_dir_ignore works", {
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

  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))


  usethis::with_project(
    path = dir_test,
    code = {
      # test adding to gitignore and buildignore
      gitignore_orig <- .projr_gitignore_get()
      buildignore_orig <- .projr_buildignore_get()
      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("output")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("archive")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("cache")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)
      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)

      yml_projr <- .projr_yml_get()
      for (i in seq_along(yml_projr[["directories-default"]])) {
        yml_projr[["directories-default"]][[i]][["ignore"]] <- FALSE
      }
      .projr_yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .projr_dir_ignore("data_raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 0L)
      buildignore <- .projr_buildignore_get()

      # test not adding when the directory is not in wd
      yml_projr <- .projr_yml_get()
      dir_out <- file.path(
        dirname(rprojroot::is_r_package$find_file()), "test_2"
      )
      if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

      for (i in seq_along(yml_projr[["directories-default"]])) {
        yml_projr[["directories-default"]][[i]][["path"]] <- dir_out
      }
      .projr_yml_set(yml_projr)
      gitignore <- .projr_gitignore_get()
      expect_identical(length(
        which(gitignore == "/tmp/RtmpkdBxQ9/test_2/**/*")
      ), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(
        which(buildignore == "^/tmp/RtmpkdBxQ9/test_2")
      ), 0L)
      buildignore <- .projr_buildignore_get()

      # test errors
      expect_error(.projr_dir_ignore(c("abc", "def")))
      expect_error(.projr_dir_ignore(1))
      yml_projr <- .projr_yml_get()
      for (i in seq_along(yml_projr[["directories-default"]])) {
        yml_projr[["directories-default"]][[i]][["ignore"]] <- 1
      }
      names(yml_projr[["directories-default"]]) <- rep("data_raw", 4)
      .projr_yml_set(yml_projr)
      expect_error(.projr_dir_ignore("data_raw"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
