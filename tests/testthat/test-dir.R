
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
  Sys.setenv("PROJR_TEST" = "TRUE")

  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      projr_dir_get("project")
      yml_projr <- .projr_yml_get_root_full()
      expect_error(projr_dir_get("ailc"))
      expect_identical(projr_dir_get("data-raw"), "_data_raw")
      expect_identical(projr_dir_get("output"), "_tmp/projr-output/0.0.0-1")
      expect_identical(projr_dir_get(
        "output",
        output_safe = TRUE,
        create = FALSE,
        path_relative_force = FALSE
      ), "_tmp/projr-output/0.0.0-1")
      expect_identical(projr_dir_get("output", output_safe = FALSE), "_output")
      expect_identical(projr_dir_get("archive"), "_archive")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_dir_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_dir_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get_root_full()
      path_tmp_data_raw <- file.path(tempdir(), "testDataRawABC1902")
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = path_tmp_data_raw,
        ignore = TRUE
      )
      .projr_yml_set(yml_projr)
      expect_identical(
        projr_dir_get("data-raw"),
        path_tmp_data_raw
      )

      expect_identical(
        projr_dir_get("data-raw", path_relative_force = TRUE),
        as.character(
          fs::path_rel(
            path_tmp_data_raw,
            dir_test
          )
        )
      )
      expect_identical(
        projr_dir_get("docs", "abc"), "docs/abc"
      )
      expect_identical(
        projr_dir_get("data-raw", "abc"),
        file.path(path_tmp_data_raw, "abc")
      )
      expect_identical(
        projr_dir_get("data-raw", "abc", "def", "ghi"),
        file.path(path_tmp_data_raw, "abc/def/ghi") |>
          fs::path_norm() |>
          as.character()
      )
      expect_identical(
        projr_dir_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_dir_get("archive", "abc"), "_archive/abc"
      )
      expect_identical(
        projr_dir_get("output", "abc", output_safe = TRUE),
        "_tmp/projr-output/0.0.0-1/abc"
      )
      expect_identical(
        projr_dir_get("cache", "fig", "intro", "p"),
        "_tmp/fig/intro/p"
      )
      # many levels
      expect_identical(
        projr_dir_get("cache", "fig", "intro", "a", "b", "c", "d", "e", "f"),
        "_tmp/fig/intro/a/b/c/d/e/f"
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
      expect_identical(
        projr_path_get("cache", "fig", "intro", "p.png"),
        "_tmp/fig/intro/p.png"
      )

      expect_error(projr_path_get("abc"))
      expect_identical(projr_path_get("data-raw"), "_data_raw")
      expect_identical(projr_path_get("output"), "_tmp/projr-output/0.0.0-1")
      expect_identical(projr_path_get(
        "output",
        output_safe = TRUE,
        create = FALSE,
        path_relative_force = FALSE
      ), "_tmp/projr-output/0.0.0-1")
      expect_identical(projr_path_get("output", output_safe = FALSE), "_output")
      expect_identical(projr_path_get("archive"), "_archive")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_path_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_path_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get_root_full()
      path_data_raw_abs <- fs::path_abs(dirname(dirname(getwd()))) |>
        as.character()
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = path_data_raw_abs, ignore = TRUE
      )
      .projr_yml_set(yml_projr)

      expect_identical(projr_path_get("data-raw"), path_data_raw_abs)
      expect_identical(
        projr_path_get("data-raw", path_relative_force = TRUE),
        as.character(
          fs::path_rel(path_data_raw_abs, dir_test) |>
          fs::path_norm()
          )
      )
      expect_identical(
        projr_path_get("docs", "abc"), "docs/reportV0.0.0-1/abc"
      )
      expect_identical(
        projr_path_get("data-raw", "abc"),
        file.path(path_data_raw_abs, "abc")
      )
      expect_identical(
        projr_path_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_path_get("archive", "abc"), "_archive/abc"
      )
      expect_identical(
        projr_path_get("data-raw", "abc", "def", "ghi"),
        file.path(path_data_raw_abs, "abc/def/ghi") |>
          fs::path_norm() |>
          as.character()
      )
      expect_true(
        fs::is_dir(dirname((projr_path_get("archive", "abc", "def"))))
      )
      expect_true(dir.exists(dirname(projr_path_get("archive", "abc"))))
      expect_true(!file.exists(projr_path_get("archive", "abc", "def")))
      expect_identical(
        projr_path_get("output", "abc", output_safe = TRUE),
        "_tmp/projr-output/0.0.0-1/abc"
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

      projr_dir_create("data-raw")
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
      .projr_dir_ignore("docs")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(
        gitignore == "docs/reportV0.0.0-1/**/*"
      )), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(
        buildignore == "^docs/reportV0\\.0\\.0-1"
      )), 1L)
      .projr_dir_ignore("data-raw")
      # test that nothing is done when directory is equal to working directory
      .projr_gitignore_set(gitignore_orig, append = FALSE)
      yml_bd_init <- .projr_yml_bd_get()
      yml_bd <- yml_bd_init
      yml_bd[["output_dir"]] <- "."
      .projr_yml_bd_set(yml_bd)
      .projr_dir_ignore("docs")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(
        gitignore == "."
      )), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(
        buildignore == "^\\."
      )), 0L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("output")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("archive")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("cache")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)

      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore"]] <- FALSE
      }
      .projr_yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 0L)
      buildignore <- .projr_buildignore_get()

      # test not adding when the directory is not in wd
      yml_projr <- .projr_yml_get_root_full()
      dir_out <- file.path(
        dirname(rprojroot::is_r_package$find_file()), "test_2"
      )
      if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["path"]] <- dir_out
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
      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore"]] <- 1
      }
      names(yml_projr[["directories"]]) <- rep("data-raw", 4)
      .projr_yml_set(yml_projr)
      expect_error(.projr_dir_ignore("data-raw"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that(".projr_dir_clear works", {
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
  Sys.setenv("PROJR_TEST" = "TRUE")

  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      expect_error(.projr_dir_clear(dir_test))
      expect_true(.projr_dir_clear(file.path(dir_test, "abc")))

      # not deleting directories
      dir_cache_sub <- projr_dir_get("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      .projr_dir_clear(
        path_dir = projr_dir_get("cache"),
        delete_directories = FALSE
      )
      expect_true(dir.exists((dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))


      dir_cache_sub <- projr_dir_get("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      path_cache_fn <- projr_path_get("cache", "test2.txt")
      invisible(file.create(path_cache_fn))
      .projr_dir_clear(
        path_dir = projr_dir_get("cache"),
        delete_directories = TRUE
      )
      expect_false(dir.exists((dir_cache_sub)))
      expect_true(dir.exists(dirname(dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      expect_false(file.exists(path_cache_fn))
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
      .projr_dir_ignore("docs")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(
        gitignore == "docs/reportV0.0.0-1/**/*"
      )), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(
        buildignore == "^docs/reportV0\\.0\\.0-1"
      )), 1L)
      .projr_dir_ignore("data-raw")
      # test that nothing is done when directory is equal to working directory
      .projr_gitignore_set(gitignore_orig, append = FALSE)
      yml_bd_init <- .projr_yml_bd_get()
      yml_bd <- yml_bd_init
      yml_bd[["output_dir"]] <- "."
      .projr_yml_bd_set(yml_bd)
      .projr_dir_ignore("docs")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(
        gitignore == "."
      )), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(
        buildignore == "^\\."
      )), 0L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 1L)
      .projr_dir_ignore("output")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_output/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_output")), 1L)
      .projr_dir_ignore("archive")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_archive/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_archive")), 1L)
      .projr_dir_ignore("cache")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)
      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_tmp/**/*")), 1L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_tmp")), 1L)

      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore"]] <- FALSE
      }
      .projr_yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .projr_dir_ignore("data-raw")
      gitignore <- .projr_gitignore_get()
      expect_identical(length(which(gitignore == "_data_raw/**/*")), 0L)
      buildignore <- .projr_buildignore_get()
      expect_identical(length(which(buildignore == "^_data_raw")), 0L)
      buildignore <- .projr_buildignore_get()

      # test not adding when the directory is not in wd
      yml_projr <- .projr_yml_get_root_full()
      dir_out <- file.path(
        dirname(rprojroot::is_r_package$find_file()), "test_2"
      )
      if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["path"]] <- dir_out
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
      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore"]] <- 1
      }
      names(yml_projr[["directories"]]) <- rep("data-raw", 4)
      .projr_yml_set(yml_projr)
      expect_error(.projr_dir_ignore("data-raw"))
    },
    force = TRUE,
    quiet = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})
