test_that("projr_dir_get works", {
  # setup
  dir_test <- .projr_test_setup_project(git = TRUE, set_env_var = TRUE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # ensure a docs bug is fixed
      expect_identical(
        projr_path_get_dir("docs"), "_tmp/projr/v0.0.0-1/_book"
      )
      expect_identical(
        .projr_yml_bd_get()[["output_dir"]], "_tmp/projr/v0.0.0-1/_book"
      )

      projr_init()
      projr_path_get_dir("project")
      yml_projr <- .projr_yml_get_root_full()
      expect_error(projr_dir_get("ailc"))
      expect_identical(projr_dir_get("data-raw"), "_data_raw")
      expect_identical(projr_dir_get("output"), "_tmp/projr/v0.0.0-1/output")
      expect_identical(projr_dir_get(
        "output",
        safe = TRUE,
        create = FALSE,
        relative = FALSE
      ), "_tmp/projr/v0.0.0-1/output")
      expect_identical(projr_dir_get("output", safe = FALSE), "_output")
      expect_identical(projr_dir_get("archive"), "_tmp/projr/v0.0.0-1/archive")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_dir_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_path_get_dir("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get_root_full()
      path_tmp_data_raw <- fs::path_abs(dirname(dirname(getwd()))) |>
        as.character()
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = path_tmp_data_raw,
        "ignore-git" = TRUE
      )
      .projr_yml_set(yml_projr)
      expect_identical(
        projr_path_get_dir("data-raw"),
        path_tmp_data_raw
      )

      expect_identical(
        projr_path_get_dir("data-raw", relative = TRUE),
        "../.."
      )

      expect_identical(
        projr_path_get_dir("docs", "abc"), "_tmp/projr/v0.0.0-1/_book/abc"
      )
      expect_identical(
        projr_path_get_dir("docs", "abc", safe = FALSE), "_book/abc"
      )
      expect_identical(
        projr_path_get_dir("data-raw", "abc"),
        file.path(path_tmp_data_raw, "abc")
      )
      expect_identical(
        projr_path_get_dir("data-raw", "abc", "def", "ghi"),
        file.path(path_tmp_data_raw, "abc/def/ghi") |>
          fs::path_norm() |>
          as.character()
      )
      expect_identical(
        projr_path_get_dir("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_path_get_dir("archive", "abc"), "_tmp/projr/v0.0.0-1/archive/abc"
      )
      expect_identical(
        projr_path_get_dir("archive", "abc", safe = FALSE),
        "_archive/v0.0.0-1/abc"
      )
      expect_identical(
        projr_path_get_dir("output", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/output/abc"
      )
      expect_identical(
        projr_path_get_dir("cache", "fig", "intro", "p"),
        "_tmp/fig/intro/p"
      )
      # many levels
      expect_identical(
        projr_path_get_dir(
          "cache", "fig", "intro", "a", "b", "c", "d", "e", "f"
        ),
        "_tmp/fig/intro/a/b/c/d/e/f"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_path_get works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
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
  if (!requireNamespace("gert", quietly = TRUE)) {
    utils::install.packages("gert")
  }

  gert::git_init(path = dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      expect_identical(
        projr_path_get("cache", "fig", "intro", "p.png"),
        "_tmp/fig/intro/p.png"
      )

      expect_error(projr_path_get("abc"))
      expect_identical(projr_path_get("data-raw"), "_data_raw")
      expect_identical(projr_path_get("output"), "_tmp/projr/v0.0.0-1/output")
      expect_identical(projr_path_get(
        "output",
        safe = TRUE,
        create = FALSE,
        relative = FALSE
      ), "_tmp/projr/v0.0.0-1/output")
      expect_identical(projr_path_get("output", safe = FALSE), "_output")
      expect_identical(
        projr_path_get("archive", safe = FALSE), "_archive/v0.0.0-1"
      )
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical(projr_path_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
      projr_path_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .projr_yml_get_root_full()
      path_data_raw_abs <- fs::path_abs(dirname(dirname(getwd()))) |>
        as.character()
      yml_projr[["directories"]][["data-raw"]] <- list(
        path = path_data_raw_abs, "ignore-git" = TRUE
      )
      .projr_yml_set(yml_projr)

      expect_identical(projr_path_get("data-raw"), path_data_raw_abs)
      expect_identical(
        projr_path_get("data-raw", relative = TRUE),
        "../.."
      )

      expect_identical(
        projr_path_get("docs", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/_book/abc"
      )
      expect_identical(
        projr_path_get("docs", "abc", safe = FALSE), "_book/abc"
      )
      expect_identical(
        projr_path_get("data-raw", "abc"),
        file.path(path_data_raw_abs, "abc")
      )
      expect_identical(
        projr_path_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
        projr_path_get("archive", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/archive/abc"
      )
      expect_identical(
        projr_path_get("archive", "abc", safe = FALSE),
        "_archive/v0.0.0-1/abc"
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
        projr_path_get("output", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/output/abc"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_dir_create works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
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
  gert::git_init(path = dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(projr_dir_create())
      expect_error(projr_dir_create("abc"))
      expect_error(projr_dir_create(TRUE))

      projr_dir_create("data-raw")
      expect_true(dir.exists("_data_raw"))
      projr_dir_create("archive", safe = FALSE)
      expect_true(dir.exists("_archive"))
      expect_true(dir.exists(file.path("_archive", "v0.0.0-1")))
      projr_dir_create("output")
      expect_false(dir.exists("_output"))
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/output"))
      projr_dir_create("output", safe = FALSE)
      expect_true(dir.exists("_output"))
      projr_dir_create("cache")
      expect_true(dir.exists("_tmp"))

      unlink("_tmp", recursive = TRUE)
      unlink("_output", recursive = TRUE)
      projr_dir_create(c("cache", "output"), safe = FALSE)
      expect_true(dir.exists("_tmp"))
      expect_true(dir.exists("_output"))
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_data_raw/**")), 0L)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_dir_ignore works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))
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

  gert::git_init(path = dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      # test adding to gitignore and buildignore
      gitignore_orig <- .projr_ignore_git_read()
      buildignore_orig <- .projr_ignore_rbuild_read()
      .projr_ignore_label_set("docs")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(
        gitignore == "_book/**"
      )), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^_book/"
      )), 1L)
      expect_identical(length(which(
        buildignore == "^_book$"
      )), 1L)
      .projr_ignore_label_set("data-raw")
      # test that nothing is done when directory is equal to working directory
      .projr_ignore_git_write(gitignore_orig, append = FALSE)
      yml_bd_init <- .projr_yml_bd_get()
      yml_bd <- yml_bd_init
      yml_bd[["output_dir"]] <- "."
      .projr_yml_bd_set(yml_bd)
      .projr_ignore_label_set("docs")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(
        gitignore == "."
      )), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^\\."
      )), 0L)
      .projr_ignore_label_set("data-raw")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_data_raw/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_data_raw/")), 1L)
      expect_identical(length(which(buildignore == "^_data_raw$")), 1L)
      .projr_ignore_label_set("output")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_output/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_output$")), 1L)
      expect_identical(length(which(buildignore == "^_output/")), 1L)
      .projr_ignore_label_set("archive")
      .projr_ignore_label_set("archive")
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "_archive/**")), 1L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_archive/")), 1L)
      expect_identical(length(which(buildignore == "^_archive$")), 1L)

      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore-git"]] <- FALSE
        yml_projr[["directories"]][[i]][["ignore-rbuild"]] <- FALSE
      }
      .projr_yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .projr_ignore_label_set("data-raw")
      yml_gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(gitignore == "^_data_raw/**")), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_data_raw/")), 0L)
      expect_identical(length(which(buildignore == "^_data_raw$")), 0L)

      # test not adding when the directory is not in wd
      yml_projr <- .projr_yml_get_root_full()
      dir_out <- .dir_proj_get("test_2")
      .dir_create(dir_out)

      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["path"]] <- dir_out
      }
      .projr_yml_set(yml_projr)
      gitignore <- .projr_ignore_git_read()
      expect_identical(length(which(grepl("test_2", gitignore))), 0L)
      buildignore <- .projr_ignore_rbuild_read()
      expect_identical(length(which(grepl("test_2", buildignore))), 0L)
      buildignore <- .projr_ignore_rbuild_read()

      # test errors
      expect_error(.projr_ignore_label_set(c("abc", "def")))
      expect_error(.projr_ignore_label_set(1))
      yml_projr <- .projr_yml_get_root_full()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore-git"]] <- 1
        yml_projr[["directories"]][[i]][["ignore-rbuild"]] <- 1
      }
      names(yml_projr[["directories"]]) <- rep("data-raw", 4)
      .projr_yml_set(yml_projr)
      expect_error(.projr_ignore_label_set("data-raw"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".projr_dir_clear works", {
  dir_test <- file.path(tempdir(), paste0("report"))

  .dir_create(dir_test)
  withr::defer(unlink(dir_test, recursive = TRUE))

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
  .test_set()
  gert::git_init(path = dir_test)

  usethis::with_project(
    path = dir_test,
    code = {
      projr_init()
      expect_error(.dir_clear(dir_test))
      expect_false(.dir_clear(file.path(dir_test, "abc")))

      # not deleting directories
      dir_cache_sub <- projr_path_get_dir("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      .projr_dir_clear_file(path = projr_path_get_dir("cache"))
      expect_true(dir.exists((dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      .dir_clear(
        path_dir = projr_path_get_dir("cache"),
      )
      expect_false(dir.exists((dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      expect_true(dir.exists(projr_path_get_dir("cache")))


      dir_cache_sub <- projr_path_get_dir("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      path_cache_fn <- projr_path_get("cache", "test2.txt")
      invisible(file.create(path_cache_fn))
      .dir_clear(
        path_dir = projr_path_get_dir("cache")
      )
      expect_false(dir.exists((dir_cache_sub)))
      expect_true(dir.exists(dirname(dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      expect_false(file.exists(path_cache_fn))
    },
    force = TRUE,
    quiet = TRUE
  )
})
