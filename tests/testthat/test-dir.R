test_that("projr_path_get_dir works", {
  # setup
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = TRUE, set_env_var = TRUE)

  # run from within project
  usethis::with_project(
    path = dir_test,
    code = {
      # ensure a docs bug is fixed
      expect_identical(
       .path_get_dir("docs"), "_tmp/projr/v0.0.0-1/_book"
      )
      expect_identical(
        .yml_bd_get()[["output_dir"]], "_tmp/projr/v0.0.0-1/_book"
      )

     .init()
     .path_get_dir("project")
      yml_projr <- .yml_get_default_raw()
      expect_error.path_get_dir("ailc"))
      expect_identical.path_get_dir("raw-data"), "_raw_data")
      expect_identical(
       .path_get_dir("output"), "_tmp/projr/v0.0.0-1/output"
      )
      expect_identical.path_get_dir(
        "output",
        safe = TRUE,
        create = FALSE,
        relative = FALSE
      ), "_tmp/projr/v0.0.0-1/output")
      expect_identical.path_get_dir("output", safe = FALSE), "_output")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical.path_get_dir("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
     .path_get_dir("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .yml_get_default_raw()
      path_tmp_raw_data <- fs::path_abs(dirname(dirname(getwd()))) |>
        as.character()
      yml_projr[["directories"]][["raw-data"]] <- list(
        path = path_tmp_raw_data,
        "ignore-git" = TRUE
      )
      .yml_set(yml_projr)
      expect_identical(
       .path_get_dir("raw-data"),
        path_tmp_raw_data
      )

      expect_identical(
       .path_get_dir("raw-data", relative = TRUE),
        "../.."
      )

      expect_identical(
       .path_get_dir("docs", "abc"), "_tmp/projr/v0.0.0-1/_book/abc"
      )
      expect_identical(
       .path_get_dir("docs", "abc", safe = FALSE), "_book/abc"
      )
      expect_identical(
       .path_get_dir("raw-data", "abc"),
        file.path(path_tmp_raw_data, "abc")
      )
      expect_identical(
       .path_get_dir("raw-data", "abc", "def", "ghi"),
        file.path(path_tmp_raw_data, "abc/def/ghi") |>
          fs::path_norm() |>
          as.character()
      )
      expect_identical(
       .path_get_dir("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
       .path_get_dir("output", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/output/abc"
      )
      expect_identical(
       .path_get_dir("cache", "fig", "intro", "p"),
        "_tmp/fig/intro/p"
      )
      # many levels
      expect_identical(
       .path_get_dir(
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
  skip_if(.is_test_select())
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
       .path_get("cache", "fig", "intro", "p.png"),
        "_tmp/fig/intro/p.png"
      )

      expect_error.path_get("abc"))
      expect_identical.path_get("raw-data"), "_raw_data")
      expect_identical.path_get("output"), "_tmp/projr/v0.0.0-1/output")
      expect_identical.path_get(
        "output",
        safe = TRUE,
        create = FALSE,
        relative = FALSE
      ), "_tmp/projr/v0.0.0-1/output")
      expect_identical.path_get("output", safe = FALSE), "_output")
      if (dir.exists("_tmp")) unlink("_tmp", recursive = TRUE)
      expect_identical.path_get("cache", create = FALSE), "_tmp")
      expect_true(!dir.exists("_tmp"))
     .path_get("cache", create = TRUE)
      expect_true(dir.exists("_tmp"))
      yml_projr <- .yml_get_default_raw()
      path_raw_data_abc <- fs::path_abs(dirname(dirname(getwd()))) |>
        as.character()
      yml_projr[["directories"]][["raw-data"]] <- list(
        path = path_raw_data_abc, "ignore-git" = TRUE
      )
      .yml_set(yml_projr)

      expect_identical.path_get("raw-data"), path_raw_data_abc)
      expect_identical(
       .path_get("raw-data", relative = TRUE),
        "../.."
      )

      expect_identical(
       .path_get("docs", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/_book/abc"
      )
      expect_identical(
       .path_get("docs", "abc", safe = FALSE), "_book/abc"
      )
      expect_identical(
       .path_get("raw-data", "abc"),
        file.path(path_raw_data_abc, "abc")
      )
      expect_identical(
       .path_get("cache", "abc"), "_tmp/abc"
      )
      expect_identical(
       .path_get("raw-data", "abc", "def", "ghi"),
        file.path(path_raw_data_abc, "abc/def/ghi") |>
          fs::path_norm() |>
          as.character()
      )
      expect_identical(
       .path_get("output", "abc", safe = TRUE),
        "_tmp/projr/v0.0.0-1/output/abc"
      )
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_create works", {
  skip_if(.is_test_select())
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
      expect_error(.dir_create())
      expect_error(.dir_create("abc"))
      expect_error(.dir_create(TRUE))

      .dir_create("raw-data")
      expect_true(dir.exists("_raw_data"))
      .dir_create("output")
      expect_false(dir.exists("_output"))
      expect_true(dir.exists("_tmp/projr/v0.0.0-1/output"))
      .dir_create("output", safe = FALSE)
      expect_true(dir.exists("_output"))
      .dir_create("cache")
      expect_true(dir.exists("_tmp"))

      unlink("_tmp", recursive = TRUE)
      unlink("_output", recursive = TRUE)
      .dir_create(c("cache", "output"), safe = FALSE)
      expect_true(dir.exists("_tmp"))
      expect_true(dir.exists("_output"))
      gitignore <- .ignore_git_read()
      expect_identical(length(which(gitignore == "_raw_data/**")), 0L)
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that("projr_dir_ignore works", {
  skip_if(.is_test_select())
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
      gitignore_orig <- .ignore_git_read()
      buildignore_orig <- .ignore_rbuild_read()
      .ignore_label_set("docs")
      gitignore <- .ignore_git_read()
      expect_identical(length(which(
        gitignore == "_book/**"
      )), 1L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^_book/"
      )), 1L)
      expect_identical(length(which(
        buildignore == "^_book$"
      )), 1L)
      .ignore_label_set("raw-data")
      # test that nothing is done when directory is equal to working directory
      .ignore_git_write(gitignore_orig, append = FALSE)
      yml_bd_init <- .yml_bd_get()
      yml_bd <- yml_bd_init
      yml_bd[["output_dir"]] <- "."
      .yml_bd_set(yml_bd)
      .ignore_label_set("docs")
      gitignore <- .ignore_git_read()
      expect_identical(length(which(
        gitignore == "."
      )), 0L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(
        buildignore == "^\\."
      )), 0L)
      .ignore_label_set("raw-data")
      gitignore <- .ignore_git_read()
      expect_identical(length(which(gitignore == "_raw_data/**")), 1L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_raw_data/")), 1L)
      expect_identical(length(which(buildignore == "^_raw_data$")), 1L)
      .ignore_label_set("output")
      gitignore <- .ignore_git_read()
      expect_identical(length(which(gitignore == "_output/**")), 1L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_output$")), 1L)
      expect_identical(length(which(buildignore == "^_output/")), 1L)
      gitignore <- .ignore_git_read()
      buildignore <- .ignore_rbuild_read()

      yml_projr <- .yml_get_default_raw()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore-git"]] <- FALSE
        yml_projr[["directories"]][[i]][["ignore-rbuild"]] <- FALSE
      }
      .yml_set(yml_projr)
      # test taking away from gitignore and buildignore

      .ignore_label_set("raw-data")
      yml_gitignore <- .ignore_git_read()
      expect_identical(length(which(gitignore == "^_raw_data/**")), 0L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(buildignore == "^_raw_data/")), 0L)
      expect_identical(length(which(buildignore == "^_raw_data$")), 0L)

      # test not adding when the directory is not in wd
      yml_projr <- .yml_get_default_raw()
      dir_out <- .path_get("test_2")
      .dir_create(dir_out)

      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["path"]] <- dir_out
      }
      .yml_set(yml_projr)
      gitignore <- .ignore_git_read()
      expect_identical(length(which(grepl("test_2", gitignore))), 0L)
      buildignore <- .ignore_rbuild_read()
      expect_identical(length(which(grepl("test_2", buildignore))), 0L)
      buildignore <- .ignore_rbuild_read()

      # test errors
      expect_error(.ignore_label_set(c("abc", "def")))
      expect_error(.ignore_label_set(1))
      yml_projr <- .yml_get_default_raw()
      for (i in seq_along(yml_projr[["directories"]])) {
        yml_projr[["directories"]][[i]][["ignore-git"]] <- 1
        yml_projr[["directories"]][[i]][["ignore-rbuild"]] <- 1
      }
      names(yml_projr[["directories"]]) <- rep("raw-data", 4)
      .yml_set(yml_projr)
      expect_error(.ignore_label_set("raw-data"))
    },
    force = TRUE,
    quiet = TRUE
  )
})

test_that(".dir_clear works", {
  skip_if(.is_test_select())
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
     .init()
      expect_error(.dir_clear(dir_test))
      expect_false(.dir_clear(file.path(dir_test, "abc")))

      # not deleting directories
      dir_cache_sub <-.path_get_dir("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      .dir_clear_file(path =.path_get_dir("cache"), recursive = TRUE)
      expect_true(dir.exists((dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      .dir_clear(
        path_dir =.path_get_dir("cache"),
      )
      expect_false(dir.exists((dir_cache_sub)))
      expect_false(file.exists(path_cache_sub_fn))
      expect_true(dir.exists.path_get_dir("cache")))


      dir_cache_sub <-.path_get_dir("cache", "sub")
      path_cache_sub_fn <- file.path(dir_cache_sub, "test.txt")
      invisible(file.create(path_cache_sub_fn))
      path_cache_fn <-.path_get("cache", "test2.txt")
      invisible(file.create(path_cache_fn))
      .dir_clear(
        path_dir =.path_get_dir("cache")
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
