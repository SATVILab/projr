test_that("getting and setting metadata files works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))
  fn_vec <- fn_vec

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
      expect_identical(class(.projr_yml_get()), "list")
      expect_identical(class(.projr_yml_bd_get()), "list")
      expect_identical(class(.projr_desc_get()), c("matrix", "array"))
      .projr_yml_set(list("directories-default" = NULL))
      expect_identical(.projr_yml_get(), list("directories-default" = NULL))
      .projr_yml_bd_set(list())
      expect_identical(.projr_yml_bd_get(), list())
      unlink(file.path(dir_test, "_projr.yml"))
      unlink(file.path(dir_test, "_bookdown.yml"))
      expect_error(.projr_yml_get())
      expect_error(.projr_yml_bd_get())
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("getting active yml file works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

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
      yml_projr_default <- list(
        `directories` = list(
          data_raw = list(
            path = "_data_raw",
            ignore = TRUE
          ), cache = list(path = "_tmp", ignore = TRUE),
          output = list(path = "_output", ignore = TRUE), archive = list(
            path = "_archive", ignore = TRUE
          )
        ), `version-format` = "major.minor.patch-dev",
        `build-dev` = list(
          `bump-version` = FALSE, rmd = NULL, `copy-to-output` = FALSE
        ),
        `build-output` = list(
          renv = TRUE, `copy-to-output` = list(
            data_raw = FALSE,
            cache = FALSE, bookdown = TRUE, package = FALSE
          )
        )
      )
      expect_identical(projr_yml_get(), yml_projr_default)
      projr_profile_create()
      yml_projr <- .projr_yml_get()
      list_non_default <- list(
        data_raw = list(path = "abc"),
        cache = list(path = "def"),
        output = list(path = "ghi"),
        archive = list(path = "jkl")
      )
      yml_projr[[paste0("directories-", projr_profile_get())]] <-
        list_non_default
      .projr_yml_set(yml_projr)
      dir_active <- projr_yml_get()[["directories"]]
      expected_dirs_list <- list(
        data_raw = list(path = "abc", ignore = TRUE),
        cache = list(path = "def", ignore = TRUE),
        output = list(path = "ghi", ignore = TRUE),
        archive = list(path = "jkl", ignore = TRUE)
      )
      expect_identical(projr_yml_get()[["directories"]], expected_dirs_list)
      yml_projr <- yml_projr[-which(names(yml_projr) == "directories-default")]
      .projr_yml_set(yml_projr)
      expect_error(.projr_yml_get())
    },
    quiet = TRUE,
    force = TRUE
  )
  unlink(dir_test, recursive = TRUE)
})

test_that("Further tests for projr_yml_get", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))

  if (!dir.exists(dir_test)) dir.create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

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
      yml_active <- projr_yml_get()
      expect_true(sum(grepl("^directories", names(yml_active))) == 1)
      projr_profile_create()
      yml <- .projr_yml_get()
      expect_true(sum(grepl("^directories", names(yml))) == 2)
      yml_active <- projr_yml_get()
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
