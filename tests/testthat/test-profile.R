test_that("projr_profile_get, _set and _create work", {
  Sys.setenv("PROJR_TEST" = "TRUE")
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(Sys.unsetenv("PROJR_TEST"))
  withr::defer(unlink(dir_test, recursive = TRUE))
  .projr_dir_create(dir_test)
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
      yml_projr_init <- .projr_yml_get_root_full()
      # test getting input and validating
      # --------------------------------
      expect_error(.projr_profile_create(silent = "wingbats"))
      yml_projr_init <- .projr_yml_get_root_full()
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")

      expect_identical(projr_profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      # test "protected" profiles
      expect_error(projr_profile_create("local"))
      expect_error(projr_profile_create("default"))
      expect_error(projr_profile_create())
      expect_error(projr_profile_create(profile = "!"))

      Sys.setenv("PROJR_TEST" = "FALSE")
      suppressMessages(
        expect_message(
          projr_profile_create(
            profile = "test_a-bc", silent = FALSE
          ),
          "Added the following profile: test_a-bc"
        )
      )
      suppressMessages(
        expect_message(
          projr_profile_create(
            profile = "test_a-bc", silent = FALSE
          ),
          "projr profile test_a-bc already exists"
        )
      )
      expect_false(
        suppressMessages(
          projr_profile_create(profile = "test_a-bc")
        )
      )
      Sys.setenv("PROJR_TEST" = "TRUE")

      # test creating profile
      # -----------------------

      # file method
      projr_profile_create(profile = "test-profile2")
      yml_projr <- .projr_yml_get_root_full()
      expect_true(file.exists("_projr-test-profile2.yml"))
      .projr_yml_set(yml_projr_init)

      # test getting a profile
      # ---------------------

      # default
      projr_profile <- projr_profile_get()
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical(projr_profile_get(), "abc")
      # PROJR_PROFILE that exists
      projr_profile_create(profile = "abc")
      expect_identical(projr_profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      .projr_yml_set(yml_projr_init)
      invisible(file.remove("_projr-abc.yml"))
      # working directory
      expect_identical(
        projr_profile_get(), "default"
      )
      # test precedence works correctly
      projr_profile_create("test_profile_hidden")
      # should get the projr profile back
      Sys.setenv("PROJR_PROFILE" = "test_profile_hidden")
      expect_identical(projr_profile_get(), "test_profile_hidden")

      # delecting a a projr profile
      # ------------------------
      .projr_yml_set(yml_projr_init)

      # input validation
      expect_error(projr_profile_delete(profile = 1))
      expect_error(projr_profile_delete())
      # default profile does nothing
      expect_error(projr_profile_delete("default"))

      # deleting it anywhere
      Sys.setenv("PROJR_PROFILE" = "xyz")
      projr_profile_create()
      expect_true(file.exists("_projr-xyz.yml"))
      projr_profile_delete(projr_profile_get())
      expect_false(file.exists("_projr-xyz.yml"))
      Sys.unsetenv("PROJR_PROFILE")
      Sys.unsetenv("PROJR_TEST")
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_create_local works", {
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))

  .projr_dir_create(dir_test)
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
      projr_profile_create_local()
      expect_true(file.exists("_projr-local.yml"))
      expect_error(.projr_profile_create_local())
      yml_projr_local <- yaml::read_yaml("_projr-local.yml")
      expect_true(
        is.null(yml_projr_local[["directories"]][["data-raw"]][["path"]])
      )
      expect_true(all(names(yml_projr_local) == c("directories", "build")))
      rbuildignore <- readLines(.projr_dir_proj_get(".Rbuildignore"))
      expect_true("^_projr-local\\.yml$" %in% rbuildignore)
      gitignore <- readLines(.projr_dir_proj_get(".gitignore"))
      expect_true("_projr-local.yml" %in% gitignore)
    },
    quiet = TRUE,
    force = TRUE
  )
})
