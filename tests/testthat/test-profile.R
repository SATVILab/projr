test_that("projr_profile_get, _set and _create work", {
  Sys.setenv("PROJR_TEST" = "TRUE")
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(Sys.unsetenv("PROJR_TEST"))
  withr::defer(unlink(dir_test, recursive = TRUE))
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
      yml_projr_init <- .projr_yml_get_root_full()
      # test getting input and validating
      # --------------------------------
      expect_error(.projr_profile_create(silent = "wingbats"))
      yml_projr_init <- .projr_yml_get_root_full()
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical(projr_profile_get(), "default")
      Sys.unsetenv("PROJR_PROFILE")
      # test "protected" profiles
      expect_error(projr_profile_create("local"))
      expect_error(projr_profile_create("default"))
      # test valid methods
      expect_error(projr_profile_create("test", method = c("key", "file")))
      expect_error(projr_profile_create("test", method = c("file", "key")))
      expect_error(projr_profile_create("test", method = c("random")))
      # have to set PROJR_PROFILE or `projr` arg when using method = "file"
      expect_error(projr_profile_create(method = "file"))
      expect_error(projr_profile_create(profile = "!", method = "file"))

      suppressMessages(
        expect_message(
          projr_profile_create(
            profile = "test_a-bc", method = "file", silent = FALSE
          ),
          "Added the following profile: test_a-bc"
        )
      )
      suppressMessages(
        expect_message(
          projr_profile_create(
            profile = "test_a-bc", method = "file", silent = FALSE
          ),
          "projr profile test_a-bc already exists"
        )
      )
      expect_false(
        suppressMessages(
          projr_profile_create(profile = "test_a-bc", method = "file")
        )
      )
      expect_false(
        suppressMessages(
          projr_profile_create(profile = "test_a-bc", method = "key")
        )
      )
      .projr_yml_set(yml_projr_init)
      projr_profile_create(profile = "test_a-bc-2", method = "key")
      expect_false(
        suppressMessages(
          projr_profile_create(profile = "test_a-bc-2", method = "file")
        )
      )
      expect_false(
        suppressMessages(
          projr_profile_create(profile = "test_a-bc-2", method = "key")
        )
      )
      .projr_yml_set(yml_projr_init)

      # test creating profile
      # -----------------------

      # key method
      projr_profile_create(profile = "test-profile", method = "key")
      yml_projr <- .projr_yml_get_root_full()
      expect_true(
        paste0("directories-test-profile") %in%
          names(yml_projr)
      )
      expect_true(
        paste0("build-test-profile") %in%
          names(yml_projr)
      )
      .projr_yml_set(yml_projr_init)

      # file method
      projr_profile_create(profile = "test-profile2", method = "file")
      yml_projr <- .projr_yml_get_root_full()
      expect_true(file.exists("_projr-test-profile2.yml"))
      .projr_yml_set(yml_projr_init)

      # test getting a profile
      # ---------------------

      # default
      projr_profile <- projr_profile_get()
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical(projr_profile_get(), "default")
      # PROJR_PROFILE that exists
      projr_profile_create(profile = "abc", method = "file")
      expect_identical(projr_profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      .projr_yml_set(yml_projr_init)
      invisible(file.remove("_projr-abc.yml"))
      # working directory
      projr_profile_create()
      expect_identical(
        projr_profile_get(), normalizePath(getwd(), winslash = "/")
      )
      # test precedence works correctly
      projr_profile_create("test_profile_hidden")
      projr_profile_create()
      Sys.setenv("PROJR_PROFILE" = "test_profile")
      yml_projr <- .projr_yml_get_root_full()
      expect_true("directories-test_profile_hidden" %in% names(yml_projr))
      # should get the directory back
      expect_identical(
        projr_profile_get(), normalizePath(getwd(), winslash = "/")
      )
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
      expect_false(projr_profile_delete("default"))

      # deleting it anywhere
      Sys.setenv("PROJR_PROFILE" = "xyz")
      projr_profile_create()
      expect_true("directories-xyz" %in% names(.projr_yml_get_root_full()))
      invisible(file.create("_projr-xyz.yml"))
      projr_profile_delete(projr_profile_get())
      expect_false("directories-xyz" %in% names(.projr_yml_get_root_full()))
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
      projr_profile_create_local()
      expect_true(file.exists("_projr-local.yml"))
      expect_error(.projr_profile_create_local())
      yml_projr_local <- yaml::read_yaml("_projr-local.yml")
      expect_true(
        is.null(yml_projr_local[["directories"]][["data-raw"]][["path"]])
      )
      expect_true(all(names(yml_projr_local) == c("directories", "build")))
      rbuildignore <- readLines(
        file.path(rprojroot::is_r_package$find_file(), ".Rbuildignore")
      )
      expect_true("^_projr-local\\.yml$" %in% rbuildignore)
      gitignore <- readLines(
        file.path(rprojroot::is_r_package$find_file(), ".gitignore")
      )
      expect_true("_projr-local.yml" %in% gitignore)
    },
    quiet = TRUE,
    force = TRUE
  )
})
