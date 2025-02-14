test_that("projr_profile_get, _set and _create work", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  .dir_create(dir_test)
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
      yml.init <- .yml_get_default_raw()
      # test getting input and validating
      # --------------------------------
      expect_error(.profile_create(silent = "wingbats"))
      yml.init <- .yml_get_default_raw()
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical.profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")

      expect_identical.profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      # test "protected" profiles
      expect_error.profile_create("local"))
      expect_error.profile_create("default"))
      expect_error.profile_create())
      expect_error.profile_create(profile = "!"))

      .test_unset()
      suppressMessages(
        expect_message(
         .profile_create(
            profile = "test_a-bc", silent = FALSE
          ),
          "Added the following profile: test_a-bc"
        )
      )
      suppressMessages(
        expect_message(
         .profile_create(
            profile = "test_a-bc", silent = FALSE
          ),
          "projr profile test_a-bc already exists"
        )
      )
      expect_false(
        suppressMessages(
         .profile_create(profile = "test_a-bc")
        )
      )
      .test_set()

      # test creating profile
      # -----------------------

      # file method
     .profile_create(profile = "test-profile2")
      yml_projr <- .yml_get_default_raw()
      expect_true(file.exists("_projr-test-profile2.yml"))
      .yml_set(yml.init)

      # test getting a profile
      # ---------------------

      # default
     .profile <-.profile_get()
      expect_identical.profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical.profile_get(), "abc")
      #.PROFILE that exists
     .profile_create(profile = "abc")
      expect_identical.profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical.profile_get(), "default")
      .yml_set(yml.init)
      invisible(file.remove("_projr-abc.yml"))
      # working directory
      expect_identical(
       .profile_get(), "default"
      )
      # test precedence works correctly
     .profile_create("test_profile_hidden")
      # should get the projr profile back
      Sys.setenv("PROJR_PROFILE" = "test_profile_hidden")
      expect_identical.profile_get(), "test_profile_hidden")

      # delecting a a projr profile
      # ------------------------
      .yml_set(yml.init)

      # input validation
      expect_error.profile_delete(profile = 1))
      expect_error.profile_delete())
      # default profile does nothing
      expect_error.profile_delete("default"))

      # deleting it anywhere
      Sys.setenv("PROJR_PROFILE" = "xyz")
     .profile_create()
      expect_true(file.exists("_projr-xyz.yml"))
     .profile_delete.profile_get())
      expect_false(file.exists("_projr-xyz.yml"))
      Sys.unsetenv("PROJR_PROFILE")
      .test_unset()
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_create_local works", {
  skip_if(.is_test_select())
  dir_test <- file.path(tempdir(), paste0("test_projr"))
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
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
     .profile_create_local()
      expect_true(file.exists("_projr-local.yml"))
      expect_error(.profile_create_local())
      yml.local <- yaml::read_yaml("_projr-local.yml")
      expect_true(
        is.null(yml.local[["directories"]][["raw-data"]][["path"]])
      )
      expect_true(all(names(yml.local) == c("directories", "build")))
      rbuildignore <- readLines(.path_get(".Rbuildignore"))
      expect_true("^_projr-local\\.yml$" %in% rbuildignore)
      gitignore <- readLines(.path_get(".gitignore"))
      expect_true("_projr-local.yml" %in% gitignore)
    },
    quiet = TRUE,
    force = TRUE
  )
})
