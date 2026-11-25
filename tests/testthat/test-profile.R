test_that("projr_profile_get, _set and _create work", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(.test_unset())
  withr::defer(unlink(dir_test, recursive = TRUE))
  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      yml_projr_init <- .yml_get_default_raw()
      # test getting input and validating
      # --------------------------------
      expect_error(.profile_create(silent = "wingbats"))
      yml_projr_init <- .yml_get_default_raw()
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

      .test_unset()
      suppressMessages(
        expect_message(
          .profile_create(
            profile = "test_a-bc",
            silent = FALSE
          ),
          "Added the following profile: test_a-bc"
        )
      )
      suppressMessages(
        expect_message(
          .profile_create(
            profile = "test_a-bc",
            silent = FALSE
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
      .yml_set(yml_projr_init)

      # test getting a profile
      # ---------------------

      # default
      .profile <- projr_profile_get()
      expect_identical(projr_profile_get(), "default")
      Sys.setenv("PROJR_PROFILE" = "abc")
      expect_identical(projr_profile_get(), "abc")
      # .PROFILE that exists
      .profile_create(profile = "abc")
      expect_identical(projr_profile_get(), "abc")
      Sys.unsetenv("PROJR_PROFILE")
      expect_identical(projr_profile_get(), "default")
      .yml_set(yml_projr_init)
      invisible(file.remove("_projr-abc.yml"))
      # working directory
      expect_identical(
        .profile_get(),
        "default"
      )
      # test precedence works correctly
      .profile_create("test_profile_hidden")
      # should get the projr profile back
      Sys.setenv("PROJR_PROFILE" = "test_profile_hidden")
      expect_identical(projr_profile_get(), "test_profile_hidden")

      # delecting a a projr profile
      # ------------------------
      .yml_set(yml_projr_init)

      # input validation
      expect_error(projr_profile_delete(profile = 1))
      expect_error(projr_profile_delete())
      # default profile does nothing
      expect_error(projr_profile_delete("default"))

      # deleting it anywhere
      Sys.setenv("PROJR_PROFILE" = "xyz")
      .profile_create()
      expect_true(file.exists("_projr-xyz.yml"))
      .profile_delete(projr_profile_get())
      expect_false(file.exists("_projr-xyz.yml"))
      Sys.unsetenv("PROJR_PROFILE")
      .test_unset()
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_create_local works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))
  usethis::with_project(
    path = dir_test,
    code = {
      result <- .profile_create_local()
      expect_true(file.exists("_projr-local.yml"))
      expect_true(result)
      expect_error(.profile_create_local())
      yml.local <- yaml::read_yaml("_projr-local.yml")
      expect_true(
        is.null(yml.local[["directories"]][["raw-data"]][["path"]])
      )
      expect_identical(names(yml.local), names(.yml_get_default_raw()))
      rbuildignore <- readLines(.path_get(".Rbuildignore"))
      expect_true("^_projr-local\\.yml$" %in% rbuildignore)
      gitignore <- readLines(.path_get(".gitignore"))
      expect_true("_projr-local.yml" %in% gitignore)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_create returns correct value", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      # Test that projr_profile_create returns TRUE when creating new profile
      result <- projr_profile_create("test_new_profile")
      expect_true(result)
      expect_true(file.exists("_projr-test_new_profile.yml"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_create_local returns correct value", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      # Test that projr_profile_create_local returns TRUE when creating
      result <- projr_profile_create_local()
      expect_true(result)
      expect_true(file.exists("_projr-local.yml"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".profile_get returns profile when set", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  withr::defer(Sys.unsetenv("PROJR_PROFILE"))

  # Test that .profile_get returns the profile when it's set
  Sys.setenv("PROJR_PROFILE" = "test_profile")
  result <- .profile_get()
  expect_identical(result, "test_profile")

  Sys.unsetenv("PROJR_PROFILE")
})

test_that("projr_profile_delete returns correct value", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a profile to delete
      .profile_create("test_delete_profile")
      expect_true(file.exists("_projr-test_delete_profile.yml"))

      # Test that projr_profile_delete returns TRUE when deleting
      result <- projr_profile_delete("test_delete_profile")
      expect_true(result)
      expect_false(file.exists("_projr-test_delete_profile.yml"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that(".profile_delete_impl handles non-existent profile", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      # Test that .profile_delete_impl returns FALSE for non-existent profile
      suppressMessages({
        result <- .profile_delete_impl("nonexistent_profile")
      })
      expect_false(result)
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("projr_profile_delete_local works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .dir_get_tmp_random_path()
  withr::defer(unlink(dir_test, recursive = TRUE))

  .dir_create(dir_test)
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    fs::file_copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }
  gitignore <- c(
    "# R",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# docs",
    "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  usethis::with_project(
    path = dir_test,
    code = {
      # Create a local profile
      .profile_create_local()
      expect_true(file.exists("_projr-local.yml"))

      # Test that projr_profile_delete_local removes the file
      result <- projr_profile_delete_local()
      expect_true(result)
      expect_false(file.exists("_projr-local.yml"))
    },
    quiet = TRUE,
    force = TRUE
  )
})

test_that("config directories accept <profile>.yml file names", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      dir.create(".projr/config", recursive = TRUE, showWarnings = FALSE)
      writeLines(
        "directories: {output: {path: _out_analysis}}",
        ".projr/config/analysis.yml"
      )
      writeLines("build: {git: false}", ".projr/config/local.yml")
      expect_match(.yml_get_path("analysis"), ".projr/config/analysis.yml$")
      expect_match(.yml_get_path("local"), ".projr/config/local.yml$")
      withr::with_envvar(c(PROJR_PROFILE = "analysis"), {
        expect_identical(
          .yml_get(NULL)$directories$output$path,
          "_out_analysis"
        )
      })
      expect_false(.yml_get(NULL)$build$git)
      expect_setequal(
        .yml_config_file_profile(.yml_config_files()),
        c("default", "analysis", "local")
      )
      .ignore_auto_yml()
      expect_true(
        any(grepl("projr/config/local.yml", readLines(".gitignore")))
      )

      file.copy(
        ".projr/config/analysis.yml",
        ".projr/config/_projr-analysis.yml"
      )
      expect_error(.yml_get_path("analysis"), "Keep one")
      unlink(".projr/config/_projr-analysis.yml")

      # The base file can live in the config directory as default.yml.
      file.rename("_projr.yml", ".projr/config/default.yml")
      expect_match(.yml_get_path("default"), ".projr/config/default.yml$")
      expect_identical(
        normalizePath(.path_project_root()),
        normalizePath(dir_test)
      )
    }
  )
})

test_that("the profile key selects projr profiles with env precedence", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      withr::local_envvar(c(PROJR_PROFILE = NA))
      yml <- .yml_get_default_raw()
      writeLines("directories: {output: {path: _out_a}}", "_projr-a.yml")
      writeLines("directories: {output: {path: _out_b}}", "_projr-b.yml")
      expect_identical(projr_profile_get(), "default")

      # A sequence in the default file.
      yaml::write_yaml(c(yml, list(profile = c("a", "b"))), "_projr.yml")
      expect_identical(projr_profile_get(), c("a", "b"))
      expect_identical(.yml_get(NULL)$directories$output$path, "_out_a")
      expect_true(projr_yml_check())

      # The mapping form in the local file overrides the default file.
      writeLines("profile: {projr: b, quarto: [q1, q2]}", "_projr-local.yml")
      expect_identical(projr_profile_get(), "b")
      expect_identical(.profile_config("quarto")$profile, c("q1", "q2"))
      expect_match(.profile_get_source(), "_projr-local.yml")
      expect_identical(.yml_get(NULL)$directories$output$path, "_out_b")

      # The environment variable overrides both files.
      withr::with_envvar(c(PROJR_PROFILE = "default"), {
        expect_identical(projr_profile_get(), "default")
        expect_identical(.profile_get_source(), "PROJR_PROFILE")
      })

      # The build argument overrides the environment variable.
      withr::with_envvar(c(PROJR_PROFILE = "b"), {
        local_mocked_bindings(.env_set = function(...) {
          stop("profiles: ", paste(.profile_get_var(), collapse = ","))
        })
        expect_error(
          .build(bump_component = "dev", args_engine = list(), profile = "a"),
          "profiles: a"
        )
        expect_identical(projr_profile_get(), "b")
      })
    }
  )
})

test_that("invalid profile keys are rejected", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = FALSE)
  usethis::with_project(
    path = dir_test,
    code = {
      withr::local_envvar(c(PROJR_PROFILE = NA))
      writeLines("profile: a", "_projr-b.yml")
      expect_error(.yml_get("b"), "default and local files")
      withr::with_envvar(c(PROJR_PROFILE = "b"), {
        expect_error(projr_yml_check(), "default and local files")
      })
      unlink("_projr-b.yml")

      writeLines("profile: 1", "_projr-local.yml")
      expect_error(projr_profile_get(), "profile.projr")
      expect_error(projr_yml_check(), "profile.projr")
      writeLines("profile: {other: a}", "_projr-local.yml")
      expect_error(projr_yml_check(), "profile")
      writeLines("profile: {renv: a}", "_projr-local.yml")
      expect_error(projr_yml_check(), "not supported")
    }
  )
})
