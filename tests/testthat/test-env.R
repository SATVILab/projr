test_that("projr_env_set works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      Sys.unsetenv("TEST_VAR")
      writeLines(c("TEST_VAR=abc", ""), "_environment")
      env <- environment()
      projr_env_set("_environment")
      expect_identical(Sys.getenv("TEST_VAR"), "abc")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR"), "abc")
      Sys.unsetenv("TEST_VAR")
      .env_set("_environment", TRUE)
      expect_identical(Sys.getenv("TEST_VAR"), "abc")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR"), "")
      file.create("_environment.local") |> invisible()
      writeLines(c("TEST_VAR_LOCAL=abc", ""), "_environment.local")
      Sys.setenv("QUARTO_PROFILE" = "test")
      file.create("_environment-test") |> invisible()
      writeLines(c("TEST_VAR_QUARTO=abc", ""), "_environment-test")
      writeLines(c("TEST_VAR_DEFAULT=abc", ""), "_environment")
      Sys.unsetenv("TEST_VAR_LOCAL")
      Sys.unsetenv("TEST_VAR_QUARTO")
      Sys.unsetenv("TEST_VAR_DEFAULT")
      .env_set(unset = TRUE)
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "abc")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "")
      projr_env_set()
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "abc")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "abc")
      Sys.setenv("TEST_VAR_QUARTO" = "def")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "def")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "def")
      Sys.unsetenv("TEST_VAR_QUARTO")
    }
  )
})

test_that("projr_env_set handles invalid environment variable lines", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      Sys.unsetenv("TEST_VAR")
      Sys.unsetenv("VALID_VAR")

      writeLines(c(
        "TEST_VAR=abc",
        "INVALID_NO_VALUE=",
        "INVALID_FORMAT",
        "=INVALID_NO_NAME",
        "VALID_VAR=xyz"
      ), "_environment")

      projr_env_set("_environment")

      expect_identical(Sys.getenv("TEST_VAR"), "abc")
      expect_identical(Sys.getenv("VALID_VAR"), "xyz")
      expect_identical(Sys.getenv("INVALID_NO_VALUE"), "")
      expect_identical(Sys.getenv("INVALID_FORMAT"), "")
      expect_identical(Sys.getenv(""), "")

      Sys.unsetenv("TEST_VAR")
      Sys.unsetenv("VALID_VAR")
    }
  )
})

test_that("projr_env_set handles comments in environment files", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      Sys.unsetenv("TEST_VAR")
      Sys.unsetenv("TEST_VAR2")
      Sys.unsetenv("TEST_VAR3")

      writeLines(c(
        "# Full line comment",
        "TEST_VAR=abc",
        "TEST_VAR2=def # Inline comment",
        "   # Indented comment",
        "TEST_VAR3=ghi"
      ), "_environment")

      projr_env_set("_environment")
      browser()

      expect_identical(Sys.getenv("TEST_VAR"), "abc")
      expect_identical(Sys.getenv("TEST_VAR2"), "def")
      expect_identical(Sys.getenv("TEST_VAR3"), "ghi")

      Sys.unsetenv("TEST_VAR")
      Sys.unsetenv("TEST_VAR2")
      Sys.unsetenv("TEST_VAR3")
    }
  )
})

test_that("projr_env_set handles multiple profiles correctly", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      invisible(file.create("_environment-dev"))
      invisible(file.create("_environment-test"))
      invisible(file.create("_environment-prod"))

      writeLines("COMMON_VAR=base", "_environment")
      writeLines(c("PROFILE_VAR=dev", "COMMON_VAR=dev"), "_environment-dev")
      writeLines(c("PROFILE_VAR=test", "COMMON_VAR=test"), "_environment-test")
      writeLines(c("PROFILE_VAR=prod", "COMMON_VAR=prod"), "_environment-prod")

      Sys.unsetenv("COMMON_VAR")
      Sys.unsetenv("PROFILE_VAR")
      Sys.setenv("QUARTO_PROFILE" = "test,dev")
      projr_env_set()

      expect_identical(Sys.getenv("PROFILE_VAR"), "test")
      expect_identical(Sys.getenv("COMMON_VAR"), "test")

      Sys.unsetenv("COMMON_VAR")
      Sys.unsetenv("PROFILE_VAR")
      Sys.unsetenv("QUARTO_PROFILE")
      Sys.setenv("PROJR_PROFILE" = "prod;dev")
      projr_env_set()

      expect_identical(Sys.getenv("PROFILE_VAR"), "prod")
      expect_identical(Sys.getenv("COMMON_VAR"), "prod")

      Sys.unsetenv("COMMON_VAR")
      Sys.unsetenv("PROFILE_VAR")
      Sys.setenv("QUARTO_PROFILE" = "test")
      Sys.setenv("PROJR_PROFILE" = "prod")
      projr_env_set()

      expect_identical(Sys.getenv("PROFILE_VAR"), "test")
      expect_identical(Sys.getenv("COMMON_VAR"), "test")

      Sys.unsetenv("COMMON_VAR")
      Sys.unsetenv("PROFILE_VAR")
      Sys.unsetenv("QUARTO_PROFILE")
      Sys.unsetenv("PROJR_PROFILE")
    }
  )
})

test_that("projr_env_set respects local file precedence", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      invisible(file.create("_environment-test"))
      invisible(file.create("_environment.local"))

      writeLines("PRECEDENCE_VAR=default", "_environment")
      writeLines("PRECEDENCE_VAR=profile", "_environment-test")
      writeLines("PRECEDENCE_VAR=local", "_environment.local")

      Sys.unsetenv("PRECEDENCE_VAR")
      Sys.setenv("QUARTO_PROFILE" = "test")
      projr_env_set()

      expect_identical(Sys.getenv("PRECEDENCE_VAR"), "local")

      file.remove("_environment.local")
      Sys.unsetenv("PRECEDENCE_VAR")
      projr_env_set()

      expect_identical(Sys.getenv("PRECEDENCE_VAR"), "profile")

      Sys.unsetenv("PRECEDENCE_VAR")
      Sys.unsetenv("QUARTO_PROFILE")
    }
  )
})

test_that("projr_env_set only sets unspecified variables", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))

      Sys.setenv("PRESET_VAR" = "original")

      writeLines(c(
        "PRESET_VAR=new",
        "UNSET_VAR=value"
      ), "_environment")

      projr_env_set("_environment")

      expect_identical(Sys.getenv("PRESET_VAR"), "original")
      expect_identical(Sys.getenv("UNSET_VAR"), "value")

      Sys.unsetenv("PRESET_VAR")
      Sys.unsetenv("UNSET_VAR")
    }
  )
})
