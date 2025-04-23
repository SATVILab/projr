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
      # check that we can set and then unset, if requested
      # to unset
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
      # check that we don't automatically unset
      projr_env_set()
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "abc")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR_LOCAL"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "abc")
      expect_identical(Sys.getenv("TEST_VAR_DEFAULT"), "abc")
      # check that we stop unsetting variables of that
      # name after they've been unset:
      Sys.setenv("TEST_VAR_QUARTO" = "def")
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "def")
      .env_unset()
      expect_identical(Sys.getenv("TEST_VAR_QUARTO"), "def")
      Sys.unsetenv("TEST_VAR_QUARTO")
    }
  )
})
