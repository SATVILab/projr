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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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
  skip_if(.is_test_cran())
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

test_that("projr_env_set handles empty environment files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))

      # Empty file should not error
      expect_error(projr_env_set("_environment"), NA)

      # File with only whitespace
      writeLines(c("", "   ", "\t"), "_environment")
      expect_error(projr_env_set("_environment"), NA)
    }
  )
})

test_that("projr_env_set handles special characters in values", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))
      Sys.unsetenv("SPECIAL_VAR")

      # Test with special characters
      writeLines(c(
        "SPECIAL_VAR=value with spaces",
        "PATH_VAR=/path/to/file",
        "URL_VAR=https://example.com?param=value&other=123"
      ), "_environment")

      projr_env_set("_environment")

      expect_identical(Sys.getenv("SPECIAL_VAR"), "value with spaces")
      expect_identical(Sys.getenv("PATH_VAR"), "/path/to/file")
      expect_identical(Sys.getenv("URL_VAR"), "https://example.com?param=value&other=123")

      Sys.unsetenv("SPECIAL_VAR")
      Sys.unsetenv("PATH_VAR")
      Sys.unsetenv("URL_VAR")
    }
  )
})

test_that("projr_env_set handles missing files gracefully", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-existent file should not error
      expect_error(projr_env_set("_environment_nonexistent"), NA)

      # Multiple non-existent files
      expect_error(projr_env_set(c("_env1", "_env2", "_env3")), NA)
    }
  )
})

test_that("projr_env_set ignores _environment.local in git", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment.local"))
      writeLines("LOCAL_VAR=test", "_environment.local")

      Sys.unsetenv("LOCAL_VAR")
      projr_env_set("_environment.local")

      # Check that file is in .gitignore
      gitignore_content <- readLines(".gitignore", warn = FALSE)
      expect_true("_environment.local" %in% gitignore_content)

      Sys.unsetenv("LOCAL_VAR")
    }
  )
})

test_that("PROJR_PROFILE handles comma and semicolon separators", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Save original
  old_val <- Sys.getenv("PROJR_PROFILE", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_PROFILE = old_val) else Sys.unsetenv("PROJR_PROFILE"))

  # Test comma separator
  Sys.setenv(PROJR_PROFILE = "test,dev,prod")
  profiles <- projr_profile_get()
  expect_identical(profiles, c("test", "dev", "prod"))

  # Test semicolon separator
  Sys.setenv(PROJR_PROFILE = "test;dev;prod")
  profiles <- projr_profile_get()
  expect_identical(profiles, c("test", "dev", "prod"))

  # Test mixed separators
  Sys.setenv(PROJR_PROFILE = "test,dev;prod")
  profiles <- projr_profile_get()
  expect_true(all(c("test", "dev", "prod") %in% profiles))
})

test_that("PROJR_PROFILE filters out default and local", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_PROFILE", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_PROFILE = old_val) else Sys.unsetenv("PROJR_PROFILE"))

  # Test that default and local are filtered
  Sys.setenv(PROJR_PROFILE = "default,test,local,dev")
  profiles <- projr_profile_get()
  expect_false("default" %in% profiles)
  expect_false("local" %in% profiles)
  expect_true("test" %in% profiles)
  expect_true("dev" %in% profiles)
})

test_that("PROJR_PROFILE handles whitespace correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_PROFILE", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_PROFILE = old_val) else Sys.unsetenv("PROJR_PROFILE"))

  # Test with whitespace around values
  Sys.setenv(PROJR_PROFILE = " test , dev , prod ")
  profiles <- projr_profile_get()
  expect_identical(profiles, c("test", "dev", "prod"))

  # Test with empty values
  Sys.setenv(PROJR_PROFILE = "test,,dev")
  profiles <- projr_profile_get()
  expect_false("" %in% profiles)
  expect_true("test" %in% profiles)
  expect_true("dev" %in% profiles)
})

test_that("QUARTO_PROFILE takes precedence over PROJR_PROFILE", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment-quarto"))
      invisible(file.create("_environment-projr"))

      writeLines("VAR=quarto", "_environment-quarto")
      writeLines("VAR=projr", "_environment-projr")

      Sys.unsetenv("VAR")
      Sys.setenv(QUARTO_PROFILE = "quarto")
      Sys.setenv(PROJR_PROFILE = "projr")

      projr_env_set()

      # QUARTO_PROFILE should take precedence
      expect_identical(Sys.getenv("VAR"), "quarto")

      Sys.unsetenv("VAR")
      Sys.unsetenv("QUARTO_PROFILE")
      Sys.unsetenv("PROJR_PROFILE")
    }
  )
})

test_that("Environment variable name parsing handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      invisible(file.create("_environment"))

      # Test various edge cases
      writeLines(c(
        "UNDERSCORE_VAR=value1",
        "NUMBER123=value2",
        "MixedCase=value3",
        "DOTS.NOT.VALID=should_not_set", # Invalid name
        "DASH-NOT-VALID=should_not_set" # Invalid name
      ), "_environment")

      Sys.unsetenv("UNDERSCORE_VAR")
      Sys.unsetenv("NUMBER123")
      Sys.unsetenv("MixedCase")

      projr_env_set("_environment")

      expect_identical(Sys.getenv("UNDERSCORE_VAR"), "value1")
      expect_identical(Sys.getenv("NUMBER123"), "value2")
      expect_identical(Sys.getenv("MixedCase"), "value3")

      Sys.unsetenv("UNDERSCORE_VAR")
      Sys.unsetenv("NUMBER123")
      Sys.unsetenv("MixedCase")
    }
  )
})

test_that(".build_env_file_required_check validates required variables", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with no _environment.required file
      result <- .build_env_file_required_check()
      expect_false(result)

      # Create _environment.required file
      invisible(file.create("_environment.required"))
      writeLines(c(
        "REQUIRED_VAR1",
        "REQUIRED_VAR2"
      ), "_environment.required")

      # Set one variable but not the other
      Sys.setenv("REQUIRED_VAR1" = "set")
      Sys.unsetenv("REQUIRED_VAR2")

      # Should warn about missing variable
      expect_warning(
        .build_env_file_required_check(),
        "REQUIRED_VAR2.*unset.*_environment.required"
      )

      # Set both variables - no warning
      Sys.setenv("REQUIRED_VAR2" = "also_set")
      expect_warning(.build_env_file_required_check(), NA)

      Sys.unsetenv("REQUIRED_VAR1")
      Sys.unsetenv("REQUIRED_VAR2")
    }
  )
})

test_that(".build_env_var_required_check validates individual variable", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with unset variable
  Sys.unsetenv("TEST_REQUIRED_VAR")
  expect_warning(
    .build_env_var_required_check("TEST_REQUIRED_VAR"),
    "TEST_REQUIRED_VAR.*unset.*_environment.required"
  )

  # Test with set variable - no warning
  Sys.setenv("TEST_REQUIRED_VAR" = "value")
  expect_warning(.build_env_var_required_check("TEST_REQUIRED_VAR"), NA)

  # Test with empty line - still warns because env_var_nm is ""
  # and Sys.getenv("") returns ""
  expect_warning(.build_env_var_required_check(""), "``.*unset")

  Sys.unsetenv("TEST_REQUIRED_VAR")
})

test_that(".build_env_check validates build environment", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with output_run = FALSE
      result <- .build_env_check(FALSE)
      expect_false(result)

      # Test with output_run = TRUE and no remotes
      result <- .build_env_check(TRUE)
      expect_true(result)

      # Test with _environment.required
      invisible(file.create("_environment.required"))
      writeLines("REQUIRED_BUILD_VAR", "_environment.required")
      Sys.unsetenv("REQUIRED_BUILD_VAR")

      expect_warning(
        .build_env_check(TRUE),
        "REQUIRED_BUILD_VAR"
      )

      Sys.unsetenv("REQUIRED_BUILD_VAR")
    }
  )
})

test_that(".env_var_add_to_unset tracks variables to unset", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean up any existing list
  list_path <- .env_file_get_path_list()
  if (file.exists(list_path)) {
    unlink(list_path)
  }
  if (file.exists(dirname(list_path))) {
    unlink(dirname(list_path), recursive = TRUE)
  }

  # Test with unset = FALSE (should not track)
  result <- .env_var_add_to_unset("TEST_VAR1", FALSE)
  expect_false(result)
  expect_false(file.exists(list_path))

  # Test with unset = TRUE (should track)
  result <- .env_var_add_to_unset("TEST_VAR1", TRUE)
  expect_true(result)
  expect_true(file.exists(list_path))

  # Check content
  vars <- readLines(list_path, warn = FALSE)
  expect_true("TEST_VAR1" %in% vars)

  # Add another variable
  result <- .env_var_add_to_unset("TEST_VAR2", TRUE)
  expect_true(result)
  vars <- readLines(list_path, warn = FALSE)
  expect_true("TEST_VAR2" %in% vars)

  # Try to add duplicate - should return FALSE
  result <- .env_var_add_to_unset("TEST_VAR1", TRUE)
  expect_false(result)

  # Clean up
  unlink(dirname(list_path), recursive = TRUE)
})

test_that(".env_file_get_path_list returns consistent path", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  path1 <- .env_file_get_path_list()
  path2 <- .env_file_get_path_list()

  # Should always return same path
  expect_identical(path1, path2)

  # Should be in tempdir
  expect_true(grepl(tempdir(), path1, fixed = TRUE))

  # Should contain expected components
  expect_true(grepl("projr-env_file", path1, fixed = TRUE))
  expect_true(grepl("env.list", path1, fixed = TRUE))
})

test_that(".env_file_activate_ind_ignore handles gitignore correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = TRUE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with _environment.local (should add to gitignore)
      invisible(file.create("_environment.local"))
      result <- .env_file_activate_ind_ignore("_environment.local")
      expect_true(result)

      gitignore_content <- readLines(".gitignore", warn = FALSE)
      expect_true("_environment.local" %in% gitignore_content)

      # Test with other environment file (should not add to gitignore)
      # First, get gitignore size before
      gitignore_before <- readLines(".gitignore", warn = FALSE)
      result <- .env_file_activate_ind_ignore("_environment")
      expect_false(result)

      # Gitignore should not have changed
      gitignore_after <- readLines(".gitignore", warn = FALSE)
      expect_identical(length(gitignore_before), length(gitignore_after))

      # Test with file outside project (should return FALSE)
      temp_file <- tempfile()
      result <- .env_file_activate_ind_ignore(temp_file)
      expect_false(result)
    }
  )
})

test_that(".env_profile_get combines profiles correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Save original values
  old_quarto <- Sys.getenv("QUARTO_PROFILE", unset = "")
  old_projr <- Sys.getenv("PROJR_PROFILE", unset = "")
  on.exit({
    if (nzchar(old_quarto)) Sys.setenv(QUARTO_PROFILE = old_quarto) else Sys.unsetenv("QUARTO_PROFILE")
    if (nzchar(old_projr)) Sys.setenv(PROJR_PROFILE = old_projr) else Sys.unsetenv("PROJR_PROFILE")
  })

  # Test with no profiles set
  Sys.unsetenv("QUARTO_PROFILE")
  Sys.unsetenv("PROJR_PROFILE")
  profiles <- .env_profile_get()
  expect_identical(profiles, character())

  # Test with only QUARTO_PROFILE
  Sys.setenv(QUARTO_PROFILE = "test,dev")
  Sys.unsetenv("PROJR_PROFILE")
  profiles <- .env_profile_get()
  expect_identical(profiles, c("test", "dev"))

  # Test with only PROJR_PROFILE
  Sys.unsetenv("QUARTO_PROFILE")
  Sys.setenv(PROJR_PROFILE = "prod,staging")
  profiles <- .env_profile_get()
  expect_identical(profiles, c("prod", "staging"))

  # Test with both (QUARTO should come first)
  Sys.setenv(QUARTO_PROFILE = "test")
  Sys.setenv(PROJR_PROFILE = "prod")
  profiles <- .env_profile_get()
  expect_identical(profiles, c("test", "prod"))

  # Test filtering of "required"
  Sys.setenv(QUARTO_PROFILE = "required,test")
  Sys.setenv(PROJR_PROFILE = "prod,required")
  profiles <- .env_profile_get()
  expect_false("required" %in% profiles)
  expect_true("test" %in% profiles)
  expect_true("prod" %in% profiles)

  # Test uniqueness
  Sys.setenv(QUARTO_PROFILE = "test,dev")
  Sys.setenv(PROJR_PROFILE = "test,prod")
  profiles <- .env_profile_get()
  expect_identical(sum(profiles == "test"), 1L)
})

test_that(".env_profile_get_quarto parses QUARTO_PROFILE correctly", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("QUARTO_PROFILE", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(QUARTO_PROFILE = old_val) else Sys.unsetenv("QUARTO_PROFILE"))

  # Test with empty/unset
  Sys.unsetenv("QUARTO_PROFILE")
  profiles <- .env_profile_get_quarto()
  expect_identical(profiles, character())

  Sys.setenv(QUARTO_PROFILE = "")
  profiles <- .env_profile_get_quarto()
  expect_identical(profiles, character())

  # Test with single profile
  Sys.setenv(QUARTO_PROFILE = "test")
  profiles <- .env_profile_get_quarto()
  expect_identical(profiles, "test")

  # Test with multiple profiles (comma-separated)
  Sys.setenv(QUARTO_PROFILE = "test,dev,prod")
  profiles <- .env_profile_get_quarto()
  expect_identical(profiles, c("test", "dev", "prod"))

  # Test with whitespace
  Sys.setenv(QUARTO_PROFILE = " test , dev , prod ")
  profiles <- .env_profile_get_quarto()
  expect_identical(profiles, c("test", "dev", "prod"))

  # Test with empty values
  Sys.setenv(QUARTO_PROFILE = "test,,dev")
  profiles <- .env_profile_get_quarto()
  expect_true("" %in% profiles) # Empty strings are preserved
})

test_that(".env_profile_get_projr returns PROJR profiles", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  old_val <- Sys.getenv("PROJR_PROFILE", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(PROJR_PROFILE = old_val) else Sys.unsetenv("PROJR_PROFILE"))

  # Test with empty/unset
  Sys.unsetenv("PROJR_PROFILE")
  profiles <- .env_profile_get_projr()
  expect_identical(profiles, character())

  # Test with profiles
  Sys.setenv(PROJR_PROFILE = "test,dev")
  profiles <- .env_profile_get_projr()
  expect_identical(profiles, c("test", "dev"))

  # Test that it uses .profile_get_var (which filters default and local)
  Sys.setenv(PROJR_PROFILE = "default,test,local,dev")
  profiles <- .env_profile_get_projr()
  expect_false("default" %in% profiles)
  expect_false("local" %in% profiles)
  expect_true("test" %in% profiles)
  expect_true("dev" %in% profiles)
})

test_that(".env_unset cleans up tracked variables", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set up some vars to unset
  Sys.setenv("UNSET_TEST_VAR1" = "value1")
  Sys.setenv("UNSET_TEST_VAR2" = "value2")

  # Track them
  .env_var_add_to_unset("UNSET_TEST_VAR1", TRUE)
  .env_var_add_to_unset("UNSET_TEST_VAR2", TRUE)

  # Verify they're set
  expect_identical(Sys.getenv("UNSET_TEST_VAR1"), "value1")
  expect_identical(Sys.getenv("UNSET_TEST_VAR2"), "value2")

  # Call .env_unset
  .env_unset()

  # Verify they're unset
  expect_identical(Sys.getenv("UNSET_TEST_VAR1"), "")
  expect_identical(Sys.getenv("UNSET_TEST_VAR2"), "")

  # Verify list file is cleaned up
  list_path <- .env_file_get_path_list()
  expect_false(file.exists(list_path))
  expect_false(file.exists(dirname(list_path)))

  # Test when list file doesn't exist
  result <- .env_unset()
  expect_false(result)
})

test_that(".env_file_activate_ind handles non-existent files", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(
    git = FALSE, github = FALSE, set_env_var = TRUE
  )
  usethis::with_project(
    path = dir_test,
    code = {
      # Non-existent file should return FALSE without error
      result <- .env_file_activate_ind("_nonexistent_env", FALSE)
      expect_false(result)
    }
  )
})

test_that(".env_var_set_line handles edge cases", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Test with already set variable (should not override)
  Sys.setenv("ALREADY_SET" = "original")
  result <- .env_var_set_line("ALREADY_SET=new_value", FALSE)
  expect_false(result)
  expect_identical(Sys.getenv("ALREADY_SET"), "original")
  Sys.unsetenv("ALREADY_SET")

  # Test with valid line and unset variable
  Sys.unsetenv("NEW_VAR")
  result <- .env_var_set_line("NEW_VAR=test_value", FALSE)
  expect_true(result)
  expect_identical(Sys.getenv("NEW_VAR"), "test_value")
  Sys.unsetenv("NEW_VAR")

  # Test with empty value
  result <- .env_var_set_line("VAR_NAME=", FALSE)
  expect_false(result) # Empty value should not set

  # Test with invalid format (no equals)
  Sys.unsetenv("INVALID")
  result <- .env_var_set_line("INVALID", FALSE)
  expect_false(result)

  # Test with comment
  result <- .env_var_set_line("# COMMENT=value", FALSE)
  expect_false(result)

  # Test with inline comment
  Sys.unsetenv("INLINE_VAR")
  result <- .env_var_set_line("INLINE_VAR=value # comment", FALSE)
  expect_true(result)
  expect_identical(Sys.getenv("INLINE_VAR"), "value")
  Sys.unsetenv("INLINE_VAR")

  # Test with unset = TRUE
  Sys.unsetenv("TRACK_VAR")
  result <- .env_var_set_line("TRACK_VAR=value", TRUE)
  expect_true(result)
  expect_identical(Sys.getenv("TRACK_VAR"), "value")

  # Check it was added to unset list
  list_path <- .env_file_get_path_list()
  if (file.exists(list_path)) {
    vars <- readLines(list_path, warn = FALSE)
    expect_true("TRACK_VAR" %in% vars)
    unlink(dirname(list_path), recursive = TRUE)
  }
  Sys.unsetenv("TRACK_VAR")
})
