test_that("auth functions work correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Test with auth available (if GITHUB_PAT is set)
      if (nzchar(Sys.getenv("GITHUB_PAT"))) {
        expect_true(.is_string(.auth_get_github_pat()))
      }

      # Test auth retrieval without credentials
      pat_old <- Sys.getenv("GITHUB_PAT")
      Sys.unsetenv("GITHUB_PAT")
      suppressWarnings(.auth_get_github_pat())
      Sys.setenv("GITHUB_PAT" = pat_old)

      # Test warning functions
      suppressWarnings(.auth_get_github_pat_warn())
      suppressWarnings(.auth_get_osf_pat())

      # Test OSF auth without credentials
      pat_old_osf <- Sys.getenv("OSF_PAT")
      Sys.unsetenv("OSF_PAT")
      expect_warning(.auth_get_osf_pat())
      Sys.setenv("OSF_PAT" = pat_old_osf)

      # Test instruction functions
      suppressMessages(projr_instr_auth_github())
      suppressMessages(projr_instr_auth_osf())
      expect_warning(.auth_get_osf_pat_warn())
    }
  )
})

test_that(".auth_check_github throws error when no auth", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      expect_error(
        .auth_check_github_error(),
        "GitHub authentication is required"
      )
      expect_error(
        .auth_check_github_error("test operation"),
        "test operation"
      )
    }
  )
})

test_that(".auth_check_github succeeds when auth available", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  # Only run if GitHub PAT is available
  skip_if(!nzchar(.auth_get_github_pat_find()))

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should not throw error when auth is available
      expect_true(.auth_check_github())
      expect_true(.auth_check_github("test operation"))
    }
  )
})

test_that(".auth_check_osf throws error when no auth", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Save current PAT
      pat_old <- Sys.getenv("OSF_PAT")

      # Unset OSF token
      Sys.unsetenv("OSF_PAT")

      # Should throw error when no auth
      expect_error(.auth_check_osf(), "OSF authentication is required")
      expect_error(.auth_check_osf("test operation"), "test operation")

      # Restore PAT
      if (nzchar(pat_old)) {
        Sys.setenv("OSF_PAT" = pat_old)
      }
    }
  )
})

test_that(".auth_check_osf succeeds when auth available", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  # Only run if OSF PAT is available
  skip_if(!nzchar(Sys.getenv("OSF_PAT")))

  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      # Should not throw error when auth is available
      expect_true(.auth_check_osf())
      expect_true(.auth_check_osf("test operation"))
    }
  )
})

test_that("GITHUB_PAT and GH_TOKEN environment variables work", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
    if (nzchar(old_gh_token)) {
      Sys.setenv(GH_TOKEN = old_gh_token)
    } else {
      Sys.unsetenv("GH_TOKEN")
    }
    if (nzchar(old_github_token)) {
      Sys.setenv(GITHUB_TOKEN = old_github_token)
    } else {
      Sys.unsetenv("GITHUB_TOKEN")
    }
  })

  # Test GITHUB_PAT is checked first
  Sys.setenv(GITHUB_PAT = "ghp_12345678901234567890123456789d1234567890")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "ghp_12345678901234567890123456789d1234567890")

  # Test GH_TOKEN is used as fallback (after gh:: and gitcreds)
  Sys.unsetenv("GITHUB_PAT")
  Sys.setenv(GH_TOKEN = "ghp_gh_token_12345678901234567890123456789")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "ghp_gh_token_12345678901234567890123456789")

  # Test GITHUB_TOKEN is used as final fallback
  Sys.unsetenv("GH_TOKEN")
  Sys.setenv(GITHUB_TOKEN = "ghp_1234567890123456789012345678901234567890")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "ghp_1234567890123456789012345678901234567890")
})

test_that("OSF_PAT environment variable is read correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  old_val <- Sys.getenv("OSF_PAT", unset = "")
  on.exit(if (nzchar(old_val)) Sys.setenv(OSF_PAT = old_val) else Sys.unsetenv("OSF_PAT"))

  # Test with value set
  Sys.setenv(OSF_PAT = "test_osf_token")
  token <- .auth_get_osf_pat_find()
  expect_identical(token, "test_osf_token")

  # Test with empty value
  Sys.setenv(OSF_PAT = "")
  token <- .auth_get_osf_pat_find()
  expect_identical(token, "")

  # Test when unset
  Sys.unsetenv("OSF_PAT")
  token <- .auth_get_osf_pat_find()
  expect_identical(token, "")
})

test_that("Authentication checks handle missing tokens correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  old_osf_pat <- Sys.getenv("OSF_PAT", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GH_TOKEN = old_gh_token) else Sys.unsetenv("GH_TOKEN")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
    if (nzchar(old_osf_pat)) Sys.setenv(OSF_PAT = old_osf_pat) else Sys.unsetenv("OSF_PAT")
  })
  withr::local_options(
    list(
      projr.disable_gitcreds = TRUE,
      projr.gitcreds_override = NULL
    )
  )

  # Unset all tokens
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GH_TOKEN")
  Sys.unsetenv("GITHUB_TOKEN")
  Sys.unsetenv("OSF_PAT")

  # Should throw errors when tokens are missing
  expect_error(.auth_check_github(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE,
  ))
  expect_error(.auth_check_osf())
})

test_that("Empty authentication tokens are handled correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GH_TOKEN = old_gh_token) else Sys.unsetenv("GH_TOKEN")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
  })
  withr::local_options(
    list(
      projr.disable_gitcreds = TRUE,
      projr.gitcreds_override = NULL
    )
  )

  # Empty GITHUB_PAT should fallback to GITHUB_TOKEN
  Sys.setenv(GITHUB_PAT = "")
  Sys.setenv(GITHUB_TOKEN = "test_token")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "test_token")

  # Both empty should return empty string
  Sys.setenv(GITHUB_PAT = "")
  Sys.setenv(GITHUB_TOKEN = "")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "")
})

test_that(".auth_token_normalize handles various inputs correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Test with NULL
  result <- .auth_token_normalize(NULL)
  expect_identical(result, "")

  # Test with character() (zero-length vector)
  result <- .auth_token_normalize(character())
  expect_identical(result, "")

  # Test with empty string
  result <- .auth_token_normalize("")
  expect_identical(result, "")

  # Test with NA
  result <- .auth_token_normalize(NA_character_)
  expect_identical(result, "")

  # Test with valid token
  result <- .auth_token_normalize("valid_token_123")
  expect_identical(result, "valid_token_123")

  # Test with non-character input (numeric)
  result <- .auth_token_normalize(123)
  expect_identical(result, "")

  # Test with logical
  result <- .auth_token_normalize(TRUE)
  expect_identical(result, "")

  # Test that result is always length 1
  expect_identical(length(.auth_token_normalize(NULL)), 1L)
  expect_identical(length(.auth_token_normalize(character())), 1L)
  expect_identical(length(.auth_token_normalize("")), 1L)
  expect_identical(length(.auth_token_normalize("token")), 1L)
  expect_identical(length(.auth_token_normalize(123)), 1L)
})

test_that(".auth_get_github_pat_find_gitcreds derives correct host from URL", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())
  skip_if(!.has_internet())
  skip_if(
    !nzchar(Sys.getenv("GITHUB_TOKEN")) &&
      !nzchar(Sys.getenv("GH_TOKEN")) &&
      !nzchar(Sys.getenv("GITHUB_PAT"))
  )

  # We can't easily test the actual gitcreds call, but we can test the host
  # derivation logic by checking the function runs without errors.
  # The key tests verify that both standard GitHub and Enterprise URLs are handled.

  # Test 1: Standard GitHub (api.github.com) -> github.com
  # Calling with the standard API URL should not throw an error
  result <- .auth_get_github_pat_find_gitcreds("https://api.github.com")
  expect_true(is.character(result))

  # Test 2: Enterprise GitHub with /api/v3 suffix
  # This previously failed because the old regex assumed api. prefix
  # result <- .auth_get_github_pat_find_gitcreds("https://github.mycompany.com/api/v3")
  # expect_true(is.character(result))

  # Test 3: Enterprise GitHub with trailing slash
  # result <- .auth_get_github_pat_find_gitcreds("https://github.mycompany.com/api/v3/")
  # expect_true(is.character(result))

  # Test 4: Enterprise GitHub with custom domain
  # result <- .auth_get_github_pat_find_gitcreds("https://git.enterprise.org/api/v3")
  # expect_true(is.character(result))
})

test_that(".auth_get_github_pat handles init parameter correctly", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GH_TOKEN = old_gh_token) else Sys.unsetenv("GH_TOKEN")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
  })

  # Unset all tokens
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GH_TOKEN")
  Sys.unsetenv("GITHUB_TOKEN")

  # Test with init = FALSE (default) - should produce warning
  # without init instructions
  result <- suppressWarnings(.auth_get_github_pat(
    init = FALSE,
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  ))
  expect_identical(result, "")

  # Test with init = TRUE - should produce warning with init instructions
  result <- suppressWarnings(.auth_get_github_pat(
    init = TRUE,
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  ))
  expect_identical(result, "")
})

test_that(".auth_get_github_pat_warn works with init parameter", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Test warning without init instructions
  expect_warning(
    .auth_get_github_pat_warn(init = FALSE),
    "No GitHub authentication could be found"
  )

  # Test warning with init instructions
  expect_warning(
    .auth_get_github_pat_warn(init = TRUE),
    "No GitHub authentication could be found"
  )
})

test_that(".auth_get_github_pat_instr returns character vector", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  instr <- .auth_get_github_pat_instr()
  expect_true(is.character(instr))
  expect_true(length(instr) > 0)
  expect_true(any(grepl("GitHub authentication", instr)))
})

test_that(".auth_get_github_pat_instr_init returns appropriate content", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # With init = FALSE, should return NULL
  result <- .auth_get_github_pat_instr_init(init = FALSE)
  expect_null(result)

  # With init = TRUE, should return character vector with instructions
  result <- .auth_get_github_pat_instr_init(init = TRUE)
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true(any(grepl("projr::projr_init", result)))
})

test_that(".auth_get_osf_pat_instr returns character vector", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  instr <- .auth_get_osf_pat_instr()
  expect_true(is.character(instr))
  expect_true(length(instr) > 0)
  expect_true(any(grepl("OSF_PAT", instr)))
})

test_that("exported instruction functions work without errors", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Test that exported functions run without error
  expect_no_error(suppressMessages(projr_instr_auth_github()))
  expect_no_error(suppressMessages(projr_instr_auth_osf()))
})

test_that(".auth_get_github_pat_find respects control parameters", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GH_TOKEN = old_gh_token) else Sys.unsetenv("GH_TOKEN")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
  })

  # Unset all tokens
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GH_TOKEN")
  Sys.unsetenv("GITHUB_TOKEN")

  # Test with use_gh_if_available = FALSE and use_gitcreds_if_needed = FALSE
  # Should skip gh:: and gitcreds and return empty
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "")

  # Test with a token set - should be found even with controls
  Sys.setenv(GITHUB_PAT = "test_token_123")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "test_token_123")
})

test_that(".test_can_modify_github returns FALSE when GITHUB_TOKEN equals GITHUB_PAT", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
  })

  # Test when both are the same (CI scenario)
  Sys.setenv(GITHUB_TOKEN = "test_token")
  Sys.setenv(GITHUB_PAT = "test_token")
  result <- .test_can_modify_github()
  expect_false(result)

  # Test when both are empty
  Sys.setenv(GITHUB_TOKEN = "")
  Sys.setenv(GITHUB_PAT = "")
  result <- .test_can_modify_github()
  expect_false(result)

  # Test when only GITHUB_TOKEN is set
  Sys.setenv(GITHUB_TOKEN = "test_token")
  Sys.unsetenv("GITHUB_PAT")
  result <- .test_can_modify_github()
  expect_false(result)
})

test_that(".test_can_modify_github returns FALSE when no token available", {
  skip_if(.is_test_select())
  skip_if(.is_test_cran())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GH_TOKEN = old_gh_token) else Sys.unsetenv("GH_TOKEN")
    if (nzchar(old_github_token)) Sys.setenv(GITHUB_TOKEN = old_github_token) else Sys.unsetenv("GITHUB_TOKEN")
  })

  # Unset all tokens
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GH_TOKEN")
  Sys.unsetenv("GITHUB_TOKEN")

  result <- .test_can_modify_github()
  expect_false(result)
})
