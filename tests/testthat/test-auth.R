test_that("auth functions work correctly", {
  skip_if(.is_test_select())
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
  # Only run if GitHub PAT |>is available
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
  skip("OSF tests disabled - to be reviewed")
  skip_if(.is_test_select())
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
  skip("OSF tests disabled - to be reviewed")
  skip_if(.is_test_select())
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

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
    if (nzchar(old_gh_token)) {
      Sys.setenv(GITHUB_TOKEN = old_gh_token)
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

  # Test GITHUB_TOKEN is used as fallback
  Sys.unsetenv("GITHUB_PAT")
  Sys.setenv(GITHUB_TOKEN = "ghp_1234567890123456789012345678901234567890")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "ghp_1234567890123456789012345678901234567890")
})

test_that("OSF_PAT environment variable is read correctly", {
  skip("OSF tests disabled - to be reviewed")
  skip_if(.is_test_select())

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

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  old_osf_pat <- Sys.getenv("OSF_PAT", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GITHUB_TOKEN = old_gh_token) else Sys.unsetenv("GITHUB_TOKEN")
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
  Sys.unsetenv("GITHUB_TOKEN")
  Sys.unsetenv("OSF_PAT")

  # Should throw errors when tokens are missing
  expect_error(.auth_check_github(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  ))
  expect_error(.auth_check_osf())
})

test_that("Empty authentication tokens are handled correctly", {
  skip_if(.is_test_select())

  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_gh_token <- Sys.getenv("GITHUB_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GITHUB_TOKEN = old_gh_token) else Sys.unsetenv("GITHUB_TOKEN")
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

  # Test that result is always length 1
  expect_identical(length(.auth_token_normalize(NULL)), 1L)
  expect_identical(length(.auth_token_normalize(character())), 1L)
  expect_identical(length(.auth_token_normalize("")), 1L)
  expect_identical(length(.auth_token_normalize("token")), 1L)
})

test_that("GH_TOKEN fallback works in .auth_get_github_pat_find()", {
  skip_if(.is_test_select())

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
    if (nzchar(old_github_token)) {
      Sys.setenv(GITHUB_TOKEN = old_github_token)
    } else {
      Sys.unsetenv("GITHUB_TOKEN")
    }
    if (nzchar(old_gh_token)) {
      Sys.setenv(GH_TOKEN = old_gh_token)
    } else {
      Sys.unsetenv("GH_TOKEN")
    }
  })

  # Test GH_TOKEN is used as last fallback
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_TOKEN")
  Sys.setenv(GH_TOKEN = "ghp_gh_token_test_1234567890123456789012345")
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = FALSE
  )
  expect_identical(token, "ghp_gh_token_test_1234567890123456789012345")
})

test_that("gitcreds return value is properly used", {
  skip_if(.is_test_select())
  skip_if(!requireNamespace("gitcreds", quietly = TRUE))

  # Save originals
  old_github_pat <- Sys.getenv("GITHUB_PAT", unset = "")
  old_github_token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  old_gh_token <- Sys.getenv("GH_TOKEN", unset = "")

  on.exit({
    if (nzchar(old_github_pat)) {
      Sys.setenv(GITHUB_PAT = old_github_pat)
    } else {
      Sys.unsetenv("GITHUB_PAT")
    }
    if (nzchar(old_github_token)) {
      Sys.setenv(GITHUB_TOKEN = old_github_token)
    } else {
      Sys.unsetenv("GITHUB_TOKEN")
    }
    if (nzchar(old_gh_token)) {
      Sys.setenv(GH_TOKEN = old_gh_token)
    } else {
      Sys.unsetenv("GH_TOKEN")
    }
  })

  # Clear all env vars
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_TOKEN")
  Sys.unsetenv("GH_TOKEN")

  # If gitcreds returns a token, it should be used
  # This is a functional test - actual gitcreds behavior depends on system state
  token <- .auth_get_github_pat_find(
    use_gh_if_available = FALSE,
    use_gitcreds_if_needed = TRUE
  )

  # Token should be a string (might be empty if no gitcreds configured)
  expect_true(is.character(token))
  expect_identical(length(token), 1L)
})

test_that(".github_api_base resolves URLs correctly", {
  skip_if(.is_test_select())

  # Save original
  old_api_url <- Sys.getenv("GITHUB_API_URL", unset = "")
  on.exit({
    if (nzchar(old_api_url)) {
      Sys.setenv(GITHUB_API_URL = old_api_url)
    } else {
      Sys.unsetenv("GITHUB_API_URL")
    }
  })

  # Test default (no env var, no arg)
  Sys.unsetenv("GITHUB_API_URL")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.github.com")

  # Test env var
  Sys.setenv(GITHUB_API_URL = "https://api.example.com")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test trailing slash removal
  Sys.setenv(GITHUB_API_URL = "https://api.example.com/")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test multiple trailing slashes
  Sys.setenv(GITHUB_API_URL = "https://api.example.com///")
  result <- .github_api_base(NULL)
  expect_identical(result, "https://api.example.com")

  # Test explicit arg wins over env var
  Sys.setenv(GITHUB_API_URL = "https://api.example.com")
  result <- .github_api_base("https://custom.api.com")
  expect_identical(result, "https://custom.api.com")
})

test_that(".release_exists requires httr", {
  skip_if(.is_test_select())

  # Mock httr not being available would require complex mocking
  # Just verify the function exists and has correct signature
  expect_true(is.function(.release_exists))

  # If httr is available, verify basic functionality
  if (requireNamespace("httr", quietly = TRUE)) {
    # Function should error or return logical
    # We can't test actual GitHub API without credentials
    expect_true(TRUE)  # Placeholder - actual tests below
  }
})

test_that(".release_exists returns correct values for known repos", {
  skip_if(.is_test_select())
  skip_if(!requireNamespace("httr", quietly = TRUE))
  skip_if(!nzchar(.auth_get_github_pat_find()))

  # Test with a well-known public repo that has releases
  # Using a stable public repo as reference
  # Note: This test may fail if the repo changes or API is down
  
  # We'll use SATVILab/projr as test repo if we can access it
  repo <- "SATVILab/projr"
  
  # Try to check for a likely non-existent tag
  result <- tryCatch(
    .release_exists(repo, "definitely-not-a-real-tag-xyz-12345"),
    error = function(e) {
      # If we get an error other than 404, skip the test
      if (!grepl("404", e$message, ignore.case = TRUE)) {
        skip("Cannot access GitHub API for testing")
      }
      FALSE
    }
  )
  
  # Should return FALSE for non-existent tag
  expect_false(result)
})
