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
      # Save current PAT
      pat_old <- Sys.getenv("GITHUB_PAT")
      
      # Unset all possible GitHub tokens
      Sys.unsetenv("GITHUB_PAT")
      Sys.unsetenv("GITHUB_TOKEN")
      
      # Should throw error when no auth
      expect_error(.auth_check_github(), "GitHub authentication is required")
      expect_error(.auth_check_github("test operation"), "test operation")
      
      # Restore PAT
      if (nzchar(pat_old)) {
        Sys.setenv("GITHUB_PAT" = pat_old)
      }
    }
  )
})

test_that(".auth_check_github succeeds when auth available", {
  skip_if(.is_test_select())
  # Only run if GitHub PAT is available
  skip_if(!nzchar(Sys.getenv("GITHUB_PAT")))
  
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
    if (nzchar(old_github_pat)) Sys.setenv(GITHUB_PAT = old_github_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_gh_token)) Sys.setenv(GITHUB_TOKEN = old_gh_token) else Sys.unsetenv("GITHUB_TOKEN")
  })
  
  # Test GITHUB_PAT is checked first
  Sys.setenv(GITHUB_PAT = "test_pat")
  Sys.unsetenv("GITHUB_TOKEN")
  token <- .auth_get_github_pat_find()
  expect_identical(token, "test_pat")
  
  # Test GITHUB_TOKEN is used as fallback
  Sys.unsetenv("GITHUB_PAT")
  Sys.setenv(GITHUB_TOKEN = "test_gh_token")
  token <- .auth_get_github_pat_find()
  expect_identical(token, "test_gh_token")
  
  # Test GITHUB_PAT takes precedence over GITHUB_TOKEN
  Sys.setenv(GITHUB_PAT = "test_pat_priority")
  Sys.setenv(GITHUB_TOKEN = "test_gh_token_ignored")
  token <- .auth_get_github_pat_find()
  expect_identical(token, "test_pat_priority")
})

test_that("OSF_PAT environment variable is read correctly", {
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
  
  # Unset all tokens
  Sys.unsetenv("GITHUB_PAT")
  Sys.unsetenv("GITHUB_TOKEN")
  Sys.unsetenv("OSF_PAT")
  
  # Should throw errors when tokens are missing
  expect_error(.auth_check_github())
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
  
  # Empty GITHUB_PAT should fallback to GITHUB_TOKEN
  Sys.setenv(GITHUB_PAT = "")
  Sys.setenv(GITHUB_TOKEN = "test_token")
  token <- .auth_get_github_pat_find()
  expect_identical(token, "test_token")
  
  # Both empty should return empty string
  Sys.setenv(GITHUB_PAT = "")
  Sys.setenv(GITHUB_TOKEN = "")
  token <- .auth_get_github_pat_find()
  expect_identical(token, "")
})
