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
