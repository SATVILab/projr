test_that(".change_get_manifest works", {
  skip_if(.is_test_select())
  dir_test <- .test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.is_string(.auth_get_github_pat()))
      pat_old <- Sys.getenv("GITHUB_PAT")
      Sys.unsetenv("GITHUB_PAT")
      suppressWarnings(.auth_get_github_pat())
      Sys.setenv("GITHUB_PAT" = pat_old)
      suppressWarnings(.auth_get_github_pat_warn())
      suppressWarnings(.auth_get_osf_pat())
      pat_old <- Sys.getenv("OSF_PAT")
      Sys.unsetenv("OSF_PAT")
      expect_warning(.auth_get_osf_pat())
      Sys.setenv("OSF_PAT" = pat_old)
      suppressMessages.instr_auth_github())
      suppressMessages.instr_auth_osf())
      expect_warning(.auth_get_osf_pat_warn())
    }
  )
})
