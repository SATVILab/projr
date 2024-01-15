test_that(".projr_change_get_manifest works", {
  skip_if(.is_test_select())
  dir_test <- .projr_test_setup_project(git = FALSE, set_env_var = TRUE)
  usethis::with_project(
    path = dir_test,
    code = {
      expect_true(.is_string(.projr_auth_get_github_pat()))
      pat_old <- Sys.unsetenv("GITHUB_PAT")
      .projr_auth_get_github_pat()
      Sys.setenv("GITHUB_PAT" = pat_old)
      suppressWarnings(.projr_auth_get_github_pat_warn())
      suppressWarnings(.projr_auth_get_osf_pat())
      pat_old <- Sys.unsetenv("OSF_PAT")
      suppressWarnings(.projr_auth_get_osf_pat())
      Sys.setenv("OSF_PAT" = pat_old)
      suppressMessages(projr_instr_auth_github())
      suppressMessages(projr_instr_auth_osf())
      suppressWarnings(.projr_auth_get_osf_pat_warn())
      Sys.setenv("GITHUB_PAT" = pat_old)
    }
  )
})
