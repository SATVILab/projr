.projr_test_setup_project <- function(git = TRUE,
                                      set_env_var = TRUE,
                                      base_name = "test_projr") {
  # set up directory
  dir_test <- file.path(tempdir(), paste0(base_name))
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  withr::defer(unlink(dir_test))
  dir.create(dir_test)

  if (set_env_var) {
    Sys.setenv("PROJR_TEST" = "TRUE")
    withr::defer(Sys.unsetenv("PROJR_TEST"))
  }

  # copy files
  fn_vec <- list.files(testthat::test_path("./project_structure"))

  for (x in fn_vec) {
    file.copy(
      file.path(testthat::test_path("./project_structure"), x),
      file.path(dir_test, x),
      overwrite = TRUE
    )
  }

  # create .gitignore and .Rbuildignore
  gitignore <- c(
    "# R", ".Rproj.user", ".Rhistory", ".RData",
    ".Ruserdata", "", "# docs", "docs/*"
  )
  writeLines(gitignore, file.path(dir_test, ".gitignore"))

  rbuildignore <- c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^docs$")
  writeLines(rbuildignore, file.path(dir_test, ".Rbuildignore"))

  # create git repo
  if (git) {
    gert::git_init(dir_test)
  }
  dir_test
}
