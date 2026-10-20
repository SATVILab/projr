# Keep ordinary tests independent of the user's credentials and Git settings.
.test_home <- tempfile("projr-test-home-")
dir.create(.test_home)
.test_git_config <- file.path(.test_home, "gitconfig")
file.create(.test_git_config)
withr::defer(unlink(.test_home, recursive = TRUE), envir = teardown_env())
withr::local_envvar(
  c(
    HOME = .test_home,
    R_USER = .test_home,
    R_ENVIRON_USER = file.path(.test_home, ".Renviron"),
    R_PROFILE_USER = file.path(.test_home, ".Rprofile"),
    XDG_CONFIG_HOME = .test_home,
    GIT_CONFIG_GLOBAL = .test_git_config,
    GIT_CONFIG_NOSYSTEM = "1",
    GIT_ALLOW_PROTOCOL = "file",
    GIT_TERMINAL_PROMPT = "0",
    GCM_INTERACTIVE = "never",
    GITHUB_PAT = "",
    GITHUB_TOKEN = "",
    GH_TOKEN = "",
    GH_ENTERPRISE_TOKEN = "",
    OSF_PAT = "",
    PROJR_AUTO_INSTALL = "false",
    R_PKG_TEST_IN_PROGRESS = "TRUE"
  ),
  .local_envir = teardown_env()
)

# Credential packages can cache tokens outside HOME. Keep discovery deterministic;
# individual auth tests can install a narrower mock inside their own scope.
if (requireNamespace("gh", quietly = TRUE)) {
  testthat::local_mocked_bindings(
    gh_token = function(...) "",
    .package = "gh",
    .env = teardown_env()
  )
}
if (requireNamespace("gitcreds", quietly = TRUE)) {
  testthat::local_mocked_bindings(
    gitcreds_get = function(...) list(password = ""),
    .package = "gitcreds",
    .env = teardown_env()
  )
}
