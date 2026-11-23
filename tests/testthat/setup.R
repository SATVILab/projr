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
    GIT_AUTHOR_NAME = "DarthVader",
    GIT_AUTHOR_EMAIL = "number_one_fan@tellytubbies.com",
    GIT_COMMITTER_NAME = "DarthVader",
    GIT_COMMITTER_EMAIL = "number_one_fan@tellytubbies.com",
    GIT_ALLOW_PROTOCOL = "file",
    GIT_TERMINAL_PROMPT = "0",
    GCM_INTERACTIVE = "never",
    PROJR_TEST_LIVE_GITHUB = "false",
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

# Run the fast offline suite under a standard R CMD check. CI can select its
# own test mode or use NOT_CRAN=true to run the full suite.
if (!nzchar(Sys.getenv("R_PKG_TEST_CRAN")) &&
    nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
    !.is_env_var_true("NOT_CRAN")) {
  withr::local_envvar(c(R_PKG_TEST_CRAN = "true"), .local_envir = teardown_env())
}

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

# CodeMeta looks up dependency origins in two online source-package indexes.
if (requireNamespace("codemeta", quietly = TRUE)) {
  testthat::local_mocked_bindings(
    available_source_packages = function(...) {
      matrix(character(), nrow = 0L, ncol = 1L, dimnames = list(NULL, "Package"))
    },
    .package = "codemeta",
    .env = teardown_env()
  )
}

# cffr reads a source package index on its first load and requires a URL for
# CRAN. Give R a cached, empty index for a dedicated fixture repository.
local({
  repo <- "https://projr-fixture.invalid"
  withr::local_options(list(repos = c(CRAN = repo), pkgType = "source"))
  withr::local_envvar(c(R_PACKAGES_CACHE_USER_DIR = "false", R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE = "3600"))
  index <- utils::URLencode(utils::contrib.url(repo, "source"), reserved = TRUE)
  cache <- file.path(tempdir(), paste0("repos_", index, ".rds"))
  saveRDS(matrix(character(), nrow = 0L, ncol = 0L), cache)
  withr::defer(unlink(cache))
  requireNamespace("cffr", quietly = TRUE)
})
