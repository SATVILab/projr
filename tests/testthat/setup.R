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

# cffr 1.4.2 fetches repository indexes on first load. Seed its cache offline,
# then restore the imported function copied while utils was temporarily mocked.
local({
  original_available_packages <- utils::available.packages
  loaded <- testthat::with_mocked_bindings(
    requireNamespace("cffr", quietly = TRUE),
    available.packages = function(...) {
      matrix(
        character(),
        nrow = 0L,
        ncol = 2L,
        dimnames = list(NULL, c("Package", "Repository"))
      )
    },
    .package = "utils"
  )
  if (loaded) {
    imports <- parent.env(asNamespace("cffr"))
    was_locked <- bindingIsLocked("available.packages", imports)
    if (was_locked) {
      unlockBinding("available.packages", imports)
    }
    assign("available.packages", original_available_packages, envir = imports)
    if (was_locked) {
      lockBinding("available.packages", imports)
    }
    stopifnot(identical(
      get("available.packages", envir = imports, inherits = FALSE),
      original_available_packages
    ))
  }
  stopifnot(identical(utils::available.packages, original_available_packages))
})
