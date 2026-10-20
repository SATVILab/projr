# Live GitHub tests require an explicit opt-in before credential or network access.
.test_can_modify_github <- function() {
  identical(Sys.getenv("PROJR_TEST_LIVE_GITHUB"), "true")
}

.test_skip_if_cannot_modify_github <- function() {
  if (!.test_can_modify_github()) {
    testthat::skip("Live GitHub tests disabled (PROJR_TEST_LIVE_GITHUB=true to opt in)")
  }
  invisible(TRUE)
}

# Repository lifecycle tests need an explicitly managed fixture. Never create or
# delete a repository automatically, even when the live lane is enabled.
.test_github_repo_create <- function(...) {
  testthat::skip("Automatic GitHub repository creation is disabled; use a managed fixture")
}
