# tests
# ---------------

.test_set <- function() {
  Sys.setenv("R_PKG_TEST_IN_PROGRESS" = "TRUE")
}

# if you add skip_if(.is_test_fast())
# to slow-running tests, then
# you can skip them by
# running .test_set_fast().
# undo by running .test_unset_fast()
.test_set_fast <- function() {
  Sys.setenv("R_PKG_TEST_FAST" = "TRUE")
}

# if you add skip_if(.is_test_select())
# to each test, then you can skip
# all tests except those for which
# you're testing by
# uncommenting skip_if(.is_test_select())
# if you run .test_set_select().
# undo by running .test_unset_select()
.test_set_select <- function() {
  Sys.setenv("R_PKG_TEST_SELECT" = "TRUE")
}

# CRAN test mode: runs only fast, essential tests
# without remote dependencies. Target: <2 minutes
# if you add skip_if(.is_test_cran())
# to comprehensive/integration/remote tests,
# they will be skipped when running in CRAN mode.
# Set with .test_set_cran()
# Undo with .test_unset_cran()
.test_set_cran <- function() {
  Sys.setenv("R_PKG_TEST_CRAN" = "TRUE")
}

# Lite test mode: runs core functionality tests
# without exhaustive parameter combinations.
# Faster than full suite for quick validation.
# if you add skip_if(.is_test_lite())
# to comprehensive tests, they will be skipped
# when running in lite mode.
# Set with .test_set_lite()
# Undo with .test_unset_lite()
.test_set_lite <- function() {
  Sys.setenv("R_PKG_TEST_LITE" = "TRUE")
}

# Kept for backward compatibility - use .test_set_lite() instead
.test_set_debug <- function() {
  .test_set_lite()
}

.test_unset <- function() {
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
}

.test_unset_fast <- function() {
  Sys.unsetenv("R_PKG_TEST_FAST")
}

.test_unset_select <- function() {
  Sys.unsetenv("R_PKG_TEST_SELECT")
}

.test_unset_cran <- function() {
  Sys.unsetenv("R_PKG_TEST_CRAN")
}

.test_unset_lite <- function() {
  Sys.unsetenv("R_PKG_TEST_LITE")
}

# Kept for backward compatibility - use .test_unset_lite() instead
.test_unset_debug <- function() {
  .test_unset_lite()
}

.is_test <- function() {
  Sys.getenv("R_PKG_TEST_IN_PROGRESS") == "TRUE"
}

# for slow functions
.is_test_fast <- function() {
  Sys.getenv("R_PKG_TEST_FAST") == "TRUE"
}

# so that we can skip all except those for
# which we're testing
.is_test_select <- function() {
  Sys.getenv("R_PKG_TEST_SELECT") == "TRUE"
}

# Check if running in CRAN test mode
# CRAN mode skips:
# - Comprehensive tests (exhaustive parameter combinations)
# - Integration tests
# - Remote-dependent tests (GitHub/OSF)
# Automatically enabled when NOT_CRAN is false/unset
.is_test_cran <- function() {
  # Explicitly set via R_PKG_TEST_CRAN
  if (Sys.getenv("R_PKG_TEST_CRAN") == "TRUE") {
    return(TRUE)
  }
  # Auto-detect: if NOT_CRAN is not "true", we're in CRAN mode
  not_cran <- tolower(Sys.getenv("NOT_CRAN", ""))
  if (not_cran %in% c("", "false", "f", "0")) {
    return(TRUE)
  }
  FALSE
}

# Check if running in lite test mode
# Lite mode skips:
# - Comprehensive tests (exhaustive parameter combinations)
# Includes: Core functionality tests, integration tests, selected remote tests
.is_test_lite <- function() {
  Sys.getenv("R_PKG_TEST_LITE") == "TRUE"
}

# Kept for backward compatibility - use .is_test_lite() instead
.is_test_debug <- function() {
  .is_test_lite()
}
