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

.test_unset <- function() {
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
}

.test_unset_fast <- function() {
  Sys.unsetenv("R_PKG_TEST_FAST")
}

.test_unset_select <- function() {
  Sys.unsetenv("R_PKG_TEST_SELECT")
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
