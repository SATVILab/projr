# Test mode functions
# Tests for R/test.R

# Test setter functions --------------------------------------------------------

test_that(".test_set works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
  expect_false(.is_test())

  # Set and verify
  .test_set()
  expect_identical(Sys.getenv("R_PKG_TEST_IN_PROGRESS"), "TRUE")
  expect_true(.is_test())

  # Clean up
  .test_unset()
})

test_that(".test_set_select works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_SELECT")
  expect_false(.is_test_select())

  # Set and verify
  .test_set_select()
  expect_identical(Sys.getenv("R_PKG_TEST_SELECT"), "TRUE")
  expect_true(.is_test_select())

  # Clean up
  .test_unset_select()
})

test_that(".test_set_cran works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_CRAN")
  expect_false(.is_test_cran())

  # Set and verify
  .test_set_cran()
  expect_identical(Sys.getenv("R_PKG_TEST_CRAN"), "TRUE")
  expect_true(.is_test_cran())

  # Clean up
  .test_unset_cran()
})

test_that(".test_set_lite works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")
  expect_false(.is_test_lite())

  # Set and verify
  .test_set_lite()
  expect_identical(Sys.getenv("R_PKG_TEST_LITE"), "TRUE")
  expect_true(.is_test_lite())

  # Clean up
  .test_unset_lite()
})

test_that(".test_set_debug works (backward compatibility)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")
  expect_false(.is_test_lite())
  expect_false(.is_test_debug())

  # Set via debug function and verify it sets LITE mode
  .test_set_debug()
  expect_identical(Sys.getenv("R_PKG_TEST_LITE"), "TRUE")
  expect_true(.is_test_lite())
  expect_true(.is_test_debug())

  # Clean up
  .test_unset_debug()
})

# Test unsetter functions ------------------------------------------------------

test_that(".test_unset works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set and verify it's set
  .test_set()
  expect_true(.is_test())

  # Unset and verify
  .test_unset()
  expect_identical(Sys.getenv("R_PKG_TEST_IN_PROGRESS"), "")
  expect_false(.is_test())
})

test_that(".test_unset_select works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set and verify it's set
  .test_set_select()
  expect_true(.is_test_select())

  # Unset and verify
  .test_unset_select()
  expect_identical(Sys.getenv("R_PKG_TEST_SELECT"), "")
  expect_false(.is_test_select())
})

test_that(".test_unset_cran works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set and verify it's set
  .test_set_cran()
  expect_true(.is_test_cran())

  # Unset and verify
  .test_unset_cran()
  expect_identical(Sys.getenv("R_PKG_TEST_CRAN"), "")
  expect_false(.is_test_cran())
})

test_that(".test_unset_lite works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set and verify it's set
  .test_set_lite()
  expect_true(.is_test_lite())

  # Unset and verify
  .test_unset_lite()
  expect_identical(Sys.getenv("R_PKG_TEST_LITE"), "")
  expect_false(.is_test_lite())
})

test_that(".test_unset_debug works (backward compatibility)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set via debug and verify it's set
  .test_set_debug()
  expect_true(.is_test_debug())

  # Unset via debug function and verify
  .test_unset_debug()
  expect_identical(Sys.getenv("R_PKG_TEST_LITE"), "")
  expect_false(.is_test_lite())
  expect_false(.is_test_debug())
})

# Test checker functions -------------------------------------------------------

test_that(".is_test works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
  expect_false(.is_test())

  # Various values
  Sys.setenv(R_PKG_TEST_IN_PROGRESS = "TRUE")
  expect_true(.is_test())

  Sys.setenv(R_PKG_TEST_IN_PROGRESS = "FALSE")
  expect_false(.is_test())

  Sys.setenv(R_PKG_TEST_IN_PROGRESS = "")
  expect_false(.is_test())

  # Clean up
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
})

test_that(".is_test_select works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_SELECT")
  expect_false(.is_test_select())

  # Various values
  Sys.setenv(R_PKG_TEST_SELECT = "TRUE")
  expect_true(.is_test_select())

  Sys.setenv(R_PKG_TEST_SELECT = "FALSE")
  expect_false(.is_test_select())

  Sys.setenv(R_PKG_TEST_SELECT = "")
  expect_false(.is_test_select())

  # Clean up
  Sys.unsetenv("R_PKG_TEST_SELECT")
})

test_that(".is_test_cran works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_CRAN")
  expect_false(.is_test_cran())

  # Various values
  Sys.setenv(R_PKG_TEST_CRAN = "TRUE")
  expect_true(.is_test_cran())

  Sys.setenv(R_PKG_TEST_CRAN = "FALSE")
  expect_false(.is_test_cran())

  Sys.setenv(R_PKG_TEST_CRAN = "")
  expect_false(.is_test_cran())

  # Clean up
  Sys.unsetenv("R_PKG_TEST_CRAN")
})

test_that(".is_test_lite works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")
  expect_false(.is_test_lite())

  # Various values
  Sys.setenv(R_PKG_TEST_LITE = "TRUE")
  expect_true(.is_test_lite())

  Sys.setenv(R_PKG_TEST_LITE = "FALSE")
  expect_false(.is_test_lite())

  Sys.setenv(R_PKG_TEST_LITE = "")
  expect_false(.is_test_lite())

  # Clean up
  Sys.unsetenv("R_PKG_TEST_LITE")
})

test_that(".is_test_debug works (backward compatibility)", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")
  expect_false(.is_test_debug())

  # Set LITE and verify debug also returns true
  Sys.setenv(R_PKG_TEST_LITE = "TRUE")
  expect_true(.is_test_lite())
  expect_true(.is_test_debug())

  Sys.setenv(R_PKG_TEST_LITE = "FALSE")
  expect_false(.is_test_lite())
  expect_false(.is_test_debug())

  # Clean up
  Sys.unsetenv("R_PKG_TEST_LITE")
})

# Test interactions between modes ----------------------------------------------

test_that("multiple test modes can coexist", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
  Sys.unsetenv("R_PKG_TEST_SELECT")
  Sys.unsetenv("R_PKG_TEST_CRAN")
  Sys.unsetenv("R_PKG_TEST_LITE")

  # Set multiple modes
  .test_set()
  .test_set_lite()
  .test_set_cran()

  # Verify all are set
  expect_true(.is_test())
  expect_true(.is_test_lite())
  expect_true(.is_test_cran())

  # Unset one at a time
  .test_unset()
  expect_false(.is_test())
  expect_true(.is_test_lite())
  expect_true(.is_test_cran())

  .test_unset_lite()
  expect_false(.is_test())
  expect_false(.is_test_lite())
  expect_true(.is_test_cran())

  .test_unset_cran()
  expect_false(.is_test())
  expect_false(.is_test_lite())
  expect_false(.is_test_cran())
})

test_that("environment variables persist across function calls", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")

  # Set in one context
  local({
    .test_set_lite()
  })

  # Check in another context
  local({
    expect_true(.is_test_lite())
  })

  # Clean up
  .test_unset_lite()
})

test_that("backward compatibility functions work identically to new ones", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Clean state
  Sys.unsetenv("R_PKG_TEST_LITE")

  # Test set_debug = set_lite
  .test_set_debug()
  expect_true(.is_test_lite())
  expect_true(.is_test_debug())
  .test_unset_debug()
  expect_false(.is_test_lite())
  expect_false(.is_test_debug())

  # Test set_lite has same effect
  .test_set_lite()
  expect_true(.is_test_lite())
  expect_true(.is_test_debug())
  .test_unset_lite()
  expect_false(.is_test_lite())
  expect_false(.is_test_debug())
})

# Test edge cases --------------------------------------------------------------

test_that("unsetting already unset variables works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Ensure variables are not set
  Sys.unsetenv("R_PKG_TEST_IN_PROGRESS")
  Sys.unsetenv("R_PKG_TEST_SELECT")
  Sys.unsetenv("R_PKG_TEST_CRAN")
  Sys.unsetenv("R_PKG_TEST_LITE")

  # Unset again should not cause errors
  expect_silent(.test_unset())
  expect_silent(.test_unset_select())
  expect_silent(.test_unset_cran())
  expect_silent(.test_unset_lite())
  expect_silent(.test_unset_debug())
})

test_that("setting already set variables works", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  # Set variables
  .test_set()
  .test_set_select()
  .test_set_cran()
  .test_set_lite()

  # Set again should not cause errors
  expect_silent(.test_set())
  expect_silent(.test_set_select())
  expect_silent(.test_set_cran())
  expect_silent(.test_set_lite())
  expect_silent(.test_set_debug())

  # Clean up
  .test_unset()
  .test_unset_select()
  .test_unset_cran()
  .test_unset_lite()
})
