# Tests for piggyback retry logic with exponential backoff

test_that(".pb_error_classify correctly identifies error types", {
  skip_if(.is_test_select())

  # Test null result
  expect_identical(.pb_error_classify(NULL), "null result")

  # Test empty result
  empty_df <- data.frame(x = character(0))
  expect_identical(.pb_error_classify(empty_df), "empty result")

  # Test rate limit error
  rate_limit_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "API rate limit exceeded")
  )
  expect_identical(.pb_error_classify(rate_limit_error), "rate limit exceeded")

  # Test timeout error
  timeout_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "Connection timed out")
  )
  expect_identical(.pb_error_classify(timeout_error), "timeout")

  # Test network error
  network_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "Network connection failed")
  )
  expect_identical(.pb_error_classify(network_error), "network error")

  # Test 403 error
  forbidden_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "HTTP 403 Forbidden")
  )
  expect_identical(.pb_error_classify(forbidden_error), "permission denied")

  # Test 404 error
  not_found_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "HTTP 404 not found")
  )
  expect_identical(.pb_error_classify(not_found_error), "not found")

  # Test server error
  server_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "HTTP 500 server error")
  )
  expect_identical(.pb_error_classify(server_error), "server error")

  # Test unknown error
  unknown_error <- structure(
    list(),
    class = "try-error",
    condition = list(message = "Something went wrong")
  )
  expect_identical(.pb_error_classify(unknown_error), "unknown error")
})

test_that(".pb_retry_with_backoff respects max_attempts", {
  skip_if(.is_test_select())

  attempt_count <- 0

  # Function that always fails
  failing_fn <- function() {
    attempt_count <<- attempt_count + 1
    structure(list(), class = "try-error", condition = list(message = "Test error"))
  }

  result <- .pb_retry_with_backoff(
    fn = failing_fn,
    max_attempts = 3,
    exponential_base = 0.1,
    operation_name = "test operation",
    output_level = "none",
    check_success = function(x) !inherits(x, "try-error")
  )

  # Should have attempted exactly 3 times
  expect_equal(attempt_count, 3)

  # Result should still be an error
  expect_true(inherits(result, "try-error"))
})

test_that(".pb_retry_with_backoff succeeds on retry", {
  skip_if(.is_test_select())

  attempt_count <- 0

  # Function that fails twice then succeeds
  retry_fn <- function() {
    attempt_count <<- attempt_count + 1
    if (attempt_count < 3) {
      structure(list(), class = "try-error", condition = list(message = "Temporary failure"))
    } else {
      "success"
    }
  }

  result <- .pb_retry_with_backoff(
    fn = retry_fn,
    max_attempts = 5,
    exponential_base = 0.1,
    operation_name = "test retry",
    output_level = "none",
    check_success = function(x) !inherits(x, "try-error")
  )

  # Should have attempted 3 times (failed twice, succeeded on third)
  expect_equal(attempt_count, 3)

  # Result should be success
  expect_identical(result, "success")
})

test_that(".pb_retry_with_backoff applies exponential backoff", {
  skip_if(.is_test_select())

  attempt_times <- c()

  # Function that records attempt times
  timing_fn <- function() {
    attempt_times <<- c(attempt_times, Sys.time())
    structure(list(), class = "try-error", condition = list(message = "Test"))
  }

  result <- .pb_retry_with_backoff(
    fn = timing_fn,
    max_attempts = 4,
    exponential_base = 0.5,
    backoff_factor = 2,
    operation_name = "test backoff",
    output_level = "none",
    check_success = function(x) FALSE  # Always fail to test all delays
  )

  # Should have 4 attempts
  expect_identical(length(attempt_times), 4L)

  # Check delays are approximately exponential
  # Delay 1: none (first attempt)
  # Delay 2: ~0.5s
  # Delay 3: ~1.0s
  # Delay 4: ~2.0s

  if (length(attempt_times) >= 3) {
    delay1 <- as.numeric(difftime(attempt_times[2], attempt_times[1], units = "secs"))
    delay2 <- as.numeric(difftime(attempt_times[3], attempt_times[2], units = "secs"))

    # Check delays are increasing (within tolerance for system timing)
    expect_true(delay2 > delay1 * 1.5)  # Should be roughly 2x but allow some tolerance
  }
})

test_that(".pb_tbl_redo_check works correctly", {
  skip_if(.is_test_select())

  # NULL should trigger redo
  expect_true(.pb_tbl_redo_check(NULL))

  # try-error should trigger redo
  error_obj <- structure(list(), class = "try-error")
  expect_true(.pb_tbl_redo_check(error_obj))

  # Empty dataframe should trigger redo
  empty_df <- data.frame(x = character(0))
  expect_true(.pb_tbl_redo_check(empty_df))

  # Non-empty dataframe should NOT trigger redo
  valid_df <- data.frame(x = c("a", "b"))
  expect_false(.pb_tbl_redo_check(valid_df))
})
