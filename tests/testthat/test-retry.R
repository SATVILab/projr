test_that(".retry_with_backoff succeeds on first attempt", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      "success"
    },
    max_attempts = 3,
    operation_name = "test operation"
  )

  expect_identical(result, "success")
  expect_equal(counter, 1)
})

test_that(".retry_with_backoff retries until success", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter < 3) {
        return(FALSE)
      }
      TRUE
    },
    max_attempts = 5,
    initial_delay = 0,
    operation_name = "test retry",
    check_success = function(x) isTRUE(x)
  )

  expect_true(result)
  expect_equal(counter, 3)
})

test_that(".retry_with_backoff exhausts all attempts", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      FALSE
    },
    max_attempts = 3,
    initial_delay = 0,
    operation_name = "test fail",
    check_success = function(x) isTRUE(x)
  )

  expect_false(result)
  expect_equal(counter, 3)
})

test_that(".retry_with_backoff respects max_total_time", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  start_time <- proc.time()[3]
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      FALSE
    },
    max_attempts = 100,
    initial_delay = 0.1,
    max_total_time = 0.3,
    backoff_factor = 1,
    operation_name = "test timeout",
    check_success = function(x) isTRUE(x)
  )
  elapsed <- proc.time()[3] - start_time

  expect_false(result)
  # Should stop before all attempts due to time constraint
  expect_true(counter < 100)
  # Should respect the time limit (with some tolerance)
  expect_true(elapsed < 1.0)
})

test_that(".retry_with_backoff calls on_retry callback", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  retry_attempts <- c()
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter < 3) {
        return(FALSE)
      }
      TRUE
    },
    max_attempts = 5,
    initial_delay = 0,
    operation_name = "test callback",
    check_success = function(x) isTRUE(x),
    on_retry = function(attempt) {
      retry_attempts <<- c(retry_attempts, attempt)
    }
  )

  expect_true(result)
  expect_equal(counter, 3)
  # on_retry should be called before attempts 2 and 3
  expect_identical(retry_attempts, c(2L, 3L))
})

test_that(".retry_with_backoff uses error_classifier", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  error_types <- c()
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter < 3) {
        return(list(error = "temporary"))
      }
      list(success = TRUE)
    },
    max_attempts = 5,
    initial_delay = 0,
    operation_name = "test classifier",
    check_success = function(x) !is.null(x$success),
    error_classifier = function(x) {
      if (!is.null(x$error)) {
        return(x$error)
      }
      "unknown"
    }
  )

  expect_true(!is.null(result$success))
  expect_equal(counter, 3)
})

test_that(".retry_with_backoff applies exponential backoff", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  delays <- c()
  last_time <- proc.time()[3]

  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter > 1) {
        current_time <- proc.time()[3]
        delays <<- c(delays, current_time - last_time)
        last_time <<- current_time
      }
      if (counter < 4) {
        return(FALSE)
      }
      TRUE
    },
    max_attempts = 4,
    initial_delay = 0.05,
    exponential_base = 2,
    backoff_factor = 2,
    operation_name = "test backoff",
    check_success = function(x) isTRUE(x)
  )

  expect_true(result)
  expect_equal(counter, 4)
  # Should have 3 delays (before attempts 2, 3, 4)
  expect_equal(length(delays), 3)
  # Delays should generally increase (with tolerance for timing variations)
  # First delay should be around initial_delay
  expect_true(delays[1] >= 0.04 && delays[1] <= 0.2)
  # Each subsequent delay should be larger (exponential_base * backoff_factor)
  # But we allow tolerance due to system timing
  expect_true(delays[2] >= delays[1] * 0.8)
  expect_true(delays[3] >= delays[2] * 0.8)
})

test_that(".retry_with_backoff respects max_delay", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  delays <- c()
  last_time <- proc.time()[3]

  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter > 1) {
        current_time <- proc.time()[3]
        delays <<- c(delays, current_time - last_time)
        last_time <<- current_time
      }
      if (counter < 4) {
        return(FALSE)
      }
      TRUE
    },
    max_attempts = 4,
    initial_delay = 0.05,
    exponential_base = 10,
    backoff_factor = 10,
    max_delay = 0.1,
    operation_name = "test max_delay",
    check_success = function(x) isTRUE(x)
  )

  expect_true(result)
  expect_equal(counter, 4)
  # All delays should be capped at max_delay
  # Allow more tolerance for timing variations
  expect_true(all(delays <= 0.3))
})

test_that(".retry_with_backoff with initial_delay of 0 starts immediately", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  start_time <- proc.time()[3]
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      if (counter < 2) {
        return(FALSE)
      }
      TRUE
    },
    max_attempts = 3,
    initial_delay = 0,
    operation_name = "test no initial delay",
    check_success = function(x) isTRUE(x)
  )
  elapsed <- proc.time()[3] - start_time

  expect_true(result)
  expect_equal(counter, 2)
  # Should complete quickly without initial delay
  expect_true(elapsed < 0.5)
})

test_that(".retry_with_backoff returns last result when attempts exhausted", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      list(attempt = counter, status = "failed")
    },
    max_attempts = 3,
    initial_delay = 0,
    operation_name = "test last result",
    check_success = function(x) FALSE
  )

  # Should return the result from the last attempt
  expect_equal(result$attempt, 3)
  expect_identical(result$status, "failed")
})

test_that(".retry_with_backoff bounds delay by remaining time", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      FALSE
    },
    max_attempts = 10,
    initial_delay = 0.1,
    backoff_factor = 10,
    max_delay = 60,
    max_total_time = 0.3,
    operation_name = "test bounded delay",
    check_success = function(x) isTRUE(x)
  )

  expect_false(result)
  # Should stop early due to time constraint
  expect_true(counter < 10)
})

test_that(".retry_with_backoff check_success defaults to always true", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      "any result"
    },
    max_attempts = 3,
    operation_name = "test default success"
    # No check_success provided - defaults to function(x) TRUE
  )

  # Should succeed on first attempt with default check_success
  expect_identical(result, "any result")
  expect_equal(counter, 1)
})

test_that(".retry_with_backoff handles complex check_success conditions", {
  skip_if(.is_test_cran())
  skip_if(.is_test_select())

  counter <- 0
  result <- .retry_with_backoff(
    fn = function() {
      counter <<- counter + 1
      list(code = counter * 100, message = "processing")
    },
    max_attempts = 5,
    initial_delay = 0,
    operation_name = "test complex success",
    check_success = function(x) {
      # Only succeed if code is 300 or higher
      !is.null(x$code) && x$code >= 300
    }
  )

  expect_equal(counter, 3)
  expect_equal(result$code, 300)
})
