.retry_with_backoff <- function(fn,
                                max_attempts = 3,
                                initial_delay = 0,
                                exponential_base = 2,
                                max_delay = 60,
                                backoff_factor = 2,
                                max_total_time = Inf, # overall cap in seconds
                                operation_name = "operation",
                                output_level = "std",
                                check_success = function(x) TRUE,
                                on_retry = NULL,
                                error_classifier = NULL) {
  start_time <- proc.time()[3]

  .cli_debug(
    "{operation_name}: starting (max {max_attempts} attempts, max {max_total_time}s total)",
    output_level = output_level
  )

  delay <- initial_delay
  last_result <- NULL

  for (attempt in seq_len(max_attempts)) {
    elapsed <- proc.time()[3] - start_time
    if (elapsed >= max_total_time) {
      .cli_debug(
        "{operation_name}: stopping before attempt {attempt} as max_total_time ({max_total_time}s) exceeded",
        output_level = output_level
      )
      break
    }

    if (attempt > 1) {
      if (!is.null(on_retry)) {
        on_retry(attempt)
      }

      remaining <- max_total_time - elapsed
      # Bound the sleep so we do not run past the total cap
      actual_delay <- min(delay, max_delay, remaining)

      if (actual_delay > 0) {
        .cli_debug(
          "{operation_name}: waiting {actual_delay}s before attempt {attempt}/{max_attempts}...", # nolint
          output_level = output_level
        )
        Sys.sleep(actual_delay)
      }

      delay <- exponential_base * backoff_factor
    }

    last_result <- fn()

    if (check_success(last_result)) {
      .cli_debug(
        "{operation_name}: succeeded on attempt {attempt}/{max_attempts}",
        output_level = output_level
      )
      return(last_result)
    }

    if (!is.null(error_classifier)) {
      error_type <- error_classifier(last_result)
      .cli_debug(
        "{operation_name}: attempt {attempt}/{max_attempts} failed ({error_type})",
        output_level = output_level
      )
    } else {
      .cli_debug(
        "{operation_name}: attempt {attempt}/{max_attempts} failed",
        output_level = output_level
      )
    }
  }

  .cli_debug(
    "{operation_name}: all attempts exhausted or max_total_time reached",
    output_level = output_level
  )

  last_result
}
