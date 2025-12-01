# Piggyback Retry and Error Handling Logic
# ==========================================
# This file contains helper functions for retrying piggyback operations
# and classifying errors that occur during GitHub release interactions.

# ========================
# Redo Check
# ========================

.pb_tbl_redo_check <- function(x) {
  # Returns TRUE if we should redo (failure condition)
  # Returns FALSE if we should stop (success condition)

  # NULL should trigger redo
  if (is.null(x)) return(TRUE)

  # try-error should trigger redo
  if (inherits(x, "try-error")) return(TRUE)

  # Empty dataframe should trigger redo
  if (is.data.frame(x) && nrow(x) == 0) return(TRUE)

  # Otherwise do not redo (success)
  FALSE
}

# ========================
# Error Classifier
# ========================

.pb_error_classify <- function(result) {
  if (is.null(result)) {
    return("null result")
  }

  if (is.data.frame(result) && nrow(result) == 0) {
    return("empty result")
  }

  if (inherits(result, "try-error")) {
    msg <- attr(result, "condition")$message
    if (is.null(msg)) msg <- as.character(result)

    if (grepl("API rate limit exceeded", msg, ignore.case = TRUE)) {
      return("rate limit exceeded")
    } else if (grepl("Connection timed out", msg, ignore.case = TRUE)) {
      return("timeout")
    } else if (grepl("Network connection failed", msg, ignore.case = TRUE)) {
      return("network error")
    } else if (grepl("HTTP 403 Forbidden", msg, ignore.case = TRUE)) {
      return("permission denied")
    } else if (grepl("HTTP 404 not found", msg, ignore.case = TRUE)) {
      return("not found")
    } else if (grepl("HTTP 500 server error", msg, ignore.case = TRUE)) {
      return("server error")
    } else {
      return("unknown error")
    }
  }

  "unknown result type"
}

# ========================
# Retry Wrapper
# ========================

.pb_retry_with_backoff <- function(fn,
                                   max_attempts    = 3,
                                   initial_delay   = 2,
                                   max_delay       = 60,
                                   backoff_factor  = 2,
                                   max_total_time  = Inf,
                                   operation_name  = "operation",
                                   output_level    = "std",
                                   log_file        = NULL,
                                   check_success   = function(x) !.pb_tbl_redo_check(x),
                                   on_retry        = NULL,
                                   error_classifier = .pb_error_classify) {
  .retry_with_backoff(
    fn = fn,
    max_attempts = max_attempts,
    initial_delay = initial_delay,
    max_delay = max_delay,
    backoff_factor = backoff_factor,
    max_total_time = max_total_time,
    operation_name = operation_name,
    output_level = output_level,
    log_file = log_file,
    check_success = check_success,
    on_retry = on_retry,
    error_classifier = error_classifier
  )
}
