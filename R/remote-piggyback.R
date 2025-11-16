# Helper function for retry logic with exponential backoff
.pb_retry_with_backoff <- function(fn,
                                   max_attempts = 6,
                                   initial_delay = 2,
                                   max_delay = 60,
                                   backoff_factor = 2,
                                   operation_name = "operation",
                                   output_level = "std",
                                   log_file = NULL,
                                   check_success = function(x) !.pb_tbl_redo_check(x)) {
  .dep_install("piggyback")
  
  .cli_debug(
    "Piggyback: Starting {operation_name} (max {max_attempts} attempts)",
    output_level = output_level,
    log_file = log_file
  )
  
  delay <- initial_delay
  
  for (attempt in seq_len(max_attempts)) {
    # Clear cache before each attempt (except first)
    if (attempt > 1) {
      piggyback::.pb_cache_clear()
      
      # Apply delay with exponential backoff
      actual_delay <- min(delay, max_delay)
      .cli_debug(
        "Piggyback: Waiting {actual_delay}s before attempt {attempt}/{max_attempts}...",
        output_level = output_level,
        log_file = log_file
      )
      Sys.sleep(actual_delay)
      delay <- delay * backoff_factor
    }
    
    # Execute the operation
    result <- fn()
    
    # Check if successful
    if (check_success(result)) {
      .cli_debug(
        "Piggyback: {operation_name} succeeded on attempt {attempt}/{max_attempts}",
        output_level = output_level,
        log_file = log_file
      )
      return(result)
    }
    
    # Log the failure with error details
    error_type <- .pb_error_classify(result)
    .cli_debug(
      "Piggyback: Attempt {attempt}/{max_attempts} failed ({error_type})",
      output_level = output_level,
      log_file = log_file
    )
  }
  
  # All attempts exhausted
  .cli_debug(
    "Piggyback: All {max_attempts} attempts for {operation_name} failed",
    output_level = output_level,
    log_file = log_file
  )
  
  result
}

# Classify error type for better debugging
.pb_error_classify <- function(result) {
  if (is.null(result)) {
    return("null result")
  }
  if (!inherits(result, "try-error")) {
    if (is.data.frame(result) && nrow(result) == 0L) {
      return("empty result")
    }
    return("unknown")
  }
  
  error_msg <- attr(result, "condition")$message
  
  # Check for common error patterns
  if (grepl("429|rate.?limit", error_msg, ignore.case = TRUE)) {
    return("rate limit exceeded")
  }
  if (grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
    return("timeout")
  }
  if (grepl("connection|network|socket", error_msg, ignore.case = TRUE)) {
    return("network error")
  }
  if (grepl("403|forbidden", error_msg, ignore.case = TRUE)) {
    return("permission denied")
  }
  if (grepl("404|not found", error_msg, ignore.case = TRUE)) {
    return("not found")
  }
  if (grepl("500|502|503|504|server error", error_msg, ignore.case = TRUE)) {
    return("server error")
  }
  
  "unknown error"
}

.pb_release_tbl_get <- function(pause_second = 3,
                                output_level = "std",
                                log_file = NULL) {
  .pb_retry_with_backoff(
    fn = function() {
      .pb_release_tbl_get_attempt(
        output_level = output_level,
        log_file = log_file
      )
    },
    max_attempts = 6,
    initial_delay = pause_second,
    operation_name = "get release table",
    output_level = output_level,
    log_file = log_file,
    check_success = function(x) !.pb_tbl_redo_check(x)
  )
}

.pb_repo_get <- function() {
  if (.git_repo_is_worktree()) {
    git_file <- readLines(.path_get(".git"), warn = FALSE)
    git_file <- gsub("^gitdir: ", "", git_file)
    if (!file.exists(git_file)) {
      stop("Cannot find gitdir: ", git_file)
    }
    .pb_guess_repo(git_file)
  } else {
    .pb_guess_repo()
  }
}

.pb_guess_repo <- function(path = ".") {
  .auth_check_github("accessing GitHub repository information")
  gh_repo <- gh::gh_tree_remote(path)
  paste0(gh_repo[[1]], "/", gh_repo[[2]])
}

.pb_release_tbl_get_attempt <- function(output_level = "std",
                                        log_file = NULL) {
  repo <- .pb_repo_get()
  result <- try(suppressWarnings(suppressMessages(
    piggyback::pb_releases(repo = repo) #nolint
  )), silent = TRUE) #nolint
  
  if (inherits(result, "try-error")) {
    error_msg <- attr(result, "condition")$message
    .cli_debug(
      "Piggyback: pb_releases() failed for {repo}: {error_msg}",
      output_level = output_level,
      log_file = log_file
    )
  }
  
  result
}

.pb_asset_tbl_get <- function(tag,
                              pause_second = 3,
                              output_level = "std",
                              log_file = NULL) {
  result <- .pb_retry_with_backoff(
    fn = function() {
      .pb_asset_tbl_get_attempt(
        tag = tag,
        output_level = output_level,
        log_file = log_file
      )
    },
    max_attempts = 6,
    initial_delay = pause_second,
    operation_name = paste0("get asset list for tag '", tag, "'"),
    output_level = output_level,
    log_file = log_file,
    check_success = function(x) !.pb_tbl_redo_check(x)
  )
  
  # Normalize the result
  .pb_asset_tbl_normalize(result)
}

.pb_tbl_redo_check <- function(tbl) {
  if (is.null(tbl)) {
    return(TRUE)
  }
  error_lgl <- inherits(tbl, "try-error")
  zero_row_lgl <- nrow(tbl) == 0L
  error_lgl || zero_row_lgl
}

.pb_asset_tbl_normalize <- function(tbl) {
  # Ensure the result is always a data frame with file_name column
  if (is.null(tbl) || inherits(tbl, "try-error")) {
    return(data.frame(
      file_name = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # If the table exists but doesn't have file_name column, add it
  if (!("file_name" %in% names(tbl))) {
    tbl[["file_name"]] <- character(0)
  }
  
  tbl
}

.pb_asset_tbl_get_attempt <- function(tag,
                                      output_level = "std",
                                      log_file = NULL) {
  repo <- .pb_repo_get()
  result <- try(suppressWarnings(suppressMessages(piggyback::pb_list(
    repo = repo, tag = tag
  ))), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    error_msg <- attr(result, "condition")$message
    .cli_debug(
      "Piggyback: pb_list() failed for tag '{tag}' in {repo}: {error_msg}",
      output_level = output_level,
      log_file = log_file
    )
  }
  
  result
}

.pb_tag_format <- function(tag) {
  tag <- switch(tag,
    `@version` = .version_get_v(),
    tag
  )
  tag <- gsub("^ +", "", tag)
  tag <- gsub(" +$", "", tag)
  gsub(" ", "-", tag)
}
