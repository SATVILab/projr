# ========================
# GitHub release preparation - create and verify all required releases
# ========================

.gh_release_prepare_tags <- function(tags,
                                     strict = TRUE,
                                     output_level = "std",
                                     log_file = NULL,
                                     create_rounds = 3,
                                     long_wait_seconds = 600,
                                     initial_delay = 2) {
  # Early exit if no tags
  if (.is_len_0(tags)) {
    .cli_debug(
      "GitHub release preparation: No tags to prepare",
      output_level = output_level,
      log_file = log_file
    )
    return(invisible(TRUE))
  }

  # Ensure GitHub authentication
  .auth_check_github("preparing GitHub releases")

  # Get current repository
  repo <- .pb_repo_get()

  # Deduplicate tags
  tags <- unique(tags)

  .cli_debug(
    "GitHub release preparation: Preparing {length(tags)} release(s) for {repo}: {paste(tags, collapse = ', ')}",
    output_level = output_level,
    log_file = log_file
  )

  # Helper to get existing tags
  get_existing_tags <- function() {
    release_tbl <- .pb_release_tbl_get_attempt(
      output_level = output_level,
      log_file = log_file
    )

    if (inherits(release_tbl, "try-error") || is.null(release_tbl)) {
      return(character(0L))
    }

    if (nrow(release_tbl) == 0L) {
      return(character(0L))
    }

    release_tbl[["tag_name"]]
  }

  # Initial check for existing tags
  existing <- get_existing_tags()
  remaining <- setdiff(tags, existing)

  .cli_debug(
    "GitHub release preparation: {length(existing)} of {length(tags)} release(s) already exist",
    output_level = output_level,
    log_file = log_file
  )

  # Create missing releases (with retry rounds)
  for (round in seq_len(create_rounds)) {
    if (.is_len_0(remaining)) {
      break
    }

    .cli_debug(
      "GitHub release preparation: Round {round}/{create_rounds} - Creating {length(remaining)} missing release(s): {paste(remaining, collapse = ', ')}",
      output_level = output_level,
      log_file = log_file
    )

    for (tag in remaining) {
      .cli_debug(
        "GitHub release preparation: Creating release '{tag}'",
        output_level = output_level,
        log_file = log_file
      )

      .remote_create_github(
        tag = tag,
        description = "Release created automatically by `projr`",
        output_level = output_level,
        log_file = log_file
      )

      # Pause between creates to avoid hammering the API
      Sys.sleep(1)
    }

    # Re-check what exists after creating
    piggyback::.pb_cache_clear()
    existing <- get_existing_tags()
    remaining <- setdiff(tags, existing)

    if (.is_len_0(remaining)) {
      .cli_debug(
        "GitHub release preparation: All releases verified after round {round}",
        output_level = output_level,
        log_file = log_file
      )
      break
    }
  }

  # If still missing after create rounds, enter long wait loop
  if (!.is_len_0(remaining)) {
    .cli_debug(
      "GitHub release preparation: {length(remaining)} release(s) still missing after {create_rounds} rounds, entering verification-only wait loop",
      output_level = output_level,
      log_file = log_file
    )

    delay <- initial_delay
    start_time <- Sys.time()
    max_delay <- 60

    while (!.is_len_0(remaining)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (elapsed >= long_wait_seconds) {
        .cli_debug(
          "GitHub release preparation: Timeout after {round(elapsed, 1)} seconds",
          output_level = output_level,
          log_file = log_file
        )
        break
      }

      # Log wait message at "std" level if delay >= 5 seconds
      if (delay >= 5) {
        .cli_info(
          "GitHub release preparation: Waiting {delay}s for {length(remaining)} release(s) to become visible: {paste(remaining, collapse = ', ')}",
          output_level = "std",
          log_file = log_file
        )
      } else {
        .cli_debug(
          "GitHub release preparation: Waiting {delay}s before next verification attempt",
          output_level = output_level,
          log_file = log_file
        )
      }

      Sys.sleep(delay)

      # Clear cache and check again
      piggyback::.pb_cache_clear()
      existing <- get_existing_tags()
      remaining <- setdiff(tags, existing)

      # Exponential backoff
      delay <- min(delay * 2, max_delay)
    }
  }

  # Final result
  if (.is_len_0(remaining)) {
    .cli_debug(
      "GitHub release preparation: All {length(tags)} release(s) verified successfully",
      output_level = output_level,
      log_file = log_file
    )
    return(invisible(TRUE))
  }

  # Some releases still missing
  msg <- .gh_release_error_message(repo, remaining)

  if (strict) {
    stop(msg, call. = FALSE)
  } else {
    .cli_info(
      msg,
      output_level = "std",
      log_file = log_file
    )
    return(invisible(FALSE))
  }
}

# Generate informative error message when releases cannot be detected
.gh_release_error_message <- function(repo, missing_tags) {
  paste0(
    "Failed to confirm GitHub releases for this build.\n\n",
    "Repository: ", repo, "\n",
    "Missing tags: ", paste(missing_tags, collapse = ", "), "\n\n",
    "projr attempted to create these releases via piggyback and waited for them to become visible, ",
    "but they did not appear within the configured time window.\n\n",
    "Likely causes:\n",
    "  - GitHub API issues (partial outage, propagation delays)\n",
    "  - Authentication or permission problems (insufficient token scopes)\n",
    "  - Repository-level restrictions or rate limiting\n",
    "  - Caching or consistency issues in piggyback or the GitHub API\n\n",
    "Suggested next steps:\n",
    "  1. Rerun the build - temporary API issues may resolve\n",
    "  2. Check the GitHub repository's Releases page to see if releases were partially created\n",
    "  3. Enable PROJR_OUTPUT_LEVEL=\"debug\" for more detailed logs\n",
    "  4. Verify GITHUB_PAT has sufficient permissions (repo scope required)\n"
  )
}

# Helper function for retry logic with exponential backoff

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

  # Validate that we got valid owner and repo names
  owner <- gh_repo[[1]]
  repo <- gh_repo[[2]]

  if (is.null(owner) || !nzchar(owner)) {
    stop(
      "Failed to get GitHub repository owner from git remote. ",
      "Please ensure the repository has a valid GitHub remote configured."
    )
  }

  if (is.null(repo) || !nzchar(repo)) {
    stop(
      "Failed to get GitHub repository name from git remote. ",
      "Please ensure the repository has a valid GitHub remote configured."
    )
  }

  paste0(owner, "/", repo)
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
    max_attempts = 3,
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

# Helper function for retry logic with exponential backoff
.pb_retry_with_backoff <- function(fn,
                                   max_attempts = 3,
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
    max_attempts = 3,
    initial_delay = pause_second,
    operation_name = "get release table",
    output_level = output_level,
    log_file = log_file,
    check_success = function(x) !.pb_tbl_redo_check(x)
  )
}
