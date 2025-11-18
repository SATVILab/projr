# ========================
# Per-build GitHub release state management
# ========================

# Package-level environment for per-build GitHub state
.projr_state <- new.env(parent = emptyenv())

# Initialize GitHub release state for a build
.gh_release_state_init <- function() {
  .projr_state$gh_releases_required <- character(0)
  .projr_state$gh_releases_verified <- character(0)
  .projr_state$gh_release_tbl <- NULL
  invisible(TRUE)
}

# Clear GitHub release state after a build
.gh_release_state_clear <- function() {
  .projr_state$gh_releases_required <- NULL
  .projr_state$gh_releases_verified <- NULL
  .projr_state$gh_release_tbl <- NULL
  invisible(TRUE)
}

# Register a required GitHub release tag
.gh_release_register_required <- function(tag, output_level = "std", log_file = NULL) {
  .assert_string(tag, required = TRUE)

  # Format the tag consistently
  tag <- .pb_tag_format(tag)

  .cli_debug(
    "GitHub release: Registering required tag '{tag}'",
    output_level = output_level,
    log_file = log_file
  )

  # Get current repo
  repo <- .pb_repo_get()
  tag_full <- paste0(repo, "::", tag)

  # Add to required set if not already present
  if (!tag_full %in% .projr_state$gh_releases_required) {
    .projr_state$gh_releases_required <- c(
      .projr_state$gh_releases_required,
      tag_full
    )
  }

  invisible(TRUE)
}

# Check if a tag is verified
.gh_release_is_verified <- function(tag) {
  .assert_string(tag, required = TRUE)
  repo <- .pb_repo_get()
  tag <- .pb_tag_format(tag)
  tag_full <- paste0(repo, "::", tag)
  tag_full %in% .projr_state$gh_releases_verified
}

# Mark a tag as verified
.gh_release_mark_verified <- function(tag) {
  .assert_string(tag, required = TRUE)
  repo <- .pb_repo_get()
  tag <- .pb_tag_format(tag)
  tag_full <- paste0(repo, "::", tag)
  if (!tag_full %in% .projr_state$gh_releases_verified) {
    .projr_state$gh_releases_verified <- c(
      .projr_state$gh_releases_verified,
      tag_full
    )
  }
  invisible(TRUE)
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

# ========================
# GitHub release preparation - create and verify all required releases
# ========================

.gh_release_prepare_all <- function(output_level = "std", log_file = NULL) {
  # Check if we have any required releases
  if (length(.projr_state$gh_releases_required) == 0) {
    .cli_debug(
      "GitHub release preparation: No releases required",
      output_level = output_level,
      log_file = log_file
    )
    return(invisible(TRUE))
  }

  # Ensure GitHub authentication
  .auth_check_github("preparing GitHub releases")

  # Get current repository
  repo <- .pb_repo_get()

  # Extract unique tags from repo::tag format
  tag_full_vec <- unique(.projr_state$gh_releases_required)
  tag_vec <- vapply(
    strsplit(tag_full_vec, "::"),
    function(x) x[2],
    character(1)
  )

  .cli_debug(
    "GitHub release preparation: Preparing {length(tag_vec)} release(s) for {repo}: {paste(tag_vec, collapse = ', ')}",
    output_level = output_level,
    log_file = log_file
  )

  # Fetch current releases table and cache it
  .cli_debug(
    "GitHub release preparation: Fetching current releases",
    output_level = output_level,
    log_file = log_file
  )

  release_tbl <- .pb_release_tbl_get(
    output_level = output_level,
    log_file = log_file
  )

  if (inherits(release_tbl, "try-error")) {
    stop(paste0(
      "Failed to fetch GitHub releases for repository '", repo, "'. ",
      "This is required to prepare releases for the build. ",
      "Please check:\n",
      "  - GitHub API is accessible\n",
      "  - GITHUB_PAT environment variable is set with correct permissions\n",
      "  - Repository exists and you have access to it\n",
      "Enable PROJR_OUTPUT_LEVEL=\"debug\" for more details."
    ))
  }

  .projr_state$gh_release_tbl <- release_tbl

  # Determine which tags need to be created
  existing_tags <- if (nrow(release_tbl) > 0) {
    release_tbl[["tag_name"]]
  } else {
    character(0)
  }

  tags_to_create <- setdiff(tag_vec, existing_tags)

  # Create missing releases
  if (length(tags_to_create) > 0) {
    .cli_debug(
      "GitHub release preparation: Creating {length(tags_to_create)} missing release(s): {paste(tags_to_create, collapse = ', ')}",
      output_level = output_level,
      log_file = log_file
    )

    for (tag in tags_to_create) {
      .remote_create_github(
        tag = tag,
        description = "Release created automatically by `projr`",
        output_level = output_level,
        log_file = log_file
      )
    }
  } else {
    .cli_debug(
      "GitHub release preparation: All required releases already exist",
      output_level = output_level,
      log_file = log_file
    )
  }

  # Verify all required tags are now present
  .gh_release_verify_all(
    tag_vec = tag_vec,
    repo = repo,
    output_level = output_level,
    log_file = log_file
  )

  invisible(TRUE)
}

# Verify all required tags exist with retry/backoff
.gh_release_verify_all <- function(tag_vec, repo, output_level = "std", log_file = NULL) {
  max_attempts <- 6
  initial_delay <- 2

  .cli_debug(
    "GitHub release verification: Starting verification for {length(tag_vec)} tag(s)",
    output_level = output_level,
    log_file = log_file
  )

  result <- .pb_retry_with_backoff(
    fn = function() {
      # Re-fetch releases
      release_tbl <- .pb_release_tbl_get_attempt(
        output_level = output_level,
        log_file = log_file
      )

      if (inherits(release_tbl, "try-error") || nrow(release_tbl) == 0) {
        return(release_tbl)
      }

      # Update cached table
      .projr_state$gh_release_tbl <- release_tbl

      # Check if all required tags are present
      existing_tags <- release_tbl[["tag_name"]]
      missing_tags <- setdiff(tag_vec, existing_tags)

      if (length(missing_tags) == 0) {
        # All tags present - mark as verified
        for (tag in tag_vec) {
          .gh_release_mark_verified(tag)
        }
        return(TRUE)
      }

      # Return error structure to trigger retry
      structure(
        list(missing_tags = missing_tags),
        class = "try-error",
        condition = list(message = paste0(
          "Missing tags: ", paste(missing_tags, collapse = ", ")
        ))
      )
    },
    max_attempts = max_attempts,
    initial_delay = initial_delay,
    operation_name = "verify all required GitHub releases",
    output_level = output_level,
    log_file = log_file,
    check_success = function(x) isTRUE(x)
  )

  # Check if verification succeeded
  if (!isTRUE(result)) {
    # Extract missing tags
    missing_tags <- if (inherits(result, "try-error") && !is.null(result$missing_tags)) {
      result$missing_tags
    } else {
      tag_vec
    }

    stop(.gh_release_error_message(repo, missing_tags))
  }

  .cli_debug(
    "GitHub release verification: All {length(tag_vec)} release(s) verified successfully",
    output_level = output_level,
    log_file = log_file
  )

  invisible(TRUE)
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
