.pb_release_tbl_get <- function(pause_second = 3,
                                output_level = "std",
                                log_file = NULL) {
  .dep_install("piggyback")
  
  .cli_debug(
    "Piggyback: Getting release table from {.pb_repo_get()}",
    output_level = output_level,
    log_file = log_file
  )
  
  gh_tbl_release <- .pb_release_tbl_get_attempt(
    output_level = output_level,
    log_file = log_file
  )
  if (!.pb_tbl_redo_check(gh_tbl_release)) {
    .cli_debug(
      "Piggyback: Successfully retrieved release table ({nrow(gh_tbl_release)} release(s))",
      output_level = output_level,
      log_file = log_file
    )
    return(gh_tbl_release)
  }
  
  .cli_debug(
    "Piggyback: First attempt failed, clearing cache and retrying...",
    output_level = output_level,
    log_file = log_file
  )
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_release <- .pb_release_tbl_get_attempt(
    output_level = output_level,
    log_file = log_file
  )
  if (!.pb_tbl_redo_check(gh_tbl_release)) {
    .cli_debug(
      "Piggyback: Successfully retrieved release table on retry ({nrow(gh_tbl_release)} release(s))",
      output_level = output_level,
      log_file = log_file
    )
    return(gh_tbl_release)
  }
  
  .cli_debug(
    "Piggyback: Second attempt failed, clearing cache and trying one more time...",
    output_level = output_level,
    log_file = log_file
  )
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  result <- .pb_release_tbl_get_attempt(
    output_level = output_level,
    log_file = log_file
  )
  
  if (.pb_tbl_redo_check(result)) {
    .cli_debug(
      "Piggyback: All attempts to get release table failed",
      output_level = output_level,
      log_file = log_file
    )
  } else {
    .cli_debug(
      "Piggyback: Successfully retrieved release table on final retry ({nrow(result)} release(s))",
      output_level = output_level,
      log_file = log_file
    )
  }
  
  result
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
  .dep_install("piggyback")
  
  .cli_debug(
    "Piggyback: Getting asset list for tag '{tag}' from {.pb_repo_get()}",
    output_level = output_level,
    log_file = log_file
  )
  
  gh_tbl_asset <- .pb_asset_tbl_get_attempt(
    tag = tag,
    output_level = output_level,
    log_file = log_file
  )
  if (!.pb_tbl_redo_check(gh_tbl_asset)) {
    result <- .pb_asset_tbl_normalize(gh_tbl_asset)
    .cli_debug(
      "Piggyback: Successfully retrieved {nrow(result)} asset(s) for tag '{tag}'",
      output_level = output_level,
      log_file = log_file
    )
    return(result)
  }
  
  .cli_debug(
    "Piggyback: First attempt failed, clearing cache and retrying...",
    output_level = output_level,
    log_file = log_file
  )
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  gh_tbl_asset <- .pb_asset_tbl_get_attempt(
    tag = tag,
    output_level = output_level,
    log_file = log_file
  )
  if (!.pb_tbl_redo_check(gh_tbl_asset)) {
    result <- .pb_asset_tbl_normalize(gh_tbl_asset)
    .cli_debug(
      "Piggyback: Successfully retrieved {nrow(result)} asset(s) for tag '{tag}' on retry",
      output_level = output_level,
      log_file = log_file
    )
    return(result)
  }
  
  .cli_debug(
    "Piggyback: Second attempt failed, clearing cache and trying one more time...",
    output_level = output_level,
    log_file = log_file
  )
  piggyback::.pb_cache_clear()
  Sys.sleep(pause_second)
  result <- .pb_asset_tbl_normalize(.pb_asset_tbl_get_attempt(
    tag = tag,
    output_level = output_level,
    log_file = log_file
  ))
  
  if (nrow(result) == 0 && !inherits(gh_tbl_asset, "try-error")) {
    .cli_debug(
      "Piggyback: Retrieved empty asset list for tag '{tag}'",
      output_level = output_level,
      log_file = log_file
    )
  } else if (nrow(result) > 0) {
    .cli_debug(
      "Piggyback: Successfully retrieved {nrow(result)} asset(s) for tag '{tag}' on final retry",
      output_level = output_level,
      log_file = log_file
    )
  } else {
    .cli_debug(
      "Piggyback: All attempts to get assets for tag '{tag}' failed",
      output_level = output_level,
      log_file = log_file
    )
  }
  
  result
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
