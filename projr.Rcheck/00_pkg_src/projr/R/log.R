# Build Logging System
# =====================
#
# This module provides persistent logging for build runs.
# Logs are stored in cache/projr/log/ which is never cleared automatically.
#
# Directory structure:
# cache/projr/log/
#   +-- output/
#   |   +-- history/
#   |   |   +-- builds.md (all build records, newest first)
#   |   +-- output/
#   |       +-- YYYY-MMM-DD/
#   |           +-- HH-MM-SS.qmd (detailed log for each build)
#   +-- dev/
#       +-- history/
#       |   +-- builds.md
#       +-- output/
#           +-- YYYY-MMM-DD/
#               +-- HH-MM-SS.qmd

# Get log directory paths
# -----------------------

#' Get the base log directory path
#'
#' @return Character. Path to the log directory.
#' @keywords internal
.log_dir_get_base <- function() {
  # Get cache directory and append log subdirectory
  cache_dir <- projr_path_get("cache", "projr", create = TRUE)
  file.path(cache_dir, "log")
}

#' Get log directory for build type
#'
#' @param build_type Character. Either "output" or "dev".
#' @param create Logical. Create directory if it doesn't exist.
#'
#' @return Character. Path to the build type log directory.
#' @keywords internal
.log_dir_get_type <- function(build_type = "output", create = TRUE) {
  .assert_in(build_type, c("output", "dev"))
  path <- file.path(.log_dir_get_base(), build_type)
  if (create && !dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  path
}

#' Get history log file path
#'
#' @param build_type Character. Either "output" or "dev".
#'
#' @return Character. Path to the history markdown file.
#' @keywords internal
.log_file_get_history <- function(build_type = "output") {
  type_dir <- .log_dir_get_type(build_type, create = TRUE)
  history_dir <- file.path(type_dir, "history")
  if (!dir.exists(history_dir)) {
    dir.create(history_dir, recursive = TRUE)
  }
  file.path(history_dir, "builds.md")
}

#' Get output log directory for a specific date
#'
#' @param build_type Character. Either "output" or "dev".
#' @param date Character. Date in YYYY-MMM-DD format. Default is today.
#'
#' @return Character. Path to the date-specific output directory.
#' @keywords internal
.log_dir_get_output_date <- function(build_type = "output", date = NULL) {
  if (is.null(date)) {
    date <- format(Sys.Date(), "%Y-%b-%d")
  }
  type_dir <- .log_dir_get_type(build_type, create = TRUE)
  output_dir <- file.path(type_dir, "output", date)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir
}

#' Get output log file path for current build
#'
#' @param build_type Character. Either "output" or "dev".
#' @param timestamp Character. Timestamp in HH-MM-SS format. Default is now.
#'
#' @return Character. Path to the log file for this build.
#' @keywords internal
.log_file_get_output <- function(build_type = "output", timestamp = NULL) {
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%H-%M-%S")
  }
  date_dir <- .log_dir_get_output_date(build_type)
  file.path(date_dir, paste0(timestamp, ".qmd"))
}

# Log writing functions
# ---------------------

#' Check if detailed logging is enabled
#'
#' @return Logical. TRUE if detailed logging should be written.
#' @keywords internal
.log_enabled <- function() {
  # Check environment variable (default is TRUE)
  env_val <- Sys.getenv("PROJR_LOG_DETAILED", unset = "TRUE")
  toupper(env_val) %in% c("TRUE", "1", "YES", "Y")
}

#' Initialize a build log file
#'
#' @param build_type Character. Either "output" or "dev".
#' @param bump_component Character. Version bump component.
#' @param msg Character. Build message.
#' @param output_level Character. Output level for this build.
#'
#' @return List with log_file path and timestamp, or NULL if logging disabled.
#' @keywords internal
.log_build_init <- function(build_type = "output", bump_component = NULL,
                            msg = "", output_level = "std") {
  # Check if detailed logging is enabled
  if (!.log_enabled()) {
    return(NULL)
  }

  timestamp <- format(Sys.time(), "%H-%M-%S")
  log_file <- .log_file_get_output(build_type, timestamp)

  # Create header for the log file
  header <- c(
    "---",
    paste0("title: \"", toupper(build_type), " Build Log\""),
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "format: html",
    "---",
    "",
    "# Build Information",
    "",
    paste0("- **Build Type**: ", build_type),
    paste0("- **Timestamp**: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    if (!is.null(bump_component)) paste0("- **Version Bump**: ", bump_component) else NULL,
    if (nzchar(msg)) paste0("- **Message**: ", msg) else NULL,
    paste0("- **Output Level**: ", output_level),
    paste0("- **projr Version**: ", projr_version_get()),
    "",
    "# Build Output",
    ""
  )

  writeLines(header, log_file)

  list(log_file = log_file, timestamp = timestamp)
}

#' Append message to build log
#'
#' @param log_file Character. Path to log file.
#' @param message Character. Message to append.
#' @param level Character. Message level (info, debug, success, etc.)
#'
#' @keywords internal
.log_build_append <- function(log_file, message, level = "info") {
  if (is.null(log_file) || !file.exists(log_file)) {
    return(invisible(NULL))
  }

  # Format message with timestamp and level
  timestamp <- format(Sys.time(), "%H:%M:%S")
  formatted <- paste0("[", timestamp, "] **", toupper(level), "**: ", message)

  # Append to file
  cat(formatted, "\n", file = log_file, append = TRUE)
  invisible(NULL)
}

#' Append section header to build log
#'
#' @param log_file Character. Path to log file.
#' @param section_name Character. Name of the section.
#'
#' @keywords internal
.log_build_section <- function(log_file, section_name) {
  if (is.null(log_file) || !file.exists(log_file)) {
    return(invisible(NULL))
  }

  header <- paste0("\n## ", section_name, "\n")
  cat(header, file = log_file, append = TRUE)
  invisible(NULL)
}

#' Finalize build log with summary
#'
#' @param log_file Character. Path to log file.
#' @param success Logical. Whether build succeeded.
#' @param start_time POSIXct. Build start time.
#'
#' @keywords internal
.log_build_finalize <- function(log_file, success = TRUE, start_time = NULL) {
  if (is.null(log_file) || !file.exists(log_file)) {
    return(invisible(NULL))
  }

  footer <- c(
    "",
    "---",
    "",
    "# Build Summary",
    "",
    paste0("- **Status**: ", if (success) "[OK] SUCCESS" else "[X] FAILED"),
    if (!is.null(start_time)) {
      duration <- difftime(Sys.time(), start_time, units = "secs")
      paste0("- **Duration**: ", round(as.numeric(duration), 2), " seconds")
    } else NULL,
    paste0("- **Completed**: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )

  cat(paste(footer, collapse = "\n"), file = log_file, append = TRUE)
  invisible(NULL)
}

#' Update build history file
#'
#' @param build_type Character. Either "output" or "dev".
#' @param bump_component Character. Version bump component.
#' @param msg Character. Build message.
#' @param success Logical. Whether build succeeded.
#' @param log_file Character. Path to detailed log file.
#'
#' @keywords internal
.log_history_add <- function(build_type = "output", bump_component = NULL,
                             msg = "", success = TRUE, log_file = NULL) {
  history_file <- .log_file_get_history(build_type)

  # Read existing history or create header
  if (file.exists(history_file)) {
    existing <- readLines(history_file, warn = FALSE)
  } else {
    existing <- c(
      "# Build History",
      "",
      paste0("History of all ", build_type, " builds, newest first."),
      ""
    )
  }

  # Create new entry
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  status <- if (success) "[OK]" else "[X]"

  entry <- c(
    paste0("## ", timestamp, " ", status),
    "",
    paste0("- **Type**: ", build_type),
    if (!is.null(bump_component)) paste0("- **Bump**: ", bump_component) else NULL,
    if (nzchar(msg)) paste0("- **Message**: ", msg) else NULL,
    paste0("- **Version**: ", projr_version_get()),
    if (!is.null(log_file)) paste0("- **Log**: `", basename(dirname(log_file)), "/", basename(log_file), "`") else NULL,
    ""
  )

  # Insert at position after header (line 4)
  updated <- c(existing[1:4], entry, existing[5:length(existing)])

  writeLines(updated, history_file)
  invisible(NULL)
}

# Log clearing functions
# ----------------------

#' Clear build logs
#'
#' @description
#' Delete build logs based on specified criteria.
#'
#' @param build_type Character. Either "output", "dev", or "all".
#'   Default is "all".
#' @param history Logical. Clear history files. Default is TRUE.
#' @param output Logical. Clear output log files. Default is TRUE.
#' @param before_date Character. Clear logs before this date (YYYY-MM-DD).
#'   Default is NULL (no date filter).
#' @param before_version Character. Clear logs before this version.
#'   Default is NULL (no version filter).
#'
#' @export
projr_log_clear <- function(build_type = "all",
                            history = TRUE,
                            output = TRUE,
                            before_date = NULL,
                            before_version = NULL) {
  # Validate inputs
  .assert_in(build_type, c("all", "output", "dev"))
  .assert_lgl(history)
  .assert_lgl(output)

  # Determine which build types to clear
  types <- if (build_type == "all") c("output", "dev") else build_type

  for (type in types) {
    type_dir <- .log_dir_get_type(type, create = FALSE)
    if (!dir.exists(type_dir)) {
      next
    }

    # Clear history
    if (history) {
      .log_clear_history(type, before_date, before_version)
    }

    # Clear output logs
    if (output) {
      .log_clear_output(type, before_date, before_version)
    }
  }

  cli::cli_alert_success("Logs cleared successfully")
  invisible(TRUE)
}

#' Clear history file with filters
#'
#' @keywords internal
.log_clear_history <- function(build_type, before_date = NULL, before_version = NULL) {
  history_file <- .log_file_get_history(build_type)

  if (!file.exists(history_file)) {
    return(invisible(NULL))
  }

  # If no filters, just delete the file
  if (is.null(before_date) && is.null(before_version)) {
    unlink(history_file)
    return(invisible(NULL))
  }

  # TODO: Implement filtered deletion based on date/version
  # For now, if filters are specified, we don't delete
  cli::cli_alert_info("Filtered history clearing not yet implemented")
  invisible(NULL)
}

#' Clear output logs with filters
#'
#' @keywords internal
.log_clear_output <- function(build_type, before_date = NULL, before_version = NULL) {
  type_dir <- .log_dir_get_type(build_type, create = FALSE)
  output_dir <- file.path(type_dir, "output")

  if (!dir.exists(output_dir)) {
    return(invisible(NULL))
  }

  # If no filters, delete entire output directory
  if (is.null(before_date) && is.null(before_version)) {
    unlink(output_dir, recursive = TRUE)
    return(invisible(NULL))
  }

  # Filter by date if specified
  if (!is.null(before_date)) {
    .log_clear_output_by_date(output_dir, before_date)
  }

  # TODO: Implement version-based filtering
  if (!is.null(before_version)) {
    cli::cli_alert_info("Version-based log clearing not yet implemented")
  }

  invisible(NULL)
}

#' Clear output logs before a specific date
#'
#' @keywords internal
.log_clear_output_by_date <- function(output_dir, before_date) {
  # Parse the before_date
  cutoff <- as.Date(before_date)

  # List all date directories
  date_dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)

  for (dir in date_dirs) {
    dir_name <- basename(dir)
    # Try to parse directory name as date (format: YYYY-MMM-DD)
    dir_date <- tryCatch(
      as.Date(dir_name, format = "%Y-%b-%d"),
      error = function(e) NULL
    )

    if (!is.null(dir_date) && dir_date <= cutoff) {
      unlink(dir, recursive = TRUE)
    }
  }

  invisible(NULL)
}
