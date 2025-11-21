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
#' @param message Character. Message to append.
#' @param level Character. Message level (info, debug, success, etc.)
#'
#' @keywords internal
.log_build_append <- function(message, level = "info") {
  log_file <- .log_file_get_most_recent()
  if (is.null(log_file)) {
    return(invisible(NULL))
  }

  # Format message with timestamp and level
  timestamp <- format(Sys.time(), "%H:%M:%S")
  formatted <- paste0(
    "[", timestamp, "] **", toupper(level), "**: ", message
  )

  # Append to file
  cat(
    formatted, "\n",
    file = log_file, append = TRUE
  )
  invisible(NULL)
}

#' Append section header to build log
#'
#' @param section_name Character. Name of the section.
#'
#' @keywords internal
.log_build_section <- function(section_name) {
  log_file <- .log_file_get_most_recent()
  if (is.null(log_file)) {
    return(invisible(NULL))
  }
  header <- paste0("\n## ", section_name, "\n")
  cat(header, file = log_file, append = TRUE)
  invisible(NULL)
}

#' Finalize build log with summary
#'
#' @param success Logical. Whether build succeeded.
#' @param start_time POSIXct. Build start time.
#'
#' @keywords internal
.log_build_finalize <- function(success = TRUE, start_time = NULL) {
  log_file <- .log_file_get_most_recent()
  if (is.null(log_file)) {
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

  cat(
    paste(footer, collapse = "\n"),
    file = log_file,
    append = TRUE
  )
  invisible(NULL)
}

#' Update build history file
#'
#' @param build_type Character. Either "output" or "dev".
#' @param bump_component Character. Version bump component.
#' @param msg Character. Build message.
#' @param success Logical. Whether build succeeded.
#'
#' @keywords internal
.log_history_add <- function(build_type = "output", bump_component = NULL,
                             msg = "", success = TRUE) {
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
  log_file <- .log_file_get_most_recent()

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

  # Read and parse history file
  lines <- readLines(history_file, warn = FALSE)
  
  # Find entry boundaries (lines starting with ##)
  entry_starts <- which(grepl("^## ", lines))
  if (length(entry_starts) == 0) {
    return(invisible(NULL))
  }
  
  # Determine which entries to keep
  keep_entries <- logical(length(entry_starts))
  for (i in seq_along(entry_starts)) {
    # Get entry lines
    start_idx <- entry_starts[i]
    end_idx <- if (i < length(entry_starts)) entry_starts[i + 1] - 1 else length(lines)
    entry_lines <- lines[start_idx:end_idx]
    
    # Extract timestamp from header line (## YYYY-MM-DD HH:MM:SS [OK/X])
    header <- entry_lines[1]
    timestamp_match <- regmatches(header, regexpr("\\d{4}-\\d{2}-\\d{2}", header))
    
    # Extract version from entry
    version_line <- entry_lines[grepl("^- \\*\\*Version\\*\\*:", entry_lines)]
    version_match <- if (length(version_line) > 0) {
      sub("^- \\*\\*Version\\*\\*: ", "", version_line[1])
    } else {
      NULL
    }
    
    # Determine if entry should be kept
    keep <- TRUE
    
    # Check date filter
    if (!is.null(before_date) && length(timestamp_match) > 0) {
      entry_date <- as.Date(timestamp_match[1])
      cutoff_date <- as.Date(before_date)
      if (entry_date < cutoff_date) {
        keep <- FALSE
      }
    }
    
    # Check version filter
    if (!is.null(before_version) && !is.null(version_match)) {
      # Compare versions (keep if entry version >= before_version)
      entry_ver_clean <- .version_v_rm(version_match)
      cutoff_ver_clean <- .version_v_rm(before_version)
      if (package_version(entry_ver_clean) < package_version(cutoff_ver_clean)) {
        keep <- FALSE
      }
    }
    
    keep_entries[i] <- keep
  }
  
  # Rebuild history file with kept entries
  if (!any(keep_entries)) {
    # Delete file if no entries remain
    unlink(history_file)
  } else {
    # Keep header (first 4 lines) and selected entries
    header_lines <- lines[1:min(4, length(lines))]
    kept_lines <- character(0)
    
    for (i in which(keep_entries)) {
      start_idx <- entry_starts[i]
      end_idx <- if (i < length(entry_starts)) entry_starts[i + 1] - 1 else length(lines)
      kept_lines <- c(kept_lines, lines[start_idx:end_idx])
    }
    
    writeLines(c(header_lines, kept_lines), history_file)
  }
  
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

  # Filter by version if specified
  if (!is.null(before_version)) {
    .log_clear_output_by_version(output_dir, before_version)
  }

  invisible(NULL)
}

#' Clear output logs before a specific version
#'
#' @keywords internal
.log_clear_output_by_version <- function(output_dir, before_version) {
  # Parse the cutoff version
  cutoff_ver_clean <- .version_v_rm(before_version)
  cutoff_ver <- package_version(cutoff_ver_clean)
  
  # List all date directories
  date_dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  
  for (date_dir in date_dirs) {
    # List all log files in this date directory
    log_files <- list.files(date_dir, pattern = "\\.qmd$", full.names = TRUE)
    
    for (log_file in log_files) {
      # Read log file to extract version
      tryCatch({
        lines <- readLines(log_file, warn = FALSE)
        # Look for projr version in the format: - **projr Version**: X.Y.Z
        version_line <- lines[grepl("^-\\s*\\*\\*projr Version\\*\\*:", lines, ignore.case = TRUE)]
        
        if (length(version_line) > 0) {
          # Extract version from bullet point format
          version_str <- sub("^-\\s*\\*\\*projr Version\\*\\*:\\s*", "", version_line[1], ignore.case = TRUE)
          version_str <- gsub("[\"\']", "", version_str)  # Remove quotes
          version_str <- trimws(version_str)  # Remove whitespace
          
          log_ver_clean <- .version_v_rm(version_str)
          log_ver <- package_version(log_ver_clean)
          
          # Delete if log version is before cutoff
          if (log_ver < cutoff_ver) {
            unlink(log_file)
          }
        }
      }, error = function(e) {
        # Skip files that can't be parsed
      })
    }
    
    # Remove empty date directories
    if (length(list.files(date_dir)) == 0) {
      unlink(date_dir, recursive = TRUE)
    }
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

#' Get the most recent detailed log file path for the current build
#'
#' @param build_type Character. Either "output" or "dev".
#' @param date Character. Optional date (YYYY-MMM-DD) to restrict search to a single
#'   date directory. Default is NULL (search across all date directories).
#' @return Character or NULL. Path to the most recent log file, or NULL if none found.
#' @keywords internal
.log_file_get_current <- function(build_type = "output", date = NULL) {
  .assert_in(build_type, c("output", "dev"))

  type_dir <- .log_dir_get_type(build_type, create = FALSE)
  if (!dir.exists(type_dir)) {
    return(NULL)
  }

  output_dir <- file.path(type_dir, "output")
  if (!dir.exists(output_dir)) {
    return(NULL)
  }

  # If a date is provided, only look in that directory
  date_dirs <- if (!is.null(date)) {
    d <- file.path(output_dir, date)
    if (dir.exists(d)) d else character(0)
  } else {
    list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  }

  if (length(date_dirs) == 0) {
    return(NULL)
  }

  # Collect all qmd files under the chosen date directories
  log_files <- unlist(lapply(date_dirs, function(dd) {
    list.files(dd, pattern = "\\.qmd$", full.names = TRUE, recursive = FALSE)
  }), use.names = FALSE)

  if (length(log_files) == 0) {
    return(NULL)
  }

  # Pick the most recently modified file
  fi <- file.info(log_files, extra_cols = FALSE)
  most_recent <- log_files[which.max(fi$mtime)]
  most_recent
}

#' Get the most recent log file across both output and dev builds
#'
#' @return Character or NULL. Path to the most recent log file, or NULL if none found.
#' @keywords internal
.log_file_get_most_recent <- function() {
  # Get most recent from both build types
  output_log <- .log_file_get_current("output")
  dev_log <- .log_file_get_current("dev")
  
  # If neither exists, return NULL
  if (is.null(output_log) && is.null(dev_log)) {
    return(NULL)
  }
  
  # If only one exists, return it
  if (is.null(output_log)) {
    return(dev_log)
  }
  if (is.null(dev_log)) {
    return(output_log)
  }
  
  # Both exist, return the most recent
  fi_output <- file.info(output_log, extra_cols = FALSE)
  fi_dev <- file.info(dev_log, extra_cols = FALSE)
  
  if (fi_output$mtime >= fi_dev$mtime) {
    return(output_log)
  } else {
    return(dev_log)
  }
}

#' View build log (last n lines)
#'
#' Display the last N lines of a detailed build log file.
#'
#' @param log_file Character. Path to a log file. If NULL, the most recent log file
#'   across both output and dev builds will be used. To view a specific build type,
#'   use the build_type parameter.
#' @param build_type Character. Either "output", "dev", or "auto" (default). When "auto",
#'   selects the most recent log across both types. When "output" or "dev", selects
#'   the most recent log of that specific type.
#' @param n_lines Integer. Number of lines to show from the end of the file.
#'   Default is 10. Set to NULL or NA to show the entire file.
#' @param show_header Logical. Whether to print a short header including the
#'   logfile path and last modification time. Default is TRUE.
#' @export
projr_log_view <- function(log_file = NULL,
                           build_type = "auto",
                           n_lines = 10,
                           show_header = TRUE) {
  .assert_in(build_type, c("auto", "output", "dev"))
  .assert_lgl(show_header)

  if (is.null(log_file)) {
    if (build_type == "auto") {
      log_file <- .log_file_get_most_recent()
    } else {
      log_file <- .log_file_get_current(build_type)
    }
  } else {
    .assert_string(log_file, required = TRUE)
  }

  if (is.null(log_file) || !file.exists(log_file)) {
    if (build_type == "auto") {
      cli::cli_alert_info("No log files found")
    } else {
      cli::cli_alert_info("No log file found for build type '{build_type}'")
    }
    return(invisible(NULL))
  }

  # Read file safely
  lines <- tryCatch(
    readLines(log_file, warn = FALSE),
    error = function(e) {
      cli::cli_alert_danger("Could not read log file: {e$message}")
      return(character(0))
    }
  )

  if (length(lines) == 0) {
    cli::cli_alert_info("Log file is empty: {log_file}")
    return(invisible(NULL))
  }

  # Determine lines to display
  if (is.null(n_lines) || is.na(n_lines) || n_lines <= 0) {
    to_show <- lines
  } else {
    to_show <- tail(lines, n_lines)
  }

  if (show_header) {
    fi <- file.info(log_file, extra_cols = FALSE)
    cli::cat_line("Log file: {cli::col_green(log_file)}")
    if (!is.na(fi$mtime)) {
      cli::cat_line("Modified: {format(fi$mtime, '%Y-%m-%d %H:%M:%S')}")
    }
    cli::cat_line("")
  }

  # Print the selected lines
  cat(paste(to_show, collapse = "\n"), "\n")
  invisible(to_show)
}