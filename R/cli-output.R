# CLI Output Management
# ======================
#
# This module provides functions for managing CLI output during builds.
# Three output levels are supported:
# - "none": No additional messages (default for dev builds)
# - "std": Standard messaging (default for output builds)
# - "debug": Verbose messaging for debugging

# Output level detection
# ----------------------

#' Get the current output level
#'
#' @param output_level Character. Explicit output level ("none", "std", "debug").
#'   If NULL, determined from environment variable PROJR_OUTPUT_LEVEL.
#' @param output_run Logical. Whether this is an output build (vs dev build).
#'   Used to set default if output_level is NULL.
#'
#' @return Character. The output level to use.
#' @keywords internal
.cli_output_level_get <- function(output_level = NULL, output_run = FALSE) {
  # If explicitly provided, validate and use it
  if (!is.null(output_level)) {
    .assert_in(output_level, c("none", "std", "debug"))
    return(output_level)
  }

  # Check environment variable
  env_level <- Sys.getenv("PROJR_OUTPUT_LEVEL", unset = "")
  if (nzchar(env_level)) {
    .assert_in(env_level, c("none", "std", "debug"))
    return(env_level)
  }

  # Default based on build type
  if (output_run) {
    return("std")
  } else {
    return("none")
  }
}

#' Check if messages should be shown at the given level
#'
#' @param required_level Character. The level required for the message.
#' @param current_level Character. The current output level.
#'
#' @return Logical. TRUE if the message should be shown.
#' @keywords internal
.cli_should_show <- function(required_level, current_level) {
  level_hierarchy <- c("none" = 0, "std" = 1, "debug" = 2)
  level_hierarchy[current_level] >= level_hierarchy[required_level]
}

# Message wrappers
# ----------------

#' Evaluate glue expressions in message
#'
#' @param ... Message components that may contain glue expressions.
#'   Can include named arguments for glue substitution.
#' @param .envir Environment for variable evaluation
#'
#' @return Character. The evaluated message string.
#' @keywords internal
.cli_eval_message <- function(..., .envir = parent.frame()) {
  dots <- list(...)
  if (length(dots) == 0) {
    return("")
  }

  # Separate named and unnamed arguments
  # Handle case where names(dots) might be NULL
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    # No named arguments, all are unnamed
    named_args <- list()
    unnamed_args <- dots
  } else {
    named_args <- dots[dot_names != ""]
    unnamed_args <- dots[dot_names == ""]
  }

  # Combine unnamed parts into a single string
  message_text <- paste(unlist(unnamed_args), collapse = " ")

  # Create a temporary environment with named arguments
  eval_env <- new.env(parent = .envir)
  for (name in names(named_args)) {
    eval_env[[name]] <- named_args[[name]]
  }

  # Evaluate glue expressions in the combined environment
  tryCatch(
    as.character(glue::glue(message_text, .envir = eval_env)),
    error = function(e) {
      # If glue fails, return the original text
      # This handles cases where there are no glue expressions
      message_text
    }
  )
}

#' Show a stage header
#'
#' @param stage_name Character. Name of the build stage.
#' @param build_type Character. Type of build ("dev" or "output").
#' @param output_level Character. Current output level.
#'
#' @keywords internal
.cli_stage_header <- function(stage_name, build_type = "output", output_level = "std") {
  build_label <- if (build_type == "dev") "Development" else "Output"
  message_text <- paste0(build_label, " Build: ", stage_name)

  # Write to most recent log file if it exists
  .log_build_section(message_text)

  # Show in console if appropriate
  if (!.cli_should_show("std", output_level)) {
    return(invisible(NULL))
  }

  # Use cli::cli_h1 which doesn't need .envir since we're constructing the full string
  cli::cli_h1(message_text)
}

#' Show a standard informational message
#'
#' @param ... Message components passed to cli::cli_alert_info
#' @param output_level Character. Current output level.
#' @param .envir Environment for variable evaluation
#'
#' @keywords internal
# Generic CLI message function for logging and console output
.cli_message <- function(...,
                         log_type,
                         console_level = "std",
                         console_fn,
                         output_level = "std",
                         .envir = parent.frame()) {
  # Capture all arguments
  dots <- list(...)

  # Evaluate glue expressions (handles both named and unnamed)
  message_text <- ""
  if (length(dots) > 0) {
    message_text <- .cli_eval_message(..., .envir = .envir)
    .log_build_append(message_text, log_type)
  }

  # Show in console if appropriate
  if (!.cli_should_show(console_level, output_level)) {
    return(invisible(NULL))
  }

  # Use the evaluated message text for console output
  console_fn(message_text)
}

.cli_info <- function(..., output_level = "std", .envir = parent.frame()) {
  .cli_message(
    ...,
    log_type = "info",
    console_level = "std",
    console_fn = cli::cli_alert_info,
    output_level = output_level,
    .envir = .envir
  )
}

#' Show a success message
#'
#' @param ... Message components passed to cli::cli_alert_success
#' @param output_level Character. Current output level.
#' @param .envir Environment for variable evaluation
#'
#' @keywords internal
.cli_success <- function(..., output_level = "std", .envir = parent.frame()) {
  .cli_message(
    ...,
    log_type = "success",
    console_level = "std",
    console_fn = cli::cli_alert_success,
    output_level = output_level,
    .envir = .envir
  )
}

#' Show a debug message (only shown at debug level)
#'
#' @param ... Message components passed to cli::cli_text
#' @param output_level Character. Current output level.
#' @param .envir Environment for variable evaluation
#'
#' @keywords internal
.cli_debug <- function(..., output_level = "std", .envir = parent.frame()) {
  .cli_message(
    ...,
    log_type = "debug",
    console_level = "debug",
    console_fn = function(msg) cli::cli_text("[DEBUG] ", msg),
    output_level = output_level,
    .envir = .envir
  )
}

#' Show a step message (sub-item in a stage)
#'
#' @param ... Message components passed to cli::cli_li
#' @param output_level Character. Current output level.
#' @param .envir Environment for variable evaluation
#'
#' @keywords internal
.cli_step <- function(..., output_level = "std", .envir = parent.frame()) {
  .cli_message(
    ...,
    log_type = "step",
    console_level = "std",
    console_fn = cli::cli_li,
    output_level = output_level,
    .envir = .envir
  )
}

#' Start a process with a spinner/status indicator
#'
#' @param ... Message components
#' @param output_level Character. Current output level.
#'
#' @return Process ID for cli_process_done
#' @keywords internal
.cli_process_start <- function(..., output_level = "std") {
  # Log the process start
  dots <- list(...)
  if (length(dots) > 0) {
    message_text <- paste(unlist(dots), collapse = " ")
    .log_build_append(paste0("Process started: ", message_text), "process")
  }

  if (!.cli_should_show("std", output_level)) {
    return(invisible(NULL))
  }
  cli::cli_process_start(...)
}

#' Mark a process as done
#'
#' @param id Process ID from cli_process_start
#' @param msg_done Success message
#' @param msg_failed Failure message
#' @param .envir Environment for the process
#' @param output_level Character. Current output level.
#'
#' @keywords internal
.cli_process_done <- function(id = NULL, msg_done = NULL, msg_failed = NULL,
                              .envir = parent.frame(), output_level = "std") {
  # Log the process completion
  if (!is.null(msg_done)) {
    .log_build_append(paste0("Process done: ", msg_done), "process")
  } else if (!is.null(msg_failed)) {
    .log_build_append(paste0("Process failed: ", msg_failed), "process")
  } else {
    .log_build_append("Process completed", "process")
  }

  if (!.cli_should_show("std", output_level)) {
    return(invisible(NULL))
  }
  if (!is.null(id)) {
    # cli::cli_process_done only accepts msg_done, not msg_failed
    # Use msg_done for both success and failure (or msg_failed if msg_done is NULL)
    msg <- if (!is.null(msg_done)) msg_done else msg_failed
    cli::cli_process_done(id = id, msg_done = msg, .envir = .envir)
  }
}
