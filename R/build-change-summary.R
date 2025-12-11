# Build Change Summary
# =====================
#
# This module provides functionality to summarize changes between builds
# by comparing manifest hashes from the previous and current versions.

#' Get change summary for build
#'
#' @description
#' Compares the current build's manifest with the previous version's manifest
#' to identify what files have changed in input and output directories.
#'
#' @param output_run Logical. Whether this is an output build.
#'
#' @return Character vector of formatted change summary lines, or NULL if no previous version.
#' @keywords internal
.build_change_summary_get <- function(output_run) {
  if (!output_run) {
    # Dev builds don't track changes in the same way
    return(NULL)
  }

  # Safely get current version - return NULL if not available
  version_current <- tryCatch(
    projr_version_get(),
    error = function(e) NULL
  )

  if (is.null(version_current)) {
    return(NULL)
  }

  version_previous <- .build_change_summary_get_previous_version()

  if (is.null(version_previous)) {
    return(NULL)
  }

  # Get manifest for both versions
  manifest_all <- tryCatch(
    .manifest_read_project(),
    error = function(e) NULL
  )

  if (is.null(manifest_all) || nrow(manifest_all) == 0) {
    return(NULL)
  }

  # Get labels to check (inputs and outputs)
  labels_input <- c(.yml_dir_get_label_cache(NULL), .yml_dir_get_label_raw(NULL))
  labels_output <- c(.yml_dir_get_label_docs(NULL), .yml_dir_get_label_output(NULL))

  # Filter to only those that are hashed
  labels_input <- labels_input[vapply(
    labels_input, .yml_dir_get_hash_complete, logical(1),
    profile = NULL
  )]
  labels_output <- labels_output[vapply(
    labels_output, .yml_dir_get_hash_complete, logical(1),
    profile = NULL
  )]

  # Build summary
  summary_lines <- c()

  if (length(labels_input) > 0) {
    summary_lines <- c(
      summary_lines,
      .build_change_summary_format_section(
        "Inputs", labels_input, manifest_all, version_previous, version_current
      )
    )
  }

  if (length(labels_output) > 0) {
    summary_lines <- c(
      summary_lines,
      .build_change_summary_format_section(
        "Outputs", labels_output, manifest_all, version_previous, version_current
      )
    )
  }

  if (length(summary_lines) == 0) {
    return(NULL)
  }

  summary_lines
}

#' Get the previous version from manifest
#'
#' @return Character version string (without 'v' prefix), or NULL if no previous version.
#' @keywords internal
.build_change_summary_get_previous_version <- function() {
  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(NULL)
  }

  # Get all unique versions
  versions <- unique(manifest[["version"]])
  versions <- vapply(versions, .version_v_rm, character(1), USE.NAMES = FALSE)

  # Sort and get the second-most recent (previous to current)
  versions_sorted <- sort(versions, decreasing = TRUE)

  if (length(versions_sorted) < 2) {
    return(NULL)
  }

  versions_sorted[2]
}

#' Format a section of the change summary (inputs or outputs)
#'
#' @param section_name Character. Name of the section ("Inputs" or "Outputs").
#' @param labels Character vector. Directory labels to check.
#' @param manifest_all Data frame. The full manifest.
#' @param version_prev Character. Previous version (no 'v' prefix).
#' @param version_curr Character. Current version (no 'v' prefix).
#'
#' @return Character vector of formatted lines for this section.
#' @keywords internal
.build_change_summary_format_section <- function(section_name, labels,
                                                 manifest_all, version_prev,
                                                 version_curr) {
  lines <- c(paste0("**", section_name, " Changes (v", version_prev, " -> v", version_curr, ")**"), "")

  has_changes <- FALSE

  for (label in labels) {
    manifest_prev <- manifest_all |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(version_prev)

    manifest_curr <- manifest_all |>
      .manifest_filter_label(label) |>
      .manifest_filter_version(version_curr)

    # Compare using existing change detection
    change_list <- .change_get_hash(manifest_prev, manifest_curr)

    # Count changes
    n_added <- length(change_list[["fn_source_extra"]])
    n_removed <- length(change_list[["fn_dest_extra"]])
    n_modified <- length(change_list[["fn_diff"]])
    n_unchanged <- length(change_list[["fn_same"]])

    # Skip if no changes
    if (n_added == 0 && n_removed == 0 && n_modified == 0) {
      if (n_unchanged > 0) {
        lines <- c(lines, paste0("- `", label, "`: No changes (", n_unchanged, " file(s) unchanged)"))
      }
      next
    }

    has_changes <- TRUE

    # Format changes for this label
    label_lines <- .build_change_summary_format_label(
      label, change_list, n_added, n_removed, n_modified, n_unchanged
    )

    lines <- c(lines, label_lines)
  }

  if (!has_changes) {
    lines <- c(lines, paste0("- No changes detected in ", tolower(section_name)))
  }

  c(lines, "")
}

#' Format change summary for a single label
#'
#' @param label Character. Directory label.
#' @param change_list List. Output from .change_get_hash().
#' @param n_added Integer. Number of added files.
#' @param n_removed Integer. Number of removed files.
#' @param n_modified Integer. Number of modified files.
#' @param n_unchanged Integer. Number of unchanged files.
#'
#' @return Character vector of formatted lines.
#' @keywords internal
.build_change_summary_format_label <- function(label, change_list, n_added,
                                               n_removed, n_modified, n_unchanged) {
  lines <- c(paste0("- `", label, "`:"))

  # Summary counts
  summary_parts <- c()
  if (n_added > 0) summary_parts <- c(summary_parts, paste0(n_added, " added"))
  if (n_removed > 0) summary_parts <- c(summary_parts, paste0(n_removed, " removed"))
  if (n_modified > 0) summary_parts <- c(summary_parts, paste0(n_modified, " modified"))
  if (n_unchanged > 0) summary_parts <- c(summary_parts, paste0(n_unchanged, " unchanged"))

  lines <- c(lines, paste0("  - ", paste(summary_parts, collapse = ", ")))

  # Show file details if total changes < 10
  total_changes <- n_added + n_removed + n_modified
  if (total_changes < 10) {
    if (n_added > 0) {
      lines <- c(lines, paste0("  - Added: ", paste(change_list[["fn_source_extra"]], collapse = ", ")))
    }
    if (n_removed > 0) {
      lines <- c(lines, paste0("  - Removed: ", paste(change_list[["fn_dest_extra"]], collapse = ", ")))
    }
    if (n_modified > 0) {
      lines <- c(lines, paste0("  - Modified: ", paste(change_list[["fn_diff"]], collapse = ", ")))
    }
  }

  lines
}

#' Get change summary for debug output
#'
#' @description
#' Similar to .build_change_summary_get() but formatted for console output.
#'
#' @param output_run Logical. Whether this is an output build.
#'
#' @return List with 'has_changes' (logical) and 'message' (character vector).
#' @keywords internal
.build_change_summary_get_debug <- function(output_run) {
  summary <- .build_change_summary_get(output_run)

  if (is.null(summary)) {
    return(list(has_changes = FALSE, message = NULL))
  }

  # Extract key info for console output
  # Look for counts in the summary
  has_changes <- any(grepl("added|removed|modified", summary))

  list(
    has_changes = has_changes,
    message = summary
  )
}

#' Display change summary during build
#'
#' @description
#' Shows change summary at debug output level during the build process.
#'
#' @param bump_component Character. Version bump component.
#' @param output_level Character. Current output level.
#'
#' @keywords internal
.build_change_summary_display <- function(bump_component, output_level = "std") {
  output_run <- .build_get_output_run(bump_component)

  # Only display for output builds
  if (!output_run) {
    return(invisible(NULL))
  }

  summary_info <- .build_change_summary_get_debug(output_run)

  if (!summary_info$has_changes || is.null(summary_info$message)) {
    .cli_debug("No changes detected since previous build", output_level = output_level)
    return(invisible(NULL))
  }

  # Display each line of the summary
  for (line in summary_info$message) {
    # Skip empty lines in console output but keep in log
    if (nzchar(line)) {
      .cli_debug(line, output_level = output_level)
    }
  }

  invisible(NULL)
}
