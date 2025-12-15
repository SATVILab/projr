#' @title Query Files Changed Between Versions
#' @rdname projr_manifest_query
#'
#' @description
#' Query which files changed between two project versions based on the manifest.
#' Returns files that were added, modified, or removed between the versions.
#'
#' @param version_from character.
#' Starting version (e.g., "0.0.1" or "v0.0.1").
#' If NULL, uses the earliest version in the manifest.
#' @param version_to character.
#' Ending version (e.g., "0.0.2" or "v0.0.2").
#' If NULL, uses the current project version.
#' @param label character.
#' Optional directory label to filter by (e.g., "output", "raw-data").
#' If NULL, includes all directories.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path relative to directory}
#'   \item{change_type}{Type of change: "added", "modified", or "removed"}
#'   \item{hash_from}{File hash in version_from (NA for added files)}
#'   \item{hash_to}{File hash in version_to (NA for removed files)}
#' }
#' Returns a 0-row data.frame if no changes found.
#'
#' @examples
#' \dontrun{
#' # Query changes between v0.0.1 and v0.0.2
#' projr_manifest_changes("0.0.1", "0.0.2")
#'
#' # Query changes in output directory only
#' projr_manifest_changes("0.0.1", "0.0.2", label = "output")
#'
#' # Query changes from earliest version to current
#' projr_manifest_changes()
#' }
#'
#' @export
projr_manifest_changes <- function(version_from = NULL,
                                   version_to = NULL,
                                   label = NULL) {
  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_changes())
  }

  # Filter out empty manifest entries (used for empty directories)
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_changes())
  }

  # Normalize versions
  version_from <- .manifest_query_normalize_version(
    version_from, manifest, use_earliest = TRUE
  )
  version_to <- .manifest_query_normalize_version(
    version_to, manifest, use_earliest = FALSE
  )

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
  }

  # Get files for each version
  files_from <- .manifest_filter_version(manifest, .version_v_rm(version_from))
  files_to <- .manifest_filter_version(manifest, .version_v_rm(version_to))

  # Find changes
  .manifest_query_compare_versions(files_from, files_to, version_from, version_to)
}


#' @title Query Files Changed in Version Range
#' @rdname projr_manifest_query
#'
#' @description
#' Query which files changed across a range of versions.
#' For each file, shows the version where it last changed.
#'
#' @param version_start character.
#' Starting version (inclusive). If NULL, uses earliest version.
#' @param version_end character.
#' Ending version (inclusive). If NULL, uses current version.
#' @param label character.
#' Optional directory label to filter by.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path relative to directory}
#'   \item{version_first}{First version where file appeared}
#'   \item{version_last_change}{Last version where file was modified}
#'   \item{hash}{Current file hash}
#' }
#'
#' @examples
#' \dontrun{
#' # Query all changes from v0.0.1 to current
#' projr_manifest_range("0.0.1")
#'
#' # Query changes in a specific range
#' projr_manifest_range("0.0.1", "0.0.5")
#' }
#'
#' @export
projr_manifest_range <- function(version_start = NULL,
                                 version_end = NULL,
                                 label = NULL) {
  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_range())
  }

  # Filter out empty manifest entries (used for empty directories)
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_range())
  }

  # Normalize versions
  version_start <- .manifest_query_normalize_version(
    version_start, manifest, use_earliest = TRUE
  )
  version_end <- .manifest_query_normalize_version(
    version_end, manifest, use_earliest = FALSE
  )

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
  }

  # Filter by version range - extract all unique versions from multi-version rows
  all_versions <- .manifest_get_all_versions(manifest)
  # Convert each version individually to character, then to package_version
  versions_as_char <- vapply(
    all_versions,
    function(v) as.character(.version_to_package_version(v)),
    character(1),
    USE.NAMES = FALSE
  )
  versions_sorted <- sort(package_version(versions_as_char))
  version_start_pkg <- .version_to_package_version(version_start)
  version_end_pkg <- .version_to_package_version(version_end)

  versions_in_range <- versions_sorted[
    versions_sorted >= version_start_pkg & versions_sorted <= version_end_pkg
  ]
  versions_in_range <- paste0("v", as.character(versions_in_range))

  # Filter manifest rows that contain any version in range
  keep_rows <- logical(nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (!is.na(ver_str) && nzchar(ver_str)) {
      row_versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      keep_rows[i] <- any(row_versions %in% versions_in_range)
    }
  }
  manifest_range <- manifest[keep_rows, , drop = FALSE]

  # For each unique file (label + fn), find first and last change
  .manifest_query_summarize_range(manifest_range)
}


#' @title Query Last Change Time for Directories
#' @rdname projr_manifest_query
#'
#' @description
#' Query when files in each directory last changed.
#' Shows the most recent version where any file in the directory was modified.
#'
#' @param version character.
#' Version to query. If NULL, uses current project version.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{version_last_change}{Most recent version with changes}
#'   \item{n_files}{Number of files in directory at this version}
#' }
#'
#' @examples
#' \dontrun{
#' # Query last changes for current version
#' projr_manifest_last_change()
#'
#' # Query last changes as of v0.0.5
#' projr_manifest_last_change("0.0.5")
#' }
#'
#' @export
projr_manifest_last_change <- function(version = NULL) {
  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_last_change())
  }

  # Filter out empty manifest entries (used for empty directories)
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_last_change())
  }

  # Normalize version
  version <- .manifest_query_normalize_version(
    version, manifest, use_earliest = FALSE
  )

  # Get all versions up to and including the target version
  all_versions <- .manifest_get_all_versions(manifest)
  versions_sorted <- sort(package_version(vapply(all_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))
  version_pkg <- .version_to_package_version(version)

  versions_up_to <- versions_sorted[versions_sorted <= version_pkg]
  versions_up_to <- paste0("v", as.character(versions_up_to))

  # Filter rows that contain any version in versions_up_to
  # Since version column now stores semicolon-separated lists, need to check membership
  keep_rows <- logical(nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (!is.na(ver_str) && nzchar(ver_str)) {
      row_versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      keep_rows[i] <- any(row_versions %in% versions_up_to)
    }
  }
  manifest_up_to <- manifest[keep_rows, , drop = FALSE]

  # For each label, find the most recent version with changes
  .manifest_query_last_change_by_label(manifest_up_to)
}


# Internal helper functions
# --------------------------------

.manifest_filter_up_to_version <- function(manifest, version_end) {
  if (nrow(manifest) == 0) {
    return(manifest)
  }

  # Get all unique versions from multi-version rows and compare them properly
  all_versions <- .manifest_get_all_versions(manifest)
  version_end_pkg <- .version_to_package_version(version_end)

  # Filter versions that are <= version_end
  versions_to_keep <- character(0)
  for (v in all_versions) {
    v_pkg <- .version_to_package_version(v)
    if (v_pkg <= version_end_pkg) {
      versions_to_keep <- c(versions_to_keep, v)
    }
  }

  # Keep rows that contain any of the versions to keep
  keep_rows <- logical(nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (!is.na(ver_str) && nzchar(ver_str)) {
      row_versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      keep_rows[i] <- any(row_versions %in% versions_to_keep)
    }
  }

  manifest[keep_rows, , drop = FALSE]
}


.manifest_query_normalize_version <- function(version,
                                              manifest,
                                              use_earliest = TRUE) {
  if (is.null(version)) {
    all_versions <- .manifest_get_all_versions(manifest)
    if (length(all_versions) == 0) {
      # If no versions available, use current project version
      return(paste0("v", projr_version_get()))
    }
    versions_pkg <- package_version(vapply(all_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))
    if (use_earliest) {
      version <- min(versions_pkg)
    } else {
      version <- projr_version_get()
    }
    return(paste0("v", as.character(version)))
  }

  .assert_string(version)
  .version_v_add(version)
}


.manifest_query_compare_versions <- function(files_from,
                                             files_to,
                                             version_from,
                                             version_to) {
  # Create unique keys for matching
  files_from$key <- paste(files_from$label, files_from$fn, sep = ":::")
  files_to$key <- paste(files_to$label, files_to$fn, sep = ":::")

  # Find added files (in to but not in from)
  added <- files_to[!(files_to$key %in% files_from$key), , drop = FALSE]
  if (nrow(added) > 0) {
    added$change_type <- "added"
    added$hash_from <- NA_character_
    added$hash_to <- added$hash
  }

  # Find removed files (in from but not in to)
  removed <- files_from[!(files_from$key %in% files_to$key), , drop = FALSE]
  if (nrow(removed) > 0) {
    removed$change_type <- "removed"
    removed$hash_from <- removed$hash
    removed$hash_to <- NA_character_
  }

  # Find modified files (in both but different hash)
  common_keys <- intersect(files_from$key, files_to$key)
  if (length(common_keys) > 0) {
    from_common <- files_from[files_from$key %in% common_keys, , drop = FALSE]
    to_common <- files_to[files_to$key %in% common_keys, , drop = FALSE]

    # Match by key and compare hashes
    from_common <- from_common[order(from_common$key), , drop = FALSE]
    to_common <- to_common[order(to_common$key), , drop = FALSE]

    # Compare hashes, handling empty strings and NAs
    modified_mask <- !is.na(from_common$hash) & !is.na(to_common$hash) &
                     from_common$hash != "" & to_common$hash != "" &
                     from_common$hash != to_common$hash
    if (any(modified_mask, na.rm = TRUE)) {
      modified <- to_common[modified_mask, , drop = FALSE]
      modified$change_type <- "modified"
      modified$hash_from <- from_common$hash[modified_mask]
      modified$hash_to <- modified$hash
    } else {
      modified <- .zero_tbl_get_manifest_changes()
    }
  } else {
    modified <- .zero_tbl_get_manifest_changes()
  }

  # Combine results
  result <- rbind(
    if (nrow(added) > 0) added[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE] else .zero_tbl_get_manifest_changes(),
    if (nrow(removed) > 0) removed[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE] else .zero_tbl_get_manifest_changes(),
    if (nrow(modified) > 0) modified[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE] else .zero_tbl_get_manifest_changes()
  )

  rownames(result) <- NULL
  result
}


.manifest_query_summarize_range <- function(manifest_range) {
  if (nrow(manifest_range) == 0) {
    return(.zero_tbl_get_manifest_range())
  }

  # Create unique key for each file
  manifest_range$key <- paste(manifest_range$label, manifest_range$fn, sep = ":::")

  # Split by key and process each file
  keys <- unique(manifest_range$key)

  result_list <- lapply(keys, function(k) {
    file_history <- manifest_range[manifest_range$key == k, , drop = FALSE]
    
    # With multi-version rows, extract version info differently
    # Each row represents a different hash state
    # Sort by the earliest version in each row's version list
    earliest_versions <- vapply(file_history$version, function(ver_str) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      if (length(versions) == 0) return("v0.0.0")
      versions_sorted <- versions[order(package_version(vapply(versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
      versions_sorted[1]
    }, character(1))
    
    file_history <- file_history[order(package_version(vapply(earliest_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))), , drop = FALSE]

    # Find when hash last changed - it's the last row
    hashes <- file_history$hash
    
    # For version_first and version_last_change, extract from version lists
    first_row_versions <- strsplit(file_history$version[1], ";", fixed = TRUE)[[1]]
    first_row_versions_sorted <- first_row_versions[order(package_version(vapply(first_row_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
    version_first <- first_row_versions_sorted[1]
    
    last_row_versions <- strsplit(file_history$version[nrow(file_history)], ";", fixed = TRUE)[[1]]
    last_row_versions_sorted <- last_row_versions[order(package_version(vapply(last_row_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
    # Use the FIRST version in the last hash state (when that change happened), not the latest
    version_last_change <- last_row_versions_sorted[1]

    data.frame(
      label = file_history$label[1],
      fn = file_history$fn[1],
      version_first = version_first,
      version_last_change = version_last_change,
      hash = hashes[length(hashes)],
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}


.manifest_query_last_change_by_label <- function(manifest_up_to) {
  if (nrow(manifest_up_to) == 0) {
    return(.zero_tbl_get_manifest_last_change())
  }

  labels <- unique(manifest_up_to$label)

  result_list <- lapply(labels, function(lbl) {
    label_data <- manifest_up_to[manifest_up_to$label == lbl, , drop = FALSE]

    # For each unique file, find when its hash last changed
    file_keys <- paste(label_data$label, label_data$fn, sep = ":::")
    unique_files <- unique(file_keys)
    
    last_change_versions <- character(length(unique_files))
    is_file_active <- logical(length(unique_files))
    
    for (i in seq_along(unique_files)) {
      file_key <- unique_files[i]
      file_rows <- label_data[file_keys == file_key, , drop = FALSE]
      
      # Collect all (min_version, max_version, hash) for each hash state of this file
      hash_states <- list()
      for (j in seq_len(nrow(file_rows))) {
        ver_str <- file_rows$version[j]
        if (!is.na(ver_str) && nzchar(ver_str)) {
          versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
          if (length(versions) > 0) {
            # Sort versions to find min and max, but preserve original format
            versions_noprefix <- vapply(versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)
            versions_pkg <- package_version(versions_noprefix)
            sorted_indices <- order(versions_pkg)
            # Use original version strings (with "v" prefix)
            hash_states[[j]] <- list(
              min_ver = versions[sorted_indices[1]],
              max_ver = versions[sorted_indices[length(sorted_indices)]],
              hash = file_rows$hash[j]
            )
          }
        }
      }
      
      # Find the hash state with the latest max_ver (most recent)
      if (length(hash_states) > 0) {
        max_vers <- sapply(hash_states, function(x) x$max_ver)
        # Find the latest version
        if (length(max_vers) == 1) {
          latest_idx <- 1
        } else {
          # Sort to find latest - use version comparison
          max_vers_noprefix <- vapply(max_vers, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)
          max_vers_pkg <- package_version(max_vers_noprefix)
          # Get the index of the maximum
          sorted_indices <- order(max_vers_pkg, decreasing = TRUE)
          latest_idx <- sorted_indices[1]
        }
        # The last change is when that hash was first introduced (use original format from manifest)
        last_change_versions[i] <- hash_states[[latest_idx]]$min_ver
        # File is active if the latest hash is not empty (not a tombstone)
        is_file_active[i] <- nzchar(hash_states[[latest_idx]]$hash)
      } else {
        last_change_versions[i] <- "v0.0.0"
        is_file_active[i] <- FALSE
      }
    }
    
    # Count only active files (exclude tombstones)
    n_active_files <- sum(is_file_active)
    
    # Overall last change for this label is the max across all files
    if (length(last_change_versions) > 0) {
      valid_versions <- last_change_versions[nzchar(last_change_versions)]
      if (length(valid_versions) > 0) {
        # Find the maximum version while preserving original format
        versions_noprefix <- vapply(valid_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)
        versions_pkg <- package_version(versions_noprefix)
        sorted_indices <- order(versions_pkg, decreasing = TRUE)
        version_last <- valid_versions[sorted_indices[1]]
      } else {
        version_last <- "v0.0.0"
      }
    } else {
      version_last <- "v0.0.0"
    }

    data.frame(
      label = lbl,
      version_last_change = version_last,
      n_files = n_active_files,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}


# Zero table helpers
# --------------------------------

.zero_tbl_get_manifest_changes <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    change_type = character(0),
    hash_from = character(0),
    hash_to = character(0),
    stringsAsFactors = FALSE
  )
}


.zero_tbl_get_manifest_range <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    version_first = character(0),
    version_last_change = character(0),
    hash = character(0),
    stringsAsFactors = FALSE
  )
}


.zero_tbl_get_manifest_last_change <- function() {
  data.frame(
    label = character(0),
    version_last_change = character(0),
    n_files = integer(0),
    stringsAsFactors = FALSE
  )
}


# File-specific query functions
# --------------------------------

#' @title Query When a Specific File Last Changed
#' @rdname projr_manifest_file_query
#'
#' @description
#' Query when a specific file last changed in the manifest.
#' Returns the most recent version where the file's hash was different
#' from the previous version, or when it first appeared.
#'
#' @param fn character.
#' File path relative to the directory (e.g., "data.csv", "subdir/file.txt").
#' @param label character.
#' Directory label (e.g., "output", "raw-data").
#' If NULL, searches all directories for the file.
#' @param version_end character.
#' End version to search up to. If NULL, uses current project version.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path}
#'   \item{version_last_change}{Version when file last changed}
#'   \item{hash}{Current file hash at that version}
#' }
#' Returns a 0-row data.frame if file not found.
#'
#' @examples
#' \dontrun{
#' # Query when a specific file last changed
#' projr_manifest_file_last_change("data.csv", label = "output")
#'
#' # Search all directories for a file
#' projr_manifest_file_last_change("report.pdf")
#' }
#'
#' @export
projr_manifest_file_last_change <- function(fn, label = NULL, version_end = NULL) {
  .assert_string(fn)

  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_query())
  }

  # Filter out empty manifest entries
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_query())
  }

  # Filter by file name
  manifest <- manifest[manifest$fn == fn, , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_query())
  }

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
    if (nrow(manifest) == 0) {
      return(.zero_tbl_get_manifest_file_query())
    }
  }

  # Normalize version_end
  version_end <- .manifest_query_normalize_version(
    version_end, manifest, use_earliest = FALSE
  )

  # Filter by version range (up to version_end)
  # Keep original version strings to avoid conversion issues
  manifest <- .manifest_filter_up_to_version(manifest, version_end)

  # For each unique label+fn combo, find the last change
  .manifest_file_query_last_change(manifest)
}


#' @title Check If a Specific File Changed Between Versions
#' @rdname projr_manifest_file_query
#'
#' @description
#' Check if a specific file changed between two versions.
#' Returns TRUE if the file's hash is different between the versions,
#' was added, or was removed.
#'
#' @param version_from character.
#' Starting version (e.g., "0.0.1" or "v0.0.1").
#' If NULL, uses the earliest version in the manifest.
#' @param version_to character.
#' Ending version (e.g., "0.0.2" or "v0.0.2").
#' If NULL, uses the current project version.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path}
#'   \item{changed}{Logical - TRUE if file changed}
#'   \item{change_type}{Type of change: "added", "modified", "removed", or "unchanged"}
#'   \item{hash_from}{File hash in version_from (NA for added files)}
#'   \item{hash_to}{File hash in version_to (NA for removed files)}
#' }
#' Returns a 0-row data.frame if file not found in either version.
#'
#' @examples
#' \dontrun{
#' # Check if a file changed between versions
#' projr_manifest_file_changed("data.csv", "output", "0.0.1", "0.0.2")
#'
#' # Check against current version
#' projr_manifest_file_changed("data.csv", "output", "0.0.1")
#' }
#'
#' @export
projr_manifest_file_changed <- function(fn, label = NULL,
                                       version_from = NULL,
                                       version_to = NULL) {
  .assert_string(fn)

  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_changed())
  }

  # Filter out empty manifest entries
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_changed())
  }

  # Filter by file name
  manifest <- manifest[manifest$fn == fn, , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_changed())
  }

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
    if (nrow(manifest) == 0) {
      return(.zero_tbl_get_manifest_file_changed())
    }
  }

  # Normalize versions
  version_from <- .manifest_query_normalize_version(
    version_from, manifest, use_earliest = TRUE
  )
  version_to <- .manifest_query_normalize_version(
    version_to, manifest, use_earliest = FALSE
  )

  # Get files for each version
  files_from <- .manifest_filter_version(manifest, .version_v_rm(version_from))
  files_to <- .manifest_filter_version(manifest, .version_v_rm(version_to))

  # Compare
  .manifest_file_query_compare(files_from, files_to, fn, label)
}


#' @title Get Version History for a Specific File
#' @rdname projr_manifest_file_query
#'
#' @description
#' Get all versions where a specific file changed or appeared.
#' Returns a chronological list of all versions in the manifest
#' where the file's hash is different from the previous version.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path}
#'   \item{version}{Version where file changed or appeared}
#'   \item{hash}{File hash at this version}
#'   \item{change_type}{Type of change: "first_appearance", "modified", or "current"}
#' }
#' Returns a 0-row data.frame if file not found.
#'
#' @examples
#' \dontrun{
#' # Get full history for a file
#' projr_manifest_file_history("data.csv", label = "output")
#'
#' # Search all directories
#' projr_manifest_file_history("config.yml")
#' }
#'
#' @export
projr_manifest_file_history <- function(fn, label = NULL) {
  .assert_string(fn)

  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_history())
  }

  # Filter out empty manifest entries
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_history())
  }

  # Filter by file name
  manifest <- manifest[manifest$fn == fn, , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_history())
  }

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
    if (nrow(manifest) == 0) {
      return(.zero_tbl_get_manifest_file_history())
    }
  }

  # Build history
  .manifest_file_query_history(manifest)
}


#' @title Get When a Specific File First Appeared
#' @rdname projr_manifest_file_query
#'
#' @description
#' Get the version when a specific file first appeared in the manifest.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{fn}{File path}
#'   \item{version_first}{Version when file first appeared}
#'   \item{hash}{File hash at first appearance}
#' }
#' Returns a 0-row data.frame if file not found.
#'
#' @examples
#' \dontrun{
#' # Get when a file first appeared
#' projr_manifest_file_first("data.csv", label = "output")
#'
#' # Search all directories
#' projr_manifest_file_first("README.md")
#' }
#'
#' @export
projr_manifest_file_first <- function(fn, label = NULL) {
  .assert_string(fn)

  manifest <- .manifest_read_project()

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_first())
  }

  # Filter out empty manifest entries
  manifest <- manifest[!is.na(manifest$fn) & manifest$fn != "", , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_first())
  }

  # Filter by file name
  manifest <- manifest[manifest$fn == fn, , drop = FALSE]

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_first())
  }

  # Filter by label if provided
  if (!is.null(label)) {
    .assert_string(label)
    manifest <- manifest[manifest$label == label, , drop = FALSE]
    if (nrow(manifest) == 0) {
      return(.zero_tbl_get_manifest_file_first())
    }
  }

  # Find first appearance for each label+fn combo
  .manifest_file_query_first(manifest)
}


# Internal helper functions for file queries
# --------------------------------

.manifest_file_query_last_change <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_query())
  }

  # Group by label
  labels <- unique(manifest$label)

  result_list <- lapply(labels, function(lbl) {
    label_data <- manifest[manifest$label == lbl, , drop = FALSE]

    # Sort by earliest version in each row's version list
    earliest_versions <- vapply(label_data$version, function(ver_str) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      if (length(versions) == 0) return("v0.0.0")
      versions_sorted <- versions[order(package_version(vapply(versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
      versions_sorted[1]
    }, character(1))
    label_data <- label_data[order(package_version(vapply(earliest_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))), , drop = FALSE]

    # Find when hash last changed
    versions <- label_data$version
    hashes <- label_data$hash

    # Start from the end and work backwards to find last change
    if (length(versions) == 1) {
      # Only one version - extract the first version from the multi-version string
      ver_list <- strsplit(versions[1], ";", fixed = TRUE)[[1]]
      if (length(ver_list) > 0) {
        ver_sorted <- ver_list[order(package_version(vapply(ver_list, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
        version_last_change <- ver_sorted[1]
      } else {
        version_last_change <- "v0.0.0"
      }
      last_idx <- 1
    } else {
      # Multiple versions - find where hash differs from next
      change_idx <- length(versions)
      for (i in length(versions):2) {
        if (hashes[i] != hashes[i - 1]) {
          change_idx <- i
          break
        }
      }
      # Extract the first version from the change row's multi-version string
      ver_list <- strsplit(versions[change_idx], ";", fixed = TRUE)[[1]]
      if (length(ver_list) > 0) {
        ver_sorted <- ver_list[order(package_version(vapply(ver_list, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
        version_last_change <- ver_sorted[1]
      } else {
        version_last_change <- "v0.0.0"
      }
      last_idx <- change_idx
    }

    data.frame(
      label = lbl,
      fn = label_data$fn[1],
      version_last_change = version_last_change,
      hash = label_data$hash[last_idx],
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}


.manifest_file_query_compare <- function(files_from, files_to, fn, label) {
  # Determine if file exists in each version
  in_from <- nrow(files_from) > 0
  in_to <- nrow(files_to) > 0

  if (!in_from && !in_to) {
    # File not found in either version
    return(.zero_tbl_get_manifest_file_changed())
  }

  # Use the first label found if label is NULL
  if (is.null(label)) {
    if (in_from) {
      label <- files_from$label[1]
    } else {
      label <- files_to$label[1]
    }
  }

  if (!in_from) {
    # File added
    return(data.frame(
      label = label,
      fn = fn,
      changed = TRUE,
      change_type = "added",
      hash_from = NA_character_,
      hash_to = files_to$hash[1],
      stringsAsFactors = FALSE
    ))
  }

  if (!in_to) {
    # File removed
    return(data.frame(
      label = label,
      fn = fn,
      changed = TRUE,
      change_type = "removed",
      hash_from = files_from$hash[1],
      hash_to = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # File exists in both - check if hash changed
  hash_from <- files_from$hash[1]
  hash_to <- files_to$hash[1]

  if (hash_from == hash_to) {
    return(data.frame(
      label = label,
      fn = fn,
      changed = FALSE,
      change_type = "unchanged",
      hash_from = hash_from,
      hash_to = hash_to,
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame(
      label = label,
      fn = fn,
      changed = TRUE,
      change_type = "modified",
      hash_from = hash_from,
      hash_to = hash_to,
      stringsAsFactors = FALSE
    ))
  }
}


.manifest_file_query_history <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_history())
  }

  # Group by label
  labels <- unique(manifest$label)

  result_list <- lapply(labels, function(lbl) {
    label_data <- manifest[manifest$label == lbl, , drop = FALSE]

    # Sort by earliest version in each row's version list
    earliest_versions <- vapply(label_data$version, function(ver_str) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      if (length(versions) == 0) return("v0.0.0")
      versions_sorted <- versions[order(package_version(vapply(versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
      versions_sorted[1]
    }, character(1))
    label_data <- label_data[order(package_version(vapply(earliest_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))), , drop = FALSE]

    # Track changes
    versions <- label_data$version
    hashes <- label_data$hash

    # First version is always included
    change_indices <- c(1)
    change_types <- c("first_appearance")

    # Find all versions where hash changed
    if (length(versions) > 1) {
      for (i in 2:length(versions)) {
        if (hashes[i] != hashes[i - 1]) {
          change_indices <- c(change_indices, i)
          change_types <- c(change_types, "modified")
        }
      }
    }

    # Extract individual change versions from multi-version strings
    change_versions <- character(length(change_indices))
    for (j in seq_along(change_indices)) {
      idx <- change_indices[j]
      ver_list <- strsplit(label_data$version[idx], ";", fixed = TRUE)[[1]]
      if (length(ver_list) > 0) {
        # For a change row, use the first (earliest) version when that hash appeared
        ver_sorted <- ver_list[order(package_version(vapply(ver_list, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
        change_versions[j] <- ver_sorted[1]
      } else {
        change_versions[j] <- "v0.0.0"
      }
    }

    # Mark the last one as current if its change version is the latest version in manifest
    if (length(change_types) > 0) {
      # Get the latest version from the last row's version list
      last_row_versions <- strsplit(label_data$version[nrow(label_data)], ";", fixed = TRUE)[[1]]
      if (length(last_row_versions) > 0) {
        latest_version_sorted <- last_row_versions[order(package_version(vapply(last_row_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)), decreasing = TRUE)]
        latest_version <- latest_version_sorted[1]
        # Compare with the last change version
        if (change_versions[length(change_versions)] == latest_version) {
          change_types[length(change_types)] <- "current"
        }
      }
    }

    data.frame(
      label = lbl,
      fn = label_data$fn[change_indices],
      version = change_versions,
      hash = label_data$hash[change_indices],
      change_type = change_types,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}


.manifest_file_query_first <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest_file_first())
  }

  # Group by label
  labels <- unique(manifest$label)

  result_list <- lapply(labels, function(lbl) {
    label_data <- manifest[manifest$label == lbl, , drop = FALSE]

    # Sort by earliest version in each row's version list
    earliest_versions <- vapply(label_data$version, function(ver_str) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      if (length(versions) == 0) return("v0.0.0")
      versions_sorted <- versions[order(package_version(vapply(versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
      versions_sorted[1]
    }, character(1))
    label_data <- label_data[order(package_version(vapply(earliest_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))), , drop = FALSE]

    # Extract the actual first version from the version list
    first_row_versions <- strsplit(label_data$version[1], ";", fixed = TRUE)[[1]]
    first_row_versions_sorted <- first_row_versions[order(package_version(vapply(first_row_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)))]
    version_first <- first_row_versions_sorted[1]

    data.frame(
      label = lbl,
      fn = label_data$fn[1],
      version_first = version_first,
      hash = label_data$hash[1],
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}


# Zero table helpers for file queries
# --------------------------------

.zero_tbl_get_manifest_file_query <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    version_last_change = character(0),
    hash = character(0),
    stringsAsFactors = FALSE
  )
}


.zero_tbl_get_manifest_file_changed <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    changed = logical(0),
    change_type = character(0),
    hash_from = character(0),
    hash_to = character(0),
    stringsAsFactors = FALSE
  )
}


.zero_tbl_get_manifest_file_history <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0),
    change_type = character(0),
    stringsAsFactors = FALSE
  )
}


.zero_tbl_get_manifest_file_first <- function() {
  data.frame(
    label = character(0),
    fn = character(0),
    version_first = character(0),
    hash = character(0),
    stringsAsFactors = FALSE
  )
}
