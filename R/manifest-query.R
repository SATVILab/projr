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
  files_from <- manifest[manifest$version == version_from, , drop = FALSE]
  files_to <- manifest[manifest$version == version_to, , drop = FALSE]
  
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
  
  # Filter by version range
  versions <- unique(manifest$version)
  versions_sorted <- sort(package_version(.version_v_rm(versions)))
  version_start_pkg <- package_version(.version_v_rm(version_start))
  version_end_pkg <- package_version(.version_v_rm(version_end))
  
  versions_in_range <- versions_sorted[
    versions_sorted >= version_start_pkg & versions_sorted <= version_end_pkg
  ]
  versions_in_range <- paste0("v", as.character(versions_in_range))
  
  manifest_range <- manifest[manifest$version %in% versions_in_range, , drop = FALSE]
  
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
  versions <- unique(manifest$version)
  versions_sorted <- sort(package_version(.version_v_rm(versions)))
  version_pkg <- package_version(.version_v_rm(version))
  
  versions_up_to <- versions_sorted[versions_sorted <= version_pkg]
  versions_up_to <- paste0("v", as.character(versions_up_to))
  
  manifest_up_to <- manifest[manifest$version %in% versions_up_to, , drop = FALSE]
  
  # For each label, find the most recent version with changes
  .manifest_query_last_change_by_label(manifest_up_to)
}


# Internal helper functions
# --------------------------------

.manifest_query_normalize_version <- function(version, 
                                              manifest,
                                              use_earliest = TRUE) {
  if (is.null(version)) {
    versions <- unique(manifest$version)
    if (length(versions) == 0) {
      # If no versions available, use current project version
      return(paste0("v", projr_version_get()))
    }
    versions_pkg <- package_version(.version_v_rm(versions))
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
    file_history <- file_history[order(package_version(.version_v_rm(file_history$version))), , drop = FALSE]
    
    # Find when hash last changed
    hashes <- file_history$hash
    versions <- file_history$version
    
    # Last change is the most recent version (assuming manifest only adds new entries when hash changes)
    version_last_change <- versions[length(versions)]
    
    data.frame(
      label = file_history$label[1],
      fn = file_history$fn[1],
      version_first = versions[1],
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
    
    # Find most recent version for this label
    versions <- unique(label_data$version)
    versions_pkg <- package_version(.version_v_rm(versions))
    version_last <- paste0("v", as.character(max(versions_pkg)))
    
    # Count files at this version
    files_at_version <- label_data[label_data$version == version_last, , drop = FALSE]
    
    data.frame(
      label = lbl,
      version_last_change = version_last,
      n_files = nrow(files_at_version),
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
