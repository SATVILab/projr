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
  .assert_string(label)
  # The default baseline is the earliest nonempty version in the whole project.
  read_label <- if (!is.null(version_from)) label
  manifest <- .manifest_query_comparison(read_label, NULL, version_from, version_to)
  if (!nrow(manifest)) return(.zero_tbl_get_manifest_changes())

  version_from <- .manifest_query_normalize_version(version_from, manifest)
  version_to <- .manifest_query_normalize_version(version_to, manifest, use_earliest = FALSE)
  if (!is.null(label)) manifest <- manifest[manifest$label == label, , drop = FALSE]
  files_from <- manifest[manifest$version == version_from, , drop = FALSE]
  files_to <- manifest[manifest$version == version_to, , drop = FALSE]
  .manifest_query_compare_versions(files_from, files_to, version_from, version_to)
}


#' @title Query Files Changed in Version Range
#' @rdname projr_manifest_query
#'
#' @description
#' List the first and last recorded versions of each file in the range,
#' including builds that reused an earlier directory snapshot.
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
#'   \item{version_last_change}{Last version in the range that recorded this file}
#'   \item{hash}{File hash at that last version}
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
  manifest <- .manifest_query_range_rows(version_start, version_end, label)
  .manifest_query_summarize_range(manifest)
}


#' @title Query Last Change Time for Directories
#' @rdname projr_manifest_query
#'
#' @description
#' Show the most recent version that recorded files in each directory,
#' including builds that reused an earlier directory snapshot.
#'
#' @param version character.
#' Version to query. If NULL, uses current project version.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{label}{Directory label}
#'   \item{version_last_change}{Most recent version that recorded files in this directory}
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
  manifest <- .manifest_query_range_rows(NULL, version)
  .manifest_query_last_change_by_label(manifest)
}


# Internal helper functions
# --------------------------------

.manifest_query_comparison <- function(label, fn, version_from, version_to) {
  .assert_string(label)
  versions <- if (!is.null(version_from)) {
    c(.version_v_add(version_from), .version_v_add(version_to %||% .version_get_v()))
  }
  rows <- .manifest_read_project(label = label, version = versions, fn = fn)
  rows[!is.na(rows$fn) & nzchar(rows$fn), , drop = FALSE]
}

.manifest_query_range_rows <- function(version_start, version_end, label = NULL, fn = NULL) {
  .assert_string(label)
  path_dir <- .manifest_project_dir()
  stored <- .manifest_store_ready(path_dir)
  # Select the index rows before opening their file lists.
  rows <- if (stored) .manifest_store_index(path_dir, label) else .manifest_read_project(label = label, fn = fn)
  if (!nrow(rows)) return(.zero_tbl_get_manifest())
  version_end <- .manifest_query_normalize_version(version_end, rows, use_earliest = FALSE)
  rows <- .manifest_filter_up_to_version(rows, version_end, version_start)
  if (stored) rows <- .manifest_store_expand(rows, path_dir, fn)
  rows[!is.na(rows$fn) & nzchar(rows$fn), , drop = FALSE]
}

.manifest_filter_up_to_version <- function(manifest, version_end, version_start = NULL) {
  versions <- unique(manifest$version)
  numbers <- numeric_version(sub("^v", "", versions))
  keep <- numbers <= numeric_version(.version_v_rm(version_end))
  if (!is.null(version_start)) {
    keep <- keep & numbers >= numeric_version(.version_v_rm(version_start))
  }
  manifest[manifest$version %in% versions[keep], , drop = FALSE]
}


.manifest_query_normalize_version <- function(version,
                                              manifest,
                                              use_earliest = TRUE) {
  if (!is.null(version)) return(.version_v_add(version))
  if (use_earliest && nrow(manifest)) {
    versions <- unique(manifest$version)
    numbers <- numeric_version(sub("^v", "", versions))
    return(versions[match(min(numbers), numbers)])
  }
  .version_get_v()
}


.manifest_query_compare_versions <- function(files_from,
                                             files_to,
                                             version_from,
                                             version_to) {
  # Create unique keys for matching
  files_from$key <- .manifest_query_file_key(files_from)
  files_to$key <- .manifest_query_file_key(files_to)

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

  # Combine results efficiently
  result_parts <- list()
  if (nrow(added) > 0) {
    result_parts[[length(result_parts) + 1]] <- added[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE]
  }
  if (nrow(removed) > 0) {
    result_parts[[length(result_parts) + 1]] <- removed[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE]
  }
  if (nrow(modified) > 0) {
    result_parts[[length(result_parts) + 1]] <- modified[, c("label", "fn", "change_type", "hash_from", "hash_to"), drop = FALSE]
  }

  if (length(result_parts) == 0) {
    result <- .zero_tbl_get_manifest_changes()
  } else if (length(result_parts) == 1) {
    result <- result_parts[[1]]
  } else {
    result <- do.call(rbind, result_parts)
  }

  rownames(result) <- NULL
  result
}


.manifest_query_file_key <- function(rows) {
  # Including the label length keeps paths and labels containing ":" distinct.
  paste(nchar(rows$label), rows$label, rows$fn, sep = ":")
}

.manifest_query_summarize_range <- function(manifest_range) {
  if (!nrow(manifest_range)) return(.zero_tbl_get_manifest_range())

  versions <- unique(manifest_range$version)
  versions <- versions[order(numeric_version(sub("^v", "", versions)))]
  by_version <- order(match(manifest_range$version, versions), method = "radix")
  file <- .manifest_query_file_key(manifest_range)
  files <- unique(file)
  first <- by_version[match(files, file[by_version])]
  reverse <- rev(by_version)
  last <- reverse[match(files, file[reverse])]
  data.frame(
    label = manifest_range$label[first], fn = manifest_range$fn[first],
    version_first = manifest_range$version[first], version_last_change = manifest_range$version[last],
    hash = manifest_range$hash[last]
  )
}


.manifest_query_last_change_by_label <- function(manifest_up_to) {
  if (nrow(manifest_up_to) == 0) {
    return(.zero_tbl_get_manifest_last_change())
  }

  labels <- unique(manifest_up_to$label)
  n_labels <- length(labels)

  # Pre-allocate vectors for better performance
  label_vec <- character(n_labels)
  version_last_change_vec <- character(n_labels)
  n_files_vec <- integer(n_labels)

  for (i in seq_along(labels)) {
    lbl <- labels[i]
    label_data <- manifest_up_to[manifest_up_to$label == lbl, , drop = FALSE]

    # Find most recent version for this label
    versions <- unique(label_data$version)
    versions_pkg <- numeric_version(sub("^v", "", versions))
    version_last <- versions[match(max(versions_pkg), versions_pkg)]

    # Count files at this version
    files_at_version <- label_data[label_data$version == version_last, , drop = FALSE]

    # Store results
    label_vec[i] <- lbl
    version_last_change_vec[i] <- version_last
    n_files_vec[i] <- nrow(files_at_version)
  }

  # Create data.frame once with all pre-allocated vectors
  result <- data.frame(
    label = label_vec,
    version_last_change = version_last_change_vec,
    n_files = n_files_vec,
    stringsAsFactors = FALSE
  )
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
  .assert_string(fn, TRUE)
  manifest <- .manifest_query_range_rows(NULL, version_end, label, fn)
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
  .assert_string(fn, TRUE)
  manifest <- .manifest_query_comparison(label, fn, version_from, version_to)
  if (!nrow(manifest)) return(.zero_tbl_get_manifest_file_changed())

  version_from <- .manifest_query_normalize_version(version_from, manifest)
  version_to <- .manifest_query_normalize_version(version_to, manifest, use_earliest = FALSE)
  files_from <- manifest[manifest$version == version_from, , drop = FALSE]
  files_to <- manifest[manifest$version == version_to, , drop = FALSE]
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
  .assert_string(fn, TRUE)
  .assert_string(label)
  manifest <- .manifest_read_project(label = label, fn = fn)
  manifest <- manifest[!is.na(manifest$fn) & nzchar(manifest$fn), , drop = FALSE]
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
  .assert_string(fn, TRUE)
  .assert_string(label)
  manifest <- .manifest_read_project(label = label, fn = fn)
  manifest <- manifest[!is.na(manifest$fn) & nzchar(manifest$fn), , drop = FALSE]
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
  n_labels <- length(labels)

  # Pre-allocate vectors for better performance
  label_vec <- character(n_labels)
  fn_vec <- character(n_labels)
  version_last_change_vec <- character(n_labels)
  hash_vec <- character(n_labels)

  for (i in seq_along(labels)) {
    lbl <- labels[i]
    label_data <- manifest[manifest$label == lbl, , drop = FALSE]

    # Sort by version
    label_data <- label_data[order(numeric_version(sub("^v", "", label_data$version))), , drop = FALSE]

    # Find when hash last changed
    versions <- label_data$version
    hashes <- label_data$hash

    # Start from the end and work backwards to find last change
    if (length(versions) == 1) {
      # Only one version, so it's the last change
      version_last_change <- versions[1]
    } else {
      # Multiple versions - find where hash differs from next
      version_last_change <- versions[length(versions)]
      for (j in length(versions):2) {
        if (hashes[j] != hashes[j - 1]) {
          version_last_change <- versions[j]
          break
        }
      }
    }

    # Get the index for the last change version
    last_idx <- which(label_data$version == version_last_change)[1]

    # Store results
    label_vec[i] <- lbl
    fn_vec[i] <- label_data$fn[1]
    version_last_change_vec[i] <- version_last_change
    hash_vec[i] <- label_data$hash[last_idx]
  }

  # Create data.frame once with all pre-allocated vectors
  result <- data.frame(
    label = label_vec,
    fn = fn_vec,
    version_last_change = version_last_change_vec,
    hash = hash_vec,
    stringsAsFactors = FALSE
  )
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

  # Collect all results into lists, then combine at end
  all_labels <- list()
  all_fns <- list()
  all_versions <- list()
  all_hashes <- list()
  all_change_types <- list()

  for (k in seq_along(labels)) {
    lbl <- labels[k]
    label_data <- manifest[manifest$label == lbl, , drop = FALSE]

    # Sort by version
    label_data <- label_data[order(numeric_version(sub("^v", "", label_data$version))), , drop = FALSE]

    # Track changes
    versions <- label_data$version
    hashes <- label_data$hash
    n_versions <- length(versions)

    # Pre-allocate with maximum possible size, then trim
    change_indices <- integer(n_versions)
    change_types <- character(n_versions)
    n_changes <- 0

    # First version is always included
    n_changes <- n_changes + 1
    change_indices[n_changes] <- 1
    change_types[n_changes] <- "first_appearance"

    # Find all versions where hash changed (vectorized comparison)
    if (n_versions > 1) {
      hash_changed <- hashes[-1] != hashes[-n_versions]
      changed_idx <- which(hash_changed) + 1 # +1 because we removed first element
      n_new_changes <- length(changed_idx)
      if (n_new_changes > 0) {
        change_indices[(n_changes + 1):(n_changes + n_new_changes)] <- changed_idx
        change_types[(n_changes + 1):(n_changes + n_new_changes)] <- "modified"
        n_changes <- n_changes + n_new_changes
      }
    }

    # Trim to actual size
    change_indices <- change_indices[seq_len(n_changes)]
    change_types <- change_types[seq_len(n_changes)]

    # Mark the last one as current if it's the most recent in manifest
    if (n_changes > 0 && change_indices[n_changes] == nrow(label_data)) {
      change_types[n_changes] <- "current"
    }

    # Store in lists
    all_labels[[k]] <- rep(lbl, n_changes)
    all_fns[[k]] <- label_data$fn[change_indices]
    all_versions[[k]] <- label_data$version[change_indices]
    all_hashes[[k]] <- label_data$hash[change_indices]
    all_change_types[[k]] <- change_types
  }

  # Combine all lists efficiently
  result <- data.frame(
    label = unlist(all_labels, use.names = FALSE),
    fn = unlist(all_fns, use.names = FALSE),
    version = unlist(all_versions, use.names = FALSE),
    hash = unlist(all_hashes, use.names = FALSE),
    change_type = unlist(all_change_types, use.names = FALSE),
    stringsAsFactors = FALSE
  )
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

    # Sort by version and get the first
    label_data <- label_data[order(numeric_version(sub("^v", "", label_data$version))), , drop = FALSE]

    data.frame(
      label = lbl,
      fn = label_data$fn[1],
      version_first = label_data$version[1],
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
