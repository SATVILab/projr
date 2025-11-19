# hashing
# ---------------------------

# Helper to extract all unique versions from manifest with multi-version rows
.manifest_get_all_versions <- function(manifest) {
  if (nrow(manifest) == 0) {
    return(character(0))
  }
  
  all_versions <- character(0)
  for (ver_str in manifest[["version"]]) {
    if (!is.na(ver_str) && nzchar(ver_str)) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      all_versions <- c(all_versions, versions)
    }
  }
  
  unique(all_versions)
}

.manifest_hash_label <- function(label,
                                 output_run) {
  # output is always in safe directory
  # as hashing is done before copying over to final directory
  hash_tbl <- .hash_dir(
    path_dir = projr_path_get_dir(label, safe = !output_run),
    dir_exc = .build_label_get_dir_exc(label)
  ) |>
    .manifest_hash_cache_filter(label)
  if (nrow(hash_tbl) == 0L) {
    .empty_tbl_get_manifest(label, projr::projr_version_get())
  } else {
    cbind(
      data.frame(label = rep(label, nrow(hash_tbl))),
      hash_tbl
    )
  }
}

.manifest_hash_cache_filter <- function(hash_tbl, # nolint
                                        label) {
  if (!.yml_dir_label_class_detect_cache(label)) {
    return(hash_tbl)
  }
  hash_tbl[!grepl("^projr/v\\d+", hash_tbl[["fn"]]), , drop = FALSE]
}

.build_label_get_dir_exc <- function(label) {
  switch(.yml_dir_label_class_get(label),
    "cache" = "projr"
  )
}

# misc operations
# ---------------------------

.manifest_filter_label <- function(manifest, label) {
  manifest[manifest[["label"]] == label, ] %@@%
    .zero_tbl_get_manifest()
}

.manifest_filter_version <- function(manifest, version) {
  # With multi-version manifest, check if target version is in version list
  # Returns files where the target version appears in the semicolon-separated version list
  # Entries with empty hash indicate file removal (tombstones)

  if (nrow(manifest) == 0) {
    return(.zero_tbl_get_manifest())
  }

  target_version <- .version_v_add(version)
  target_version_pkg <- .version_to_package_version(target_version)

  # Check each row to see if target version is in the version list
  keep_rows <- logical(nrow(manifest))
  
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (is.na(ver_str) || ver_str == "") {
      keep_rows[i] <- FALSE
      next
    }
    
    # Split version list
    versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
    
    # Check if any version in list is <= target_version
    # Keep the row if target version exists in list OR if latest version <= target
    has_match <- FALSE
    for (v in versions) {
      v_pkg <- .version_to_package_version(v)
      if (v_pkg <= target_version_pkg) {
        has_match <- TRUE
        break
      }
    }
    keep_rows[i] <- has_match
  }

  manifest_filtered <- manifest[keep_rows, , drop = FALSE]

  if (nrow(manifest_filtered) == 0) {
    return(.zero_tbl_get_manifest())
  }

  # For files with multiple hash states, keep only the most recent one <= target version
  # Group by (label, fn) and select the appropriate hash
  manifest_filtered[["file_key"]] <- paste(
    manifest_filtered[["label"]],
    manifest_filtered[["fn"]],
    sep = ":::"
  )

  file_keys <- unique(manifest_filtered[["file_key"]])
  result_list <- list()

  for (i in seq_along(file_keys)) {
    fkey <- file_keys[i]
    rows <- manifest_filtered[manifest_filtered[["file_key"]] == fkey, , drop = FALSE]

    if (nrow(rows) == 1) {
      result_list[[i]] <- rows[1, c("label", "fn", "version", "hash"), drop = FALSE]
    } else {
      # Multiple hash states for this file - find the most recent one <= target
      best_row <- NULL
      best_version <- NULL

      for (j in seq_len(nrow(rows))) {
        ver_str <- rows[["version"]][j]
        versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
        
        # Find the highest version in this row's list that is <= target
        matching_versions <- character(0)
        for (v in versions) {
          v_pkg <- .version_to_package_version(v)
          if (v_pkg <= target_version_pkg) {
            matching_versions <- c(matching_versions, v)
          }
        }
        
        if (length(matching_versions) > 0) {
          # Sort and get highest
          matching_versions <- matching_versions[order(
            package_version(vapply(matching_versions, function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE)),
            decreasing = TRUE
          )]
          row_best_version <- matching_versions[1]
          
          if (is.null(best_version) || 
              .version_to_package_version(row_best_version) > .version_to_package_version(best_version)) {
            best_version <- row_best_version
            best_row <- rows[j, c("label", "fn", "version", "hash"), drop = FALSE]
          }
        }
      }

      if (!is.null(best_row)) {
        result_list[[i]] <- best_row
      }
    }
  }

  manifest_filtered <- do.call(rbind, result_list)
  rownames(manifest_filtered) <- NULL

  # Filter out tombstone entries (removed files have empty hash)
  manifest_filtered <- manifest_filtered[
    !is.na(manifest_filtered[["hash"]]) & manifest_filtered[["hash"]] != "",
    , drop = FALSE
  ]

  rownames(manifest_filtered) <- NULL
  manifest_filtered %@@% .zero_tbl_get_manifest()
}

.manifest_filter_out_version_label <- function(manifest, # nolint
                                               version,
                                               label) {
  label_vec_non <- manifest[["label"]] != label
  
  # Check if version is NOT in the version list (handle multi-version strings)
  target_version <- .version_v_add(version)
  version_vec_non <- logical(nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (is.na(ver_str) || ver_str == "") {
      version_vec_non[i] <- TRUE
    } else {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      version_vec_non[i] <- !(target_version %in% versions)
    }
  }
  
  manifest[label_vec_non & version_vec_non, ] %@@%
    .zero_tbl_get_manifest()
}

# get what would be added,
# based on the project
# ---------------------------

.manifest_get_add_project <- function(manifest, label) { # nolint
  manifest_project <- .manifest_read_project()
  manifest_add <- manifest_project |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(projr::projr_version_get())
  if (nrow(manifest_add) == 0L) {
    return(.empty_tbl_get_manifest(label, projr::projr_version_get()))
  }
  manifest_add
}

# writing, reading and merging
# ---------------------------

.manifest_write <- function(manifest, path, overwrite = TRUE) {
  rownames(manifest) <- NULL
  manifest |>
    .manifest_remove_duplicate() |>
    .manifest_write_impl(path, overwrite)
}

.manifest_append_previous <- function(manifest, append, path_previous) { # nolint
  if (!append) {
    return(manifest)
  }
  if (is.null(path_previous)) {
    path_previous <- .manifest_get_path_file("NULL")
  }
  if (!file.exists(path_previous)) {
    return(manifest)
  }
  manifest_pre <- .manifest_read(path_previous)
  manifest |> .manifest_append_previous_impl(manifest_pre)
}

.manifest_append_previous_impl <- function(manifest, manifest_pre) { # nolint
  rownames(manifest_pre) <- NULL
  manifest <- manifest_pre |> rbind(manifest)
  rownames(manifest) <- NULL
  manifest
}

.manifest_remove_duplicate <- function(manifest) { # nolint: object_length_linter, line_length_linter.
  # Multi-version deduplication strategy:
  # - For each (label, fn) file, merge CONTIGUOUS versions with same hash
  # - Version column stores semicolon-separated list of versions
  # - IMPORTANT: Only merge consecutive occurrences of the same hash
  # - If hash changes then reverts (A→B→A), keep both A occurrences as separate rows
  #
  # Example 1: file.txt hash_A in v1, v2, v3 → single row with versions="v0.0.1;v0.0.2;v0.0.3"
  # Example 2: file.txt A→B→A across v1, v2, v3 → THREE rows (A@v1, B@v2, A@v3)

  if (nrow(manifest) == 0) {
    return(manifest)
  }

  # First, expand any existing multi-version rows into individual rows
  expanded_rows <- list()
  for (i in seq_len(nrow(manifest))) {
    ver_str <- manifest[["version"]][i]
    if (!is.na(ver_str) && nzchar(ver_str)) {
      versions <- strsplit(ver_str, ";", fixed = TRUE)[[1]]
      for (v in versions) {
        expanded_rows[[length(expanded_rows) + 1]] <- data.frame(
          label = manifest[["label"]][i],
          fn = manifest[["fn"]][i],
          version = v,
          hash = manifest[["hash"]][i],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(expanded_rows) == 0) {
    return(manifest)
  }
  
  manifest_expanded <- do.call(rbind, expanded_rows)
  rownames(manifest_expanded) <- NULL
  
  # Sort by label, fn, and version
  manifest_expanded <- manifest_expanded[order(
    manifest_expanded[["label"]],
    manifest_expanded[["fn"]],
    package_version(vapply(manifest_expanded[["version"]], function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE))
  ), , drop = FALSE]
  
  # Group by (label, fn) and merge contiguous same-hash rows
  file_keys <- unique(paste(manifest_expanded[["label"]], manifest_expanded[["fn"]], sep = ":::"))
  result_list <- list()
  
  for (fkey in file_keys) {
    # Get all rows for this file
    file_rows <- manifest_expanded[
      paste(manifest_expanded[["label"]], manifest_expanded[["fn"]], sep = ":::") == fkey,
      , drop = FALSE
    ]
    
    if (nrow(file_rows) == 0) next
    
    # Merge contiguous rows with same hash
    current_hash <- file_rows[["hash"]][1]
    current_versions <- file_rows[["version"]][1]
    
    if (nrow(file_rows) > 1) {
      for (i in 2:nrow(file_rows)) {
        # Compare hashes, handling NA values
        hash_i <- file_rows[["hash"]][i]
        hash_matches <- (!is.na(current_hash) && !is.na(hash_i) && current_hash == hash_i) ||
                        (is.na(current_hash) && is.na(hash_i))
        
        if (hash_matches) {
          # Same hash - add version to current group
          current_versions <- c(current_versions, file_rows[["version"]][i])
        } else {
          # Hash changed - save current group and start new one
          result_list[[length(result_list) + 1]] <- data.frame(
            label = file_rows[["label"]][1],
            fn = file_rows[["fn"]][1],
            version = paste(current_versions, collapse = ";"),
            hash = current_hash,
            stringsAsFactors = FALSE
          )
          current_hash <- file_rows[["hash"]][i]
          current_versions <- file_rows[["version"]][i]
        }
      }
    }
    
    # Don't forget the last group (or the only group if nrow == 1)
    result_list[[length(result_list) + 1]] <- data.frame(
      label = file_rows[["label"]][1],
      fn = file_rows[["fn"]][1],
      version = paste(current_versions, collapse = ";"),
      hash = current_hash,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(result_list) == 0) {
    return(.zero_tbl_get_manifest())
  }
  
  manifest <- do.call(rbind, result_list)
  rownames(manifest) <- NULL
  manifest
}

.manifest_write_impl <- function(manifest, path, overwrite) {
  rownames(manifest) <- NULL
  if (file.exists(path)) {
    if (!overwrite) {
      stop("file '", path, "' already exists", call. = FALSE)
    }
    invisible(file.remove(path))
  }
  .dir_create(dirname(path))
  utils::write.csv(
    manifest, path,
    row.names = FALSE
  )
  invisible(path)
}

# .manifest_read <- function(path = NULL) {
.manifest_read <- function(path = NULL) {
  if (!.is_string(path) || !file.exists(path)) {
    return(.zero_tbl_get_manifest())
  }
  out_tbl <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.manifest_read_project <- function() {
  .manifest_read(.path_get("manifest.csv"))
}

.manifest_get_path_dir <- function(path_dir) {
  if (is.null(path_dir)) {
    path_dir <- .path_get()
  }
  .dir_create(path_dir)
}

.manifest_get_path_file <- function(path_dir) {
  path_dir |>
    .manifest_get_path_dir() |>
    file.path("manifest.csv")
}

.manifest_version_get_latest <- function(manifest) { # nolint: object_length_linter, line_length_linter.
  manifest[["version"]] |> .version_get_earliest()
}

.empty_tbl_get_manifest <- function(label, version) {
  # If version is NULL, use current project version
  if (is.null(version)) {
    version <- projr_version_get()
  }
  out_df <- data.frame(
    label = label,
    fn = character(1),
    version = version |> .version_v_add(),
    hash = character(1)
  )
  rownames(out_df) <- NULL
  out_df
}

.zero_tbl_get_manifest <- function() {
  out_df <- data.frame(
    label = character(0),
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}

.version_file_update_project_version <- function(version_file) { # nolint: object_length_linter, line_length_linter.
  # Get current version and ensure it's a single version (not multi-version string)
  version_current <- projr_version_get()
  
  # If version contains semicolons (multi-version string), extract latest version
  # and ensure all "v" prefixes are properly handled
  if (grepl(";", version_current, fixed = TRUE)) {
    version_parts <- strsplit(version_current, ";", fixed = TRUE)[[1]]
    # Ensure all parts have "v" prefix, then extract latest
    version_parts <- vapply(version_parts, function(v) {
      if (!startsWith(v, "v")) paste0("v", v) else v
    }, character(1), USE.NAMES = FALSE)
    # Get latest version (last in the list)
    version_current <- version_parts[length(version_parts)]
  }
  
  if (.is_len_0(version_file)) {
    return(paste0("Project: ", version_current))
  }
  if (any(grepl("^Project: ", version_file))) {
    version_file <- version_file[!grepl("^Project: ", version_file)]
  }
  c(paste0("Project: ", version_current), version_file)
}

.version_file_update_label_version <- function(version_file, # nolint
                                               label,
                                               add_asterisk) {
  # Get current version and ensure it's a single version (not multi-version string)
  version_current <- .version_get()
  
  # If version contains semicolons (multi-version string), extract latest version
  # and ensure all "v" prefixes are properly handled
  if (grepl(";", version_current, fixed = TRUE)) {
    version_parts <- strsplit(version_current, ";", fixed = TRUE)[[1]]
    # Ensure all parts have "v" prefix, then extract latest
    version_parts <- vapply(version_parts, function(v) {
      if (!startsWith(v, "v")) paste0("v", v) else v
    }, character(1), USE.NAMES = FALSE)
    # Get latest version (last in the list)
    version_current <- version_parts[length(version_parts)]
  }
  
  version_add <- if (add_asterisk) {
    paste0(version_current, "*")
  } else {
    version_current
  }
  if (.is_len_0(version_file)) {
    return(paste0(label, ": ", version_add))
  }
  label_ind <- which(grepl(paste0("^", label, ": "), version_file))
  line_add <- paste0(label, ": ", version_add)
  if (.is_len_0(label_ind)) {
    c(version_file, line_add)
  } else {
    version_file[label_ind[[1]]] <- line_add
    if (length(label_ind) > 1) {
      version_file[-label_ind[-1]]
    } else {
      version_file
    }
  }
}

.version_file_check_update_label <- function(fn, # nolint: object_length_linter, line_length_linter.
                                             version_file,
                                             label) {
  is_change <- .is_len_pos(fn) # nolint
  is_label_present <- .version_file_check_update_label_present(
    version_file, label
  )
  is_change || !is_label_present
}

.version_file_check_update_label_present <- function(version_file, # nolint
                                                     label) {
  !.is_len_0(which(grepl(paste0("^", label, ": "), version_file)))
}

# get minimum acceptable version
.manifest_get_version_earliest_match <- function(label, # nolint
                                                 version_comp) {
  # begin with latest version (most conservative)
  version_earliest_match <- projr_version_get() |>
    .version_to_package_version()
  # get lowest version available, if version_comp not provided
  version_comp <-
    .manifest_get_version_earliest_match_get_version_comp(version_comp)
  manifest_project <- .remote_get_manifest_project() |>
    .manifest_filter_label(label)
  rownames(manifest_project) <- NULL
  manifest_latest <- manifest_project |>
    .manifest_filter_version(projr_version_get())
  manifest_latest <- manifest_latest[, c("label", "fn", "hash")]

  # Handle empty manifest case
  version_raw <- manifest_project[["version"]]
  if (.is_len_0(version_raw)) {
    return(version_earliest_match)
  }

  version_vec <- version_raw |>
    vapply(function(v) as.character(.version_to_package_version(v)), character(1), USE.NAMES = FALSE) |>
    package_version() |>
    sort()
  version_vec <- version_vec[version_vec >= .version_to_package_version(version_comp)]
  if (.is_len_0(version_vec)) {
    return(version_earliest_match)
  }
  version_vec <- version_vec |> rev()
  for (i in seq_along(version_vec)) {
    version_curr <- version_vec[[i]]
    manifest_curr <- manifest_project |>
      .manifest_filter_version(version_curr)
    manifest_curr <- manifest_curr[, c("label", "fn", "hash")]
    change_list <- .change_get_hash(manifest_curr, manifest_latest)
    change_vec <- change_list[-which(names(change_list) == "fn_same")] |>
      unlist()
    is_change <- .is_len_pos(change_vec)
    if (is_change) {
      return(version_earliest_match)
    }
    version_earliest_match <- version_curr
  }
  version_earliest_match
}

.manifest_get_version_earliest_match_get_version_comp <- # nolint
  function(version_comp = NULL) {
    if (!is.null(version_comp)) {
      return(version_comp)
    }
    version_format <- .yml_metadata_get_version_format(NULL)
    # remove dev
    version_format <- gsub(".dev$", "", version_format)
    # swop all but last component for 0, and last for 1,
    # to get earliest possible valid version
    version_format <- gsub("major|minor|patch", "0", version_format)
    gsub("0$", "1", version_format)
  }
