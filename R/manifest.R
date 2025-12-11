# hashing
# ---------------------------

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
  manifest[manifest[["version"]] == .version_v_add(version), ] %@@%
    .zero_tbl_get_manifest()
}

.manifest_filter_out_version_label <- function(manifest, # nolint
                                               version,
                                               label) {
  label_vec_non <- manifest[["label"]] != label
  version_vec_non <- manifest[["version"]] != .version_v_add(version)
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
  manifest[["string"]] <- Reduce(paste0, manifest)
  manifest <- manifest[!duplicated(manifest[["string"]]), ]
  manifest[["string"]] <- NULL
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
  # Try to read from split manifests first, fall back to consolidated
  .manifest_read_project_impl()
}

# Read manifest from split files or consolidated file
.manifest_read_project_impl <- function() {
  # Check if split manifest directory exists
  split_dir <- .manifest_split_get_dir()
  consolidated_path <- .path_get("manifest.csv")

  # Read split manifests if they exist
  manifest_split <- .zero_tbl_get_manifest()
  if (dir.exists(split_dir)) {
    manifest_split <- .manifest_split_read_all()
  }

  # Read consolidated manifest if it exists
  manifest_consolidated <- .zero_tbl_get_manifest()
  if (file.exists(consolidated_path)) {
    manifest_consolidated <- .manifest_read(consolidated_path)
  }

  # If we have split manifests, merge with consolidated to preserve
  # historical versions during migration period
  if (nrow(manifest_split) > 0 && nrow(manifest_consolidated) > 0) {
    # Merge both sources and remove duplicates
    # Split manifests take precedence for versions they contain
    manifest_merged <- rbind(manifest_consolidated, manifest_split) |>
      .manifest_remove_duplicate()
    return(manifest_merged)
  }

  # If only split manifests exist, return them
  if (nrow(manifest_split) > 0) {
    return(manifest_split)
  }

  # If only consolidated exists, return it
  if (nrow(manifest_consolidated) > 0) {
    return(manifest_consolidated)
  }

  # No manifests exist
  .zero_tbl_get_manifest()
}

# Get the directory path for split manifests
.manifest_split_get_dir <- function() {
  file.path(.path_get(), "_projr", "manifest")
}

# Get the file path for a specific version's split manifest
.manifest_split_get_path <- function(version) {
  version <- .version_v_add(version)
  file.path(.manifest_split_get_dir(), paste0(version, ".csv"))
}

# Read all split manifest files and combine them
.manifest_split_read_all <- function() {
  split_dir <- .manifest_split_get_dir()

  if (!dir.exists(split_dir)) {
    return(.zero_tbl_get_manifest())
  }

  # Get all .csv files in the split manifest directory
  manifest_files <- list.files(
    split_dir,
    pattern = "^v.*\\.csv$",
    full.names = TRUE
  )

  if (length(manifest_files) == 0) {
    return(.zero_tbl_get_manifest())
  }

  # Read all manifest files
  manifest_list <- lapply(manifest_files, function(path) {
    tryCatch(
      .manifest_read(path),
      error = function(e) .zero_tbl_get_manifest()
    )
  })

  # Combine all manifests
  if (length(manifest_list) == 0) {
    return(.zero_tbl_get_manifest())
  }

  manifest_combined <- Reduce(rbind, manifest_list)
  rownames(manifest_combined) <- NULL
  manifest_combined
}

# Read a single version's split manifest
.manifest_split_read_version <- function(version) {
  path <- .manifest_split_get_path(version)
  .manifest_read(path)
}

# Write a single version's split manifest
.manifest_split_write_version <- function(manifest, version) {
  # Filter manifest to only this version
  version_str <- .version_v_add(version)
  manifest_version <- manifest[manifest$version == version_str, , drop = FALSE]

  if (nrow(manifest_version) == 0) {
    return(invisible(NULL))
  }

  # Create split manifest directory
  split_dir <- .manifest_split_get_dir()
  .dir_create(split_dir)

  # Get path for this version's manifest
  path <- .manifest_split_get_path(version)

  # If the file already exists, read it and merge
  # This handles cases where the build is run multiple times with the same version
  if (file.exists(path)) {
    manifest_existing <- .manifest_read(path)
    manifest_version <- rbind(manifest_existing, manifest_version)
  }

  # Write this version's manifest (deduplication happens in .manifest_write)
  .manifest_write(manifest_version, path, overwrite = TRUE)

  invisible(path)
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
  if (.is_len_0(version_file)) {
    return(paste0("Project: ", projr_version_get()))
  }
  if (any(grepl("^Project: ", version_file))) {
    version_file <- version_file[!grepl("^Project: ", version_file)]
  }
  c(paste0("Project: ", projr_version_get()), version_file)
}

.version_file_update_label_version <- function(version_file, # nolint
                                               label,
                                               add_asterisk) {
  version_add <- if (add_asterisk) {
    .version_get() |> paste0("*")
  } else {
    .version_get()
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

.version_file_check_label_trusted <- function(version_file, # nolint: object_length_linter, line_length_linter.
                                              label) {
  version_file <- version_file[grepl(paste0("^", label, ": "), version_file)]
  if (.is_len_0(version_file)) {
    return(FALSE)
  }
  # only care about the top one (may be odd stuff lower down, who knows)
  !grepl(paste0(label, "\\:", ".*\\*$"), version_file[[1]])
}

# Utility functions for split manifests
# --------------------------------

#' @title List Available Manifest Versions
#'
#' @description
#' Lists all versions that have split manifest files in the `_projr/manifest/` directory.
#' Useful for understanding what historical manifest data is available.
#'
#' @return Character vector of version strings (with 'v' prefix), or NULL if no split manifests exist.
#'
#' @examples
#' \dontrun{
#' # List all versions with split manifests
#' projr_manifest_versions()
#' }
#'
#' @export
projr_manifest_versions <- function() {
  split_dir <- .manifest_split_get_dir()

  if (!dir.exists(split_dir)) {
    return(NULL)
  }

  # Get all .csv files in the split manifest directory
  manifest_files <- list.files(
    split_dir,
    pattern = "^v.*\\.csv$"
  )

  if (length(manifest_files) == 0) {
    return(NULL)
  }

  # Extract version from filenames (remove .csv extension)
  versions <- sub("\\.csv$", "", manifest_files)

  # Sort versions properly
  versions_pkg <- package_version(vapply(versions, .version_v_rm, character(1), USE.NAMES = FALSE))
  versions_sorted <- versions[order(versions_pkg)]

  versions_sorted
}


#' @title Get Manifest Storage Information
#'
#' @description
#' Returns information about manifest storage, including the number of versions,
#' file sizes, and storage format (split vs consolidated).
#'
#' @return A list with components:
#' \describe{
#'   \item{versions}{Character vector of available versions}
#'   \item{n_versions}{Number of versions with split manifests}
#'   \item{split_manifest_exists}{Logical - whether split manifest directory exists}
#'   \item{consolidated_exists}{Logical - whether consolidated manifest.csv exists}
#'   \item{split_total_size}{Total size of split manifest files in bytes}
#'   \item{consolidated_size}{Size of consolidated manifest.csv in bytes}
#' }
#'
#' @examples
#' \dontrun{
#' # Get manifest storage info
#' info <- projr_manifest_info()
#' print(info)
#' }
#'
#' @export
projr_manifest_info <- function() {
  split_dir <- .manifest_split_get_dir()
  consolidated_path <- .path_get("manifest.csv")

  # Check split manifests
  split_exists <- dir.exists(split_dir)
  versions <- NULL
  n_versions <- 0L
  split_total_size <- 0L

  if (split_exists) {
    versions <- projr_manifest_versions()
    n_versions <- length(versions)

    if (n_versions > 0) {
      manifest_files <- list.files(split_dir, full.names = TRUE, pattern = "^v.*\\.csv$")
      split_total_size <- sum(file.size(manifest_files))
    }
  }

  # Check consolidated manifest
  consolidated_exists <- file.exists(consolidated_path)
  consolidated_size <- if (consolidated_exists) file.size(consolidated_path) else 0L

  list(
    versions = versions,
    n_versions = n_versions,
    split_manifest_exists = split_exists,
    consolidated_exists = consolidated_exists,
    split_total_size = split_total_size,
    consolidated_size = consolidated_size
  )
}


# get minimum acceptable version
.manifest_get_version_earliest_match <- function(label, # nolint
                                                 version_comp) {
  # begin with latest version (most conservative)
  version_earliest_match <- projr_version_get() |>
    .version_v_rm() |>
    package_version()
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
    vapply(.version_v_rm, character(1), USE.NAMES = FALSE) |>
    package_version() |>
    sort()
  version_vec <- version_vec[version_vec >= package_version(version_comp)]
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
