# ========================
# Update manifest
# ========================

#' @title Write manifest metadata to a remote
#' @description Writes the supplied manifest tibble to a temporary directory and
#'   uploads it to the destination remote, handling the GitHub asset naming
#'   conventions when necessary.
#' @param type Character scalar identifying the remote backend (`project`,
#'   `local`, `github`, or `osf`).
#' @param remote Backend-specific remote handle (final destination) that will
#'   receive the manifest.
#' @param manifest Tibble/data.frame produced by `.manifest_write()`.
#' @param output_level CLI verbosity level for logging helpers.
#' @return Invisibly returns `TRUE` after the manifest is uploaded.
#' @keywords internal
#' @noRd
.remote_write_manifest <- function(type,
                                   remote,
                                   manifest,
                                   output_level = "std"
                                   ) {
  .cli_debug(
    "Writing manifest.csv to {type} remote",
    output_level = output_level
  )

  path_dir_save <- .dir_create_tmp_random()
  .manifest_write(manifest, file.path(path_dir_save, "manifest.csv"))
  remote <- if (type == "github") {
    remote[["fn"]] <- "manifest.zip"
    remote
  } else {
    remote
  }
  switch(type,
    "project" = NULL,
    .remote_file_add(
      type,
      remote,
      path_dir_save,
      "manifest.csv",
      output_level = output_level
    )
  )
  unlink(path_dir_save, recursive = TRUE)

  .cli_debug(
    "Successfully wrote manifest.csv to {type} remote",
    output_level = output_level
  )

  invisible(TRUE)
}

# ========================
# Write CHANGELOG file
# ========================

#' @title Upload CHANGELOG.md to a remote
#' @description Copies the local `CHANGELOG.md` file to the remote destination,
#'   renaming the GitHub asset when required.
#' @param type Character scalar identifying the remote backend.
#' @param remote_pre Backend-specific parent remote (directory path, release tag
#'   or OSF node) underneath which the changelog will be stored.
#' @return Invisibly returns `TRUE` after the file is uploaded.
#' @keywords internal
#' @noRd
.remote_write_changelog <- function(type,
                                    remote_pre,
                                    output_level = "std") {
  .cli_debug(
    "Writing CHANGELOG file to {type} remote",
    output_level = output_level
  )
  remote <- if (type == "github") {
    remote_pre[["fn"]] <- "CHANGELOG.zip"
    remote_pre
  } else {
    remote_pre
  }
  switch(type,
    "project" = NULL,
    .remote_file_add(
      type, remote, .path_get(), "CHANGELOG.md",
      output_level = output_level
    )
  )
  invisible(TRUE)
}

# ========================
# Update VERSION file
# ========================

#' @title Write the VERSION file to a remote
#' @description Persists the provided VERSION file contents to the destination
#'   remote, using a temporary directory and GitHub-compatible asset naming.
#' @param type Character scalar identifying the remote backend.
#' @param remote_pre Backend-specific parent remote (directory path, release
#'   tag, OSF node).
#' @param version_file Character vector containing the VERSION file lines.
#' @param output_level CLI verbosity level for logging helpers.
#' @return Invisibly returns `TRUE` after the VERSION file is uploaded.
#' @keywords internal
#' @noRd
.remote_write_version_file <- function(type,
                                       remote_pre,
                                       version_file,
                                       output_level = "std"
                                       ) {
  .cli_debug(
    "Writing VERSION file to {type} remote",
    output_level = output_level
  )

  path_dir_save <- .dir_create_tmp_random()
  writeLines(version_file, file.path(path_dir_save, "VERSION"))
  remote <- if (type == "github") {
    remote_pre[["fn"]] <- "VERSION.zip"
    remote_pre
  } else {
    remote_pre
  }
  switch(type,
    "project" = NULL,
    .remote_file_add(
      type,
      remote,
      path_dir_save,
      "VERSION",
      output_level = output_level
    )
  )

  .cli_debug(
    "Successfully wrote VERSION file to {type} remote",
    output_level = output_level
  )

  unlink(path_dir_save, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Get manifests
# ========================

#' @title Retrieve manifest data from a remote
#' @description Downloads `manifest.csv` from the requested remote (or reads it
#'   from the current project) and returns it as a tibble.
#' @param type Character scalar identifying the remote backend.
#' @param remote_pre Backend-specific parent remote used when fetching from a
#'   remote destination.
#' @param output_level CLI verbosity level for logging helpers.
#' @return Tibble containing manifest entries; empty tibble when unavailable.
#' @keywords internal
#' @noRd
.remote_get_manifest <- function(type,
                                 remote_pre,
                                 output_level = "std"
                                 ) {
  .cli_debug(
    "Getting manifest.csv from {type} remote",
    output_level = output_level
  )

  result <- switch(type,
    "project" = .remote_get_manifest_project(),
    .remote_get_manifest_non_project(
      type,
      remote_pre,
      output_level = output_level
    )
  )

  .cli_debug(
    "Retrieved manifest with {nrow(result)} row(s) from {type} remote",
    output_level = output_level
  )

  result
}

#' @title Read the local project manifest
#' @description Convenience helper that loads `manifest.csv` from the current
#'   project root.
#' @return Tibble containing the local manifest entries.
#' @keywords internal
#' @noRd
.remote_get_manifest_project <- function() {
  # just the actual project
  .manifest_read(.path_get("manifest.csv"))
}

#' @title Retrieve manifest data from non-project remotes
#' @description Downloads and parses the manifest stored on remote backends,
#'   returning an empty manifest when none is present.
#' @inheritParams .remote_get_manifest
#' @return Tibble containing remote manifest rows (possibly empty).
#' @keywords internal
#' @noRd
.remote_get_manifest_non_project <- function(type,
                                             remote_pre,
                                             output_level = "std"
                                             ) {
  manifest_actual <- .remote_get_manifest_non_project_raw(
    type,
    remote_pre,
    output_level = output_level
  )
  if (is.null(manifest_actual)) {
    .cli_debug(
      "No manifest found on {type} remote, returning empty manifest",
      output_level = output_level
    )
    return(.zero_tbl_get_manifest())
  }
  manifest_actual
}

#' @title Download the raw manifest file from a remote
#' @description Handles the actual file transfer for `manifest.csv`, returning
#'   parsed manifest data or `NULL` when the file cannot be retrieved.
#' @inheritParams .remote_get_manifest
#' @return Tibble containing manifest contents or `NULL` if missing.
#' @keywords internal
#' @noRd
.remote_get_manifest_non_project_raw <- function(type,
                                                 remote_pre,
                                                 output_level = "std"
                                                 ) {
  path_dir_save <- .dir_create_tmp_random()

  .cli_debug(
    "Attempting to download manifest.csv from {type} remote",
    output_level = output_level
  )

  remote <- if (type == "github") {
    remote_pre[["fn"]] <- "manifest.zip"
    remote_pre
  } else {
    remote_pre
  }

  path_manifest <- tryCatch({
    .remote_file_get(
      type,
      remote,
      "manifest.csv",
      path_dir_save
    )
  }, error = function(e) {
    .cli_debug(
      "Error downloading manifest.csv from {type} remote: {e$message}",
      output_level = output_level
    )
    character(0L)
  })

  if (length(path_manifest) == 0 || !file.exists(path_manifest)) {
    .cli_debug(
      "manifest.csv not found on {type} remote",
      output_level = output_level
    )
    unlink(path_dir_save, recursive = TRUE)
    return(NULL)
  }

  .cli_debug(
    "Successfully downloaded manifest.csv from {type} remote",
    output_level = output_level
  )

  manifest <- .manifest_read(path_manifest)
  unlink(path_dir_save, recursive = TRUE)
  manifest
}

# ========================
# Get VERSION
# ========================

#' @title Retrieve VERSION file contents from a remote
#' @description Downloads the VERSION file (when present) from the remote and
#'   returns its contents as a character vector.
#' @inheritParams .remote_get_manifest
#' @return Character vector with VERSION file lines; empty when absent.
#' @keywords internal
#' @noRd
.remote_get_version_file <- function(type,
                                     remote_pre,
                                     output_level = "std"
                                     ) {
  .cli_debug(
    "Getting VERSION file from {type} remote",
    output_level = output_level
  )

  result <- switch(type,
    "project" = character(0L),
    .remote_get_version_file_non_project(
      type,
      remote_pre,
      output_level = output_level
    )
  )

  if (length(result) > 0) {
    .cli_debug(
      "Retrieved VERSION file from {type} remote ({length(result)} line(s))",
      output_level = output_level
    )
  } else {
    .cli_debug(
      "No VERSION file found on {type} remote",
      output_level = output_level
    )
  }

  result
  }


#' @title Download VERSION file from non-project remotes
#' @description Performs the remote download for the VERSION file and returns
#'   its contents.
#' @inheritParams .remote_get_manifest
#' @return Character vector of VERSION lines (possibly length zero).
#' @keywords internal
#' @noRd
.remote_get_version_file_non_project <- function(type,
                                                 remote_pre,
                                                 output_level = "std"
                                                 ) {
  path_dir_save <- .dir_create_tmp_random()

  .cli_debug(
    "Attempting to download VERSION file from {type} remote",
    output_level = output_level
  )

  remote <- if (type == "github") {
    remote_pre[["fn"]] <- "VERSION.zip"
    remote_pre
  } else {
    remote_pre
  }

  path_version <- tryCatch({
    .remote_file_get(
      type,
      remote,
      "VERSION",
      path_dir_save
    )
  }, error = function(e) {
    .cli_debug(
      "Error downloading VERSION file from {type} remote: {e$message}",
      output_level = output_level
    )
    character(0L)
  })

  if (length(path_version) == 0 || !file.exists(path_version)) {
    .cli_debug(
      "VERSION file not found on {type} remote",
      output_level = output_level
    )
    unlink(path_dir_save, recursive = TRUE)
    return(character(0L))
  }

  .cli_debug(
    "Successfully downloaded VERSION file from {type} remote",
    output_level = output_level
  )

  version_file <- .remote_get_version_file_read(path_version)
  unlink(path_dir_save, recursive = TRUE)
  version_file
}

#' @title Read VERSION contents from disk
#' @description Thin wrapper around `readLines()` that returns an empty vector
#'   when the file path is invalid or missing.
#' @param path Character scalar path to the VERSION file on disk.
#' @return Character vector with VERSION lines, or empty when unavailable.
#' @keywords internal
#' @noRd
.remote_get_version_file_read <- function(path) {
  if (!.is_string(path) || !file.exists(path)) {
    return(character(0L))
  }
  readLines(path, warn = FALSE)
}

# ========================
# Get latest version of a particular label from a remote
# ========================

#' @title Determine the latest version recorded for a label
#' @description Resolves the most recent version recorded for a specific label
#'   across local project or remote destinations, validating manifests when
#'   possible.
#' @param remote_pre Backend-specific parent remote object.
#' @param type Remote backend identifier.
#' @param label Directory label whose version should be queried.
#' @param structure Remote structure (`latest` or `archive`).
#' @return Version string stripped of suffixes, or empty when unavailable.
#' @keywords internal
#' @noRd
.remote_get_version_latest_label <- function(remote_pre,
                                             type,
                                             label,
                                             structure) {
  if (type == "project") {
    .remote_get_version_project()
  } else {
    .remote_get_version_latest_label_non_project(
      remote_pre, type, label, structure
    )
  }
}

#' @title Get the current project version without suffixes
#' @description Helper that returns the project version stripped of any dev
#'   suffix, mirroring what remote version files store.
#' @return Character scalar version string.
#' @keywords internal
#' @noRd
.remote_get_version_project <- function() {
  .version_get() |> .version_v_rm()
}

#' @title Determine the latest trusted version stored on a remote
#' @description Uses remote version files, manifest comparisons, and structure
#'   rules to decide whether a label/version pair can be trusted for
#'   comparisons.
#' @inheritParams .remote_get_version_latest_label
#' @return Character version string or empty vector when no trusted version is
#'   available.
#' @keywords internal
#' @noRd
.remote_get_version_latest_label_non_project <- function(remote_pre, # nolint
                                                         type,
                                                         label,
                                                         structure) {
  # use the versioned files (raw-data-project: v1.0.0)
  version_file <- .remote_get_version_latest_label_non_project_file(
    remote_pre, type, label
  )

  # if it's not correctly formatted (which may happen because
  # it's not found) or if it does not match version_archive,
  # then return nothing
  if (!.version_check_error_free(version_file)) {
    return(character(0L))
  }

  # check what is the version indicated by the file structure
  if (structure == "archive") {
    version_archive <- .remote_get_version_latest_label_non_project_archive(
      remote_pre, type, label, structure
    )
    if (
      !.is_string(version_archive) ||
        !.version_check_error_free(version_archive)
    ) { # nolint
      return(character(0L))
    }
    if (!identical(version_archive, version_file)) {
      return(character(0L))
    }
  }

  # check that the manifest matches
  manifest_project <- .remote_get_manifest_project() |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(version_file)
  manifest_remote <- .remote_get_manifest(type, remote_pre) |>
    .manifest_filter_label(label) |>
    .manifest_filter_version(version_file)
  rownames(manifest_project) <- NULL
  rownames(manifest_remote) <- NULL
  # if the relevant version wasn't actually recorded online
  if (nrow(manifest_remote) == 0L) {
    return(character(0L))
  }
  # if they don't match (being recorded as both
  # empty should be a match)
  change_list <- .change_get_hash(manifest_remote, manifest_project)
  change_list <- change_list[-which(names(change_list) == "fn_same")]
  same_contents <- vapply(change_list, .is_len_0, logical(1)) |> all()
  if (!same_contents) {
    return(character(0L))
  }

  version_file
}

#' @title Inspect archive directories for version inference
#' @description Examines archive-style remotes to determine which versions are
#'   available, returning the newest version present.
#' @inheritParams .remote_get_version_latest_label
#' @return Version string (without suffix) or empty vector when unavailable.
#' @keywords internal
#' @noRd
.remote_get_version_latest_label_non_project_archive <- function(remote_pre,
                                                          type,
                                                          label,
                                                          structure) {
  if (structure != "archive") {
    return(character(0L))
  }
  remote_pre_down <- switch(type,
    "local" = {
      if (is.null(remote_pre)) {
        return(character(0L))
      }
      dir_vec <- .dir_ls(remote_pre, recursive = FALSE)
      if (is.null(dir_vec) || .is_len_0(dir_vec) || !label %in% dir_vec) {
        return(character(0L))
      }
      file.path(remote_pre, label)
    },
    "github" = remote_pre,
    "osf" = stop("Not yet implemented for OSF")
  )
  if (is.null(remote_pre)) {
    return(character(0L))
  }
  remote_final_vec_basename <- .remote_ls_final(
    type, remote_pre_down
  )
  .remote_version_latest_get(remote_final_vec_basename, type, label) |>
    .version_v_rm()
}

#' @title Determine the newest version represented by filenames
#' @description Parses filenames/asset names to extract semantic versions and
#'   returns the maximum.
#' @param fn Character vector of filenames to inspect.
#' @param type Remote backend identifier (some parsing differs for GitHub).
#' @param label Label associated with the filenames (used for GitHub assets).
#' @return Version object or character string depending on backend.
#' @keywords internal
#' @noRd
.remote_version_latest_get <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  if (type != "github") {
    fn <- vapply(fn, .version_v_rm, character(1L))
    # Strip -empty suffix for local/osf remotes (not needed for GitHub .zip files)
    fn <- gsub("-empty$", "", fn)
    return(fn |> package_version() |> max())
  }
  fn <- .remote_version_latest_filter(fn, type, label)
  .remote_version_latest_extract(fn, label)
}

#' @title Filter filenames down to ones that match expected version format
#' @description Applies backend-specific glob patterns that describe how version
#'   filenames should look for the provided label.
#' @inheritParams .remote_version_latest_get
#' @return Character vector of filenames that match the expected pattern.
#' @keywords internal
#' @noRd
.remote_version_latest_filter <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  version_format_regex_dev_n <- .remote_version_latest_filter_get_regex(
    type, label
  )
  grep(version_format_regex_dev_n, fn, value = TRUE)
}

#' @title Build regex describing versioned filenames
#' @description Converts the configured version format into a regex that can
#'   match filenames/asset names for the requested label.
#' @inheritParams .remote_version_latest_get
#' @return Regex string suitable for `grep()` matching.
#' @keywords internal
#' @noRd
.remote_version_latest_filter_get_regex <- function(type, label) {
  version_format <- .yml_metadata_get_version_format(NULL)
  version_format_regex <- gsub("major", "\\\\d\\+", version_format)
  version_format_regex <- gsub("minor", "\\\\d\\+", version_format_regex)
  version_format_regex <- gsub("patch", "\\\\d\\+", version_format_regex)
  version_format_regex <- gsub("\\.dev$|\\-dev", "", version_format_regex)
  version_format_regex <- paste0(label, "-v", version_format_regex)
  if (type == "github") {
    version_format_regex <- paste0(
      paste0(version_format_regex, ".zip"),
      "|",
      paste0(version_format_regex, "-empty.zip")
    )
  }
  utils::glob2rx(version_format_regex)
}

#' @title Extract semantic versions from filenames
#' @description Pulls the version component out of filenames and validates that
#'   every extracted version matches the configured format.
#' @inheritParams .remote_version_latest_get
#' @return Latest version string or empty vector when parsing fails.
#' @keywords internal
#' @noRd
.remote_version_latest_extract <- function(fn, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  fn_no_zip <- sub("\\.zip$", "", fn)
  version_vec <- sub(".*-v(.*)", "\\1", fn_no_zip)
  version_vec <- gsub("-empty", "", version_vec)
  version_vec <- setdiff(version_vec, "")
  if (.is_len_0(version_vec)) {
    return(character(0L))
  }
  version_format_correct <- try(
    vapply(
      version_vec,
      function(x) .version_format_check(x), logical(1)
    ) |>
      all(),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(character(0L))
  }
  .version_get_latest(version_vec)
}

#' @title Read remote VERSION file entries for a specific label
#' @description Parses the remote VERSION file and extracts the line matching
#'   the supplied label.
#' @param remote_pre Backend-specific parent remote.
#' @param type Remote backend identifier.
#' @param label Directory label whose VERSION entry should be retrieved.
#' @return Character version string (without suffix) or empty vector.
#' @keywords internal
#' @noRd
.remote_get_version_latest_label_non_project_file <- function(remote_pre,
                                                       type,
                                                       label) {
  version_file <- .remote_get_version_file(type, remote_pre)
  .remote_get_version_latest_label_non_project_file_extract(
    version_file, label
  )
}

#' @title Extract a single label entry from a VERSION file
#' @description Finds the VERSION line matching the label and returns the
#'   version (without any trust asterisks).
#' @param version_file Character vector representing the remote VERSION file.
#' @param label Directory label to match.
#' @return Version string (without suffix) or empty vector when missing.
#' @keywords internal
#' @noRd
.remote_get_version_latest_label_non_project_file_extract <-
  function(version_file,
           label) {
    match_str <- utils::glob2rx(label) |>
      (\(x) gsub("\\$", "", x))() |>
      paste0(": ")
    label_regex <- grep(match_str, version_file, value = TRUE)
    if (.is_len_0(label_regex)) {
      return(character(0L))
    }
    # Extract version, removing the asterisk if present
    version_with_possible_asterisk <-
      gsub(match_str, "", label_regex) |> trimws()
    # Remove asterisk for version comparison purposes but don't mark as trusted
    gsub("\\*$", "", version_with_possible_asterisk) |> .version_v_rm()
  }


# ==========================
# Get the most recent remote
# ==========================

#' @title Retrieve metadata for the most recently modified remote asset
#' @description Delegates to backend-specific helpers that locate the newest
#'   remote object when multiple candidates exist.
#' @param remote_final Backend-specific final remote handle.
#' @param type Remote backend identifier.
#' @return Backend-specific object describing the newest remote.
#' @keywords internal
#' @noRd
.remote_get_recent <- function(remote_final,
                               type) {
  switch(type,
    "local" = .remote_get_recent_local(remote_final),
    "osf" = .remote_get_recent_osf(remote_final),
    "github" = .remote_get_recent_github(remote_final)
  )
}

#' @title Placeholder for local recent-remote detection
#' @description Currently unimplemented; calling will throw an error.
#' @param remote_final Backend-specific remote handle.
#' @return No return value; always errors.
#' @keywords internal
#' @noRd
.remote_get_recent_local <- function(remote_final) {
  stop("Not defined yet")
}

#' @title Placeholder for OSF recent-remote detection
#' @description Currently unimplemented; calling will throw an error.
#' @param remote_final Backend-specific remote handle.
#' @return No return value; always errors.
#' @keywords internal
#' @noRd
.remote_get_recent_osf <- function(remote_final) {
  stop("Not defined yet")
}

#' @title Placeholder for GitHub recent-remote detection
#' @description Currently unimplemented; calling will throw an error.
#' @param remote_final Backend-specific remote handle.
#' @return No return value; always errors.
#' @keywords internal
#' @noRd
.remote_get_recent_github <- function(remote_final) {
  stop("Not defined yet")
}

# ========================
# Detect whether a remote is version or latest
# based on the remote itself
# ========================

#' @title Detect whether a remote handle represents a versioned or latest structure
#' @description Dispatches to backend-specific helpers that inspect the remote
#'   handle and attempt to classify it as `version` or `latest`.
#' @param remote Backend-specific remote handle.
#' @param type Remote backend identifier.
#' @return Character scalar: "version" or "latest".
#' @keywords internal
#' @noRd
.remote_detect_structure <- function(remote, type) {
  switch(type,
    "local" = .remote_detect_structure_local(remote),
    "osf" = .remote_detect_structure_osf(remote),
    "github" = .remote_detect_structure_github(remote)
  )
}

#' @title Detect structure for local remotes
#' @description Uses the basename of the remote path to decide whether it looks
#'   like a semantic version.
#' @param remote Character path to the local remote.
#' @return "version" or "latest" depending on format.
#' @keywords internal
#' @noRd
.remote_detect_structure_local <- function(remote) {
  version_format_correct <- try(
    .version_format_check(basename(remote)),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "archive"
}

#' @title Detect structure for OSF remotes
#' @description Examines the remote metadata to determine whether the entry
#'   looks like a versioned object.
#' @param remote OSF remote object.
#' @return "version" or "latest" depending on metadata.
#' @keywords internal
#' @noRd
.remote_detect_structure_osf <- function(remote) {
  version_format_correct <- try(
    remote[["name"]][[1]],
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "archive"
}

#' @title Detect structure for GitHub remotes
#' @description Uses the stored asset name to decide whether the asset
#'   represents a specific version archive.
#' @param remote GitHub remote object.
#' @return "version" or "latest".
#' @keywords internal
#' @noRd
.remote_detect_structure_github <- function(remote) {
  version_remote <- .version_get_remote_github(remote)
  if (is.null(version_remote)) "latest" else "archive"
}

#' @title Extract version string embedded in a remote handle
#' @description Dispatches to backend-specific helpers that can parse version
#'   identifiers directly from remote handles.
#' @param remote Backend-specific remote object.
#' @param type Remote backend identifier.
#' @return Character version string or `NULL` when parsing fails.
#' @keywords internal
#' @noRd
.version_get_remote <- function(remote, type) {
  switch(type,
    "github" = .version_get_remote_github(remote),
    "local" = .version_get_remote_local(remote),
    stop("type not recognized")
  )
}

#' @title Extract version from a local remote path
#' @description Parses the basename of a local remote path and validates it as a
#'   version string.
#' @param remote Character path representing the remote directory.
#' @return Character version string or `NULL` when invalid.
#' @keywords internal
#' @noRd
.version_get_remote_local <- function(remote) {
  version <- basename(remote)
  version_format_correct <- try(
    .version_format_check(version),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(NULL)
  }
  version
}

#' @title Extract version from a GitHub asset name
#' @description Pulls the semantic version out of a GitHub asset filename such
#'   as `label-v1.2.3.zip`.
#' @param remote GitHub remote object (list with `fn`).
#' @return Character version string or `NULL` when invalid.
#' @keywords internal
#' @noRd
.version_get_remote_github <- function(remote) {
  version <- sub(".*-v(.*)\\.zip$", "\\1", remote[["fn"]])
  version_format_correct <- try(
    .version_format_check(version),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(NULL)
  }
  version
}