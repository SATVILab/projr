# ========================
# Update manifest
# ========================

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

.remote_write_changelog <- function(type,
                                    remote_pre) {
  remote <- if (type == "github") {
    remote_pre[["fn"]] <- "CHANGELOG.zip"
    remote_pre
  } else {
    remote_pre
  }
  switch(type,
    "project" = NULL,
    .remote_file_add(type, remote, .path_get(), "CHANGELOG.md")
  )
}

# ========================
# Update VERSION file
# ========================

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
}

# ========================
# Get manifests
# ========================

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

.remote_get_manifest_project <- function() {
  # just the actual project
  .manifest_read(.path_get("manifest.csv"))
}

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
    .zero_tbl_get_manifest()
  } else {
    manifest_actual
  }
}

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

.remote_get_version_file_read <- function(path) {
  if (!.is_string(path) || !file.exists(path)) {
    return(character(0L))
  }
  readLines(path, warn = FALSE)
}

# ========================
# Get latest version of a particular label from a remote
# ========================

.remote_get_version_label <- function(remote_pre,
                                      type,
                                      label,
                                      structure) {
  if (type == "project") {
    .remote_get_version_project()
  } else {
    .remote_get_version_label_non_project(
      remote_pre, type, label, structure
    )
  }
}

.remote_get_version_project <- function() {
  .version_get() |> .version_v_rm()
}

.remote_get_version_label_non_project <- function(remote_pre, # nolint
                                                  type,
                                                  label,
                                                  structure) {
  # use the versioned files (raw-data-project: v1.0.0)
  version_file <- .remote_get_version_label_non_project_file(
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
    version_archive <- .remote_get_version_label_non_project_archive(
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

.remote_get_version_label_non_project_archive <- function(remote_pre,
                                                          type,
                                                          label,
                                                          structure) {
  if (structure != "archive") {
    return(character(0L))
  }
  remote_pre_down <- switch(type,
    "local" = {
      if (is.null(remote_pre)) {
        return(NULL)
      }
      dir_vec <- .dir_ls(remote_pre, recursive = FALSE)
      if (is.null(dir_vec) || .is_len_0(dir_vec) || !label %in% dir_vec) {
        return(NULL)
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

.remote_version_latest_get <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  if (type != "github") {
    fn <- vapply(fn, .version_v_rm, character(1L))
    return(fn |> package_version() |> max())
  }
  fn <- .remote_version_latest_filter(fn, type, label)
  .remote_version_latest_extract(fn, label)
}

.remote_version_latest_filter <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  version_format_regex_dev_n <- .remote_version_latest_filter_get_regex(
    type, label
  )
  grep(version_format_regex_dev_n, fn, value = TRUE)
}

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

.remote_get_version_label_non_project_file <- function(remote_pre,
                                                       type,
                                                       label) {
  version_file <- .remote_get_version_file(type, remote_pre)
  .remote_get_version_label_non_project_file_extract(
    version_file, label
  )
}

.remote_get_version_label_non_project_file_extract <-
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
    version_with_possible_asterisk <- gsub(match_str, "", label_regex) |> trimws()
    # Remove asterisk for version comparison purposes but don't mark as trusted
    gsub("\\*$", "", version_with_possible_asterisk) |> .version_v_rm()
  }


# ==========================
# Get the most recent remote
# ==========================

.remote_get_recent <- function(remote_final,
                               type) {
  switch(type,
    "local" = .remote_get_recent_local(remote_final),
    "osf" = .remote_get_recent_osf(remote_final),
    "github" = .remote_get_recent_github(remote_final)
  )
}

.remote_get_recent_local <- function(remote_final) {
  stop("Not defined yet")
}

.remote_get_recent_osf <- function(remote_final) {
  stop("Not defined yet")
}

.remote_get_recent_github <- function(remote_final) {
  stop("Not defined yet")
}

# ========================
# Detect whether a remote is version or latest
# based on the remote itself
# ========================

.remote_detect_structure <- function(remote, type) {
  switch(type,
    "local" = .remote_detect_structure_local(remote),
    "osf" = .remote_detect_structure_osf(remote),
    "github" = .remote_detect_structure_github(remote)
  )
}

.remote_detect_structure_local <- function(remote) {
  version_format_correct <- try(
    .version_format_check(basename(remote)),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "version"
}

.remote_detect_structure_osf <- function(remote) {
  version_format_correct <- try(
    remote[["name"]][[1]],
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "version"
}

.remote_detect_structure_github <- function(remote) {
  .dep_install("piggyback")
  version_remote <- .version_get_remote_github(remote)
  if (is.null(version_remote)) "latest" else "version"
}

.version_get_remote <- function(remote, type) {
  switch(type,
    "github" = .version_get_remote_github(remote),
    "local" = .version_get_remote_local(remote),
    stop("type not recognized")
  )
}

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
