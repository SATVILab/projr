# ========================
# check existence
# ========================

# local
.remote_check_exists_local <- function(path) {
  .assert_path_not_file(path)
  dir.exists(path)
}

# ========================
# check existence of final remote
# ========================

.remote_final_check_exists_local <- function(id,
                                             label,
                                             structure,
                                             path,
                                             path_append_label,
                                             version,
                                             empty) {
  remote_final <- .remote_final_get(
    "local", id, label, structure, path, path_append_label,
    version, FALSE, empty
  )
  exists <- dir.exists(remote_final)
  if (exists) {
    .cli_debug(
      "Local remote: Final remote exists at '{remote_final}'"
    )
  } else {
    .cli_debug(
      "Local remote: Final remote does not exist at '{remote_final}'"
    )
  }
  exists
}

.remote_final_check_exists_direct_local <- function(remote) {
  .assert_string(remote, TRUE)
  exists <- dir.exists(remote)
  if (exists) {
    .cli_debug(
      "Local remote: Direct remote exists at '{remote}'"
    )
  } else {
    .cli_debug(
      "Local remote: Direct remote does not exist at '{remote}'"
    )
  }
  exists
}

# ========================
# create remote
# ========================

# local
.remote_create_local <- function(path) {
  .assert_string(path)
  .dir_create(path)
  invisible(path)
}

# ========================
# Get remote
# =======================

.remote_get_local <- function(id) {
  .assert_string(id, TRUE)
  id
}

# ========================
# list all directories immediately under pre-remote
# ========================

.remote_ls_final_local <- function(remote_pre) {
  list.dirs(remote_pre, full.names = FALSE, recursive = FALSE)
}

# ========================
# Get final remote
# =======================

.remote_final_get_local <- function(path,
                                    path_append_label,
                                    label,
                                    structure,
                                    version = NULL,
                                    pre,
                                    empty) {
  .assert_string(path, TRUE)
  .assert_path_not_file(path)
  .assert_flag(path_append_label)
  .assert_in(label, .opt_dir_get_label_send(NULL))
  .assert_in_single(structure, .opt_remote_get_structure())
  .assert_string(version)
  .assert_lgl(pre, TRUE)

  # the local destination is just the
  # local directory where files are get, so
  # it is just the path.
  # note that `output_run` does not matter,
  # as this is a "remote" in the sense of something
  # interacted without either before or after the build process,
  # unlike the direoctires specified in
  # _projr.yml[["directories"]]`
  path_dir <- .remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    type = "local",
    version = version,
    pre = pre,
    empty = empty
  )
  # Don't automatically create directory here - it will be created when files
  # are actually written via .remote_file_add() -> .dir_copy_file().
  # This prevents creating both empty and full remotes when only checking
  # for existence.
  path_dir
}

# ========================
# Create empty remote directory marker
# ========================

.remote_final_empty_get_local <- function(path,
                                          path_append_label,
                                          label,
                                          structure,
                                          version = NULL) {
  # Get the path for the empty remote
  remote_empty <- .remote_final_get_local(
    path = path,
    label = label,
    structure = structure,
    path_append_label = path_append_label,
    version = version,
    pre = FALSE,
    empty = TRUE
  )

  .cli_debug(
    "remote_final_empty_get_local: Creating empty at '{path}'",
    path = remote_empty
  )

  # Check if it already exists
  if (dir.exists(remote_empty)) {
    .cli_debug(
      "remote_final_empty_get_local: Already exists at '{path}'",
      path = remote_empty
    )
    return(remote_empty)
  }

  # Create the empty directory as a marker (like GitHub creates projr-empty file)
  .dir_create(remote_empty)
  .cli_debug(
    "remote_final_empty_get_local: Created empty directory at '{path}'",
    path = remote_empty
  )
  remote_empty
}

# ========================
# Delete an unused empty remote directory
# ========================

# local
.remote_final_rm_if_empty_local <- function(remote) {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote}' does not exist, returning FALSE"
    )
    return(invisible(FALSE))
  }
  # Check for FILES only (not directories) - empty directories don't count
  all_entries <- list.files(remote, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  files_only <- all_entries[!dir.exists(all_entries)]

  if (.is_len_pos(files_only)) {
    num_files <- length(files_only)
    .cli_debug(
      "Local remote: Directory '{remote}' has {num_files} file(s), not removing, returning FALSE"
    )
    return(invisible(FALSE))
  }
  .cli_debug(
    "Local remote: Directory '{remote}' is empty (no files), removing it"
  )
  .remote_final_rm_local(remote)
  invisible(TRUE)
}

# ========================
# Delete a remote
# ========================

.remote_rm_local <- function(remote) {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    return(invisible(FALSE))
  }
  unlink(remote, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Delete a final remote
# ========================

.remote_final_rm_local <- function(remote) {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote}' does not exist, cannot remove, returning FALSE"
    )
    return(invisible(FALSE))
  }
  .cli_debug(
    "Local remote: Removing directory '{remote}'"
  )
  unlink(remote, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Empty a final remote
# ========================

.remote_final_empty_local <- function(remote) {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote}' does not exist, cannot empty, returning FALSE"
    )
    return(invisible(FALSE))
  }
  .cli_debug(
    "Local remote: Emptying directory '{remote}'"
  )
  dir_vec <- list.dirs(remote, recursive = TRUE)[-1]
  while (length(dir_vec) > 0L) {
    unlink(dir_vec[1], recursive = TRUE)
    dir_vec <- list.dirs(remote, recursive = TRUE)[-1]
  }
  fn_vec <- list.files(remote, full.names = TRUE, all.files = TRUE)[-(1:2)]
  if (length(fn_vec) > 0) {
    file.remove(fn_vec)
  }
  invisible(TRUE)
}

# ========================
# Download all files
# ========================

.remote_file_get_all_local <- function(remote,
                                       path_dir_save_local) {
  .assert_string(remote, TRUE)
  if (grepl("-empty$", remote)) {
    .cli_debug(
      "Local remote: Remote is by definition empty, no files to get, returning character(0L)" # nolint
    )
    return(character(0L))
  }
  # Check if remote directory exists
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Remote directory '{remote}' does not exist, returning character(0L)"
    )
    return(character(0L))
  }
  .dir_copy(remote, path_dir_save_local)
}

# ========================
# Download a single file
# ========================

.remote_file_get_local <- function(remote,
                                   fn,
                                   path_dir_save_local) {
  .assert_string(remote, TRUE)
  if (grepl("-empty$", remote)) {
    .cli_debug(
      "Local remote: Remote is empty, no files to get, returning character(0L)" # nolint
    )
    return(character(0L))
  }
  path_remote_fn <- file.path(remote, fn)
  if (!file.exists(path_remote_fn)) {
    return(character(0L))
  } else {
    path_fn <- file.path(path_dir_save_local, basename(fn))
    fs::file_copy(path_remote_fn, path_fn, overwrite = TRUE)
    path_fn
  }
}

# ========================
# List all contents
# ========================

.remote_file_ls_local <- function(remote) {
  .assert_string(remote, TRUE)
  .file_ls(path_dir = remote)
}

# ========================
# Delete individual files
# ========================

.remote_file_rm_local <- function(fn,
                                  remote,
                                  rm_if_empty = TRUE) {
  .assert_chr_min(fn, TRUE)
  if (length(fn) == 0L) {
    .cli_debug(
      "Local remote: No files specified for removal, returning FALSE"
    )
    return(invisible(FALSE))
  }
  .assert_string(remote, TRUE)
  fn_vec <- .file_filter_exists(file.path(remote, fn))
  if (length(fn_vec) == 0L) {
    .cli_debug(
      "Local remote: No matching files found to remove in '{remote}', returning FALSE"
    )
    return(invisible(FALSE))
  }
  count <- length(fn_vec)
  .cli_debug(
    "Local remote: Removing {count} file(s) from '{remote}'"
  )
  suppressWarnings(file.remove(fn_vec))
  if (rm_if_empty) {
    .remote_final_rm_if_empty("local", remote)
  }
  invisible(TRUE)
}

# ========================
# Add individual files
# ========================

.remote_file_add_local <- function(fn,
                                   path_dir_local,
                                   remote) {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    .cli_debug(
      "Local remote: No files specified for addition, returning FALSE"
    )
    return(invisible(FALSE))
  }
  count <- length(fn)
  from_path <- path_dir_local
  to_path <- remote
  .cli_debug(
    "Local remote: Adding {count} file(s) from '{from_path}' to '{to_path}'"
  )
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)
  .assert_string(remote, TRUE)

  # This will create the directory if it doesn't exist
  .cli_debug(
    "Local remote: Calling .dir_copy_file to copy files"
  )
  .dir_copy_file(
    fn = fn,
    path_dir_from = path_dir_local,
    path_dir_to = remote
  )
}
