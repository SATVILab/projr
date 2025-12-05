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

.remote_final_check_exists_local <- function(remote_pre,
                                             structure,
                                             label,
                                             version,
                                             output_level = "std") {
  remote_final_pseudo <- if (structure == "archive") {
    file.path(remote_pre, .version_v_add(version))
  } else {
    file.path(remote_pre, label)
  }
  exists <- .remote_check_exists("local", remote_final_pseudo)
  if (exists) {
    .cli_debug(
      "Local remote: Final remote exists at '{remote_path}'",
      remote_path = remote_final_pseudo,
      output_level = output_level
    )
  } else {
    .cli_debug(
      "Local remote: Final remote does not exist at '{remote_path}'",
      remote_path = remote_final_pseudo,
      output_level = output_level
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
  # create this, as we create the OSF sub-directory
  # if specified. Needs to be automated
  # due to versioning
  .remote_create("local", path_dir)
}

# ========================
# Delete an unused empty remote directory
# ========================

# local
.remote_final_rm_if_empty_local <- function(remote,
                                            structure,
                                            output_level = "std") {
  .assert_string(remote, TRUE)
  .assert_in(structure, .opt_remote_get_structure(), TRUE)
  # Only remove empty remotes for archive structure, not latest
  if (structure != "archive") {
    .cli_debug(
      "Local remote: Structure is '{structure}', not removing, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote}' does not exist, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  if (.is_len_pos(list.files(remote))) {
    .cli_debug(
      "Local remote: Directory '{remote}' is not empty, not removing, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  .remote_final_rm_local(remote, output_level)
  invisible(TRUE)
}

# ========================
# Delete a final remote
# ========================

.remote_final_rm_local <- function(remote,
                                   output_level = "std") {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote_path}' does not exist, cannot remove, returning FALSE",
      remote_path = remote,
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  .cli_debug(
    "Local remote: Removing directory '{remote_path}'",
    remote_path = remote,
    output_level = output_level
  )
  unlink(remote, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Empty a final remote
# ========================

.remote_final_empty_local <- function(remote,
                                      output_level = "std") {
  .assert_string(remote, TRUE)
  if (!dir.exists(remote)) {
    .cli_debug(
      "Local remote: Directory '{remote_path}' does not exist, cannot empty, returning FALSE",
      remote_path = remote,
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  .cli_debug(
    "Local remote: Emptying directory '{remote_path}'",
    remote_path = remote,
    output_level = output_level
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
  .dir_copy(remote, path_dir_save_local)
}

# ========================
# Download a single file
# ========================

.remote_file_get_local <- function(remote,
                                       fn,
                                       path_dir_save_local) {
  path_remote_fn <- file.path(remote, fn)
  if (!file.exists(path_remote_fn)) {
    return(character(0L))
  } else {
    path_fn <- file.path(path_dir_save_local, basename(fn))
    file.copy(path_remote_fn, path_fn, overwrite = TRUE)
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
                                  rm_if_empty = TRUE,
                                  output_level = "std") {
  .assert_chr_min(fn, TRUE)
  if (length(fn) == 0L) {
    .cli_debug(
      "Local remote: No files specified for removal, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  .assert_string(remote, TRUE)
  fn_vec <- .file_filter_exists(file.path(remote, fn))
  if (length(fn_vec) == 0L) {
    .cli_debug(
      "Local remote: No matching files found to remove in '{remote_path}', returning FALSE",
      remote_path = remote,
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  count <- length(fn_vec)
  remote_path <- remote
  .cli_debug(
    "Local remote: Removing {count} file(s) from '{remote_path}'",
    output_level = output_level
  )
  suppressWarnings(file.remove(fn_vec))
  if (rm_if_empty) {
    .remote_final_rm_if_empty("local", remote_path)
  }
  invisible(TRUE)
}

# ========================
# Add individual files
# ========================

.remote_file_add_local <- function(fn,
                                   path_dir_local,
                                   remote,
                                   output_level = "std") {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    .cli_debug(
      "Local remote: No files specified for addition, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }
  count <- length(fn)
  from_path <- path_dir_local
  to_path <- remote
  .cli_debug(
    "Local remote: Adding {count} file(s) from '{from_path}' to '{to_path}'",
    output_level = output_level
  )
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)
  .assert_string(remote, TRUE)
  .dir_copy_file(
    fn = fn,
    path_dir_from = path_dir_local,
    path_dir_to = remote
  )
}
