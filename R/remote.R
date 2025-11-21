
# ========================
# check existence
# ========================

.remote_check_exists <- function(type,
                                 id,
                                 ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_check_exists_local(path = id),
    "osf" = .remote_check_exists_osf(id = id),
    "github" = .remote_check_exists_github(
      tag = .remote_misc_github_tag_get(id),
      ...
    )
  )
}

# ========================
# check existence of remote_final
# ========================

.remote_final_check_exists <- function(type,
                                       id,
                                       label,
                                       structure,
                                       path,
                                       path_append_label,
                                       version,
                                       empty) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  version <- if (is.null(version)) {
    .version_get_v()
  } else {
    version |> .version_v_add()
  }
  remote_pre <- .remote_final_get(
    type, id, label, structure, path, path_append_label,
    version, TRUE, empty
  )
  switch(type,
    "local" = .remote_final_check_exists_local(
      remote_pre, structure, label, version
    ),
    "osf" = .remote_final_check_exists_osf(
      remote_pre, structure, label, version
    ),
    "github" = .remote_final_check_exists_github(
      remote_pre, structure, label, version, empty
    )
  )
}

# ========================
# create remotes
# ========================

# return a character giving us the key information to
# find the remote again

.remote_create <- function(type,
                           id,
                           name,
                           output_level = "std",
                           
                           ...) {
  .cli_debug(
    "Remote create: type={type}, id={id}",
    output_level = output_level
  )

  switch(type,
    "local" = .remote_create_local(path = id),
    "osf" = .remote_create_osf(title = name, ...),
    "github" = .remote_create_github(
      tag = .remote_misc_github_tag_get(id),
      output_level = output_level,
      ...
    )
  )
}


# ========================
# List all final remotes in a particular pre-remote
# ========================

.remote_ls_final <- function(type,
                             remote_pre) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_ls_final_local(remote_pre),
    "osf" = .remote_ls_final_osf(remote_pre),
    "github" = .remote_ls_final_github(remote_pre)
  )
}

# ========================
# Get remote
# =======================

# gets remote in a way that we can work
# with it locally.
# For local and GitHub remotes, that simply
# means the path (local) and tag (GitHub),
# but for OSF it is an `osf_tbl_file` object.
# The main thing is that it must be an object
# we can use to interact with the remote,
# such as checking existence of files,
# uploading files, etc.

.remote_get <- function(type,
                        id) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_get_local(id = id),
    "osf" = .remote_get_osf(id = id),
    "github" = .remote_get_github(.remote_misc_github_tag_get(id)),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# =====================
# Get final remote
# =====================

# wrapper functions to get the "final" remote.
# For hierarchical remotes, this essentially
# means creating sub-directories,
# where the following rules are observed:
# - the path is the first part
# - the label comes next
# - the version comes last
# For GitHub, this means creating
# the file name of the asset,
# where the following rules are observed:
# - any path is prepended
# - the label is appended ot the path
# - the version is appended thereafter
#   (if it's a versioned structure).
# - it then ends in `.zip`.
# - if path_append_label is FALSE and path
#   is not supplied, then is treated as if
#   path_append_label is TRUE.
.remote_final_get <- function(type,
                              id,
                              label,
                              structure,
                              path = NULL,
                              path_append_label = TRUE,
                              version = NULL,
                              pre = FALSE,
                              empty = FALSE) {
  # pre: "one up" from the final remote, e.g. the directory
  # above for hierarchical. Does not apply to flat.
  switch(type,
    "local" = .remote_final_get_local(
      path = id,
      label = label,
      structure = structure,
      path_append_label = path_append_label,
      version = version,
      pre = pre
    ),
    "osf" = .remote_final_get_osf(
      id = id,
      label = label,
      structure = structure,
      path = path,
      path_append_label = path_append_label,
      version = version,
      pre = pre
    ),
    "github" = .remote_final_get_github(
      id = .remote_misc_github_tag_get(id),
      label = label,
      structure = structure,
      path = path,
      path_append_label = path_append_label,
      version = version,
      pre = pre,
      empty = empty
    ),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# wrapper if it returns NULL because we
# already know it does not exist
.remote_final_get_if_exists <- function(type,
                                        id,
                                        label,
                                        structure,
                                        path = NULL,
                                        path_append_label = TRUE,
                                        version = NULL,
                                        pre = FALSE,
                                        empty = FALSE) {
  exists <- .remote_final_check_exists(
    type, id, label, structure, path, path_append_label, version, empty
  )
  if (!exists) {
    return(NULL)
  }
  .remote_final_get(
    type, id, label, structure, path, path_append_label, version, pre, empty
  )
}

# =====================
# get relative paths
# =====================

# overall function
.remote_get_path_rel <- function(path,
                                 path_append_label,
                                 label,
                                 structure,
                                 type,
                                 version,
                                 pre,
                                 empty) {
  switch(type,
    "osf" = , # same as local
    "local" = .remote_get_path_rel_hierarchy(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version,
      pre = pre
    ),
    "github" = .remote_get_path_rel_github(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version,
      empty = empty
    )
  )
}

# hierarchical remotes
.remote_get_path_rel_hierarchy <- function(path,
                                           path_append_label,
                                           label,
                                           structure,
                                           version,
                                           pre) {
  .assert_string(path)
  .assert_flag(path_append_label, TRUE)
  .assert_in_single(structure, .opt_remote_get_structure(), TRUE)
  if (path_append_label) {
    .assert_in(label, .opt_dir_get_label_send(NULL), TRUE)
  }

  args_list <- list()
  if (!is.null(path)) {
    args_list <- list(path)
  }
  if (path_append_label) {
    args_list <- args_list |> append(list(label))
  }
  if (structure == "archive") {
    version_add <- if (is.null(version)) {
      .version_get_v()
    } else {
      version |> .version_v_add()
    }
    args_list <- args_list |> append(list(version_add))
  }
  if (length(args_list) == 0L) {
    return(character())
  }
  if (pre) {
    # remove the label (if structure is latest),
    # or the version (if structure is archive).
    # ensures that the manifest and version files
    # are always at the same level as the labels
    args_list <- args_list[-length(args_list)]
    if (structure == "archive") {
      # need to also remove the label
      # if it's an archive
      args_list <- args_list[-length(args_list)]
    }
  }

  do.call(file.path, args_list)
}


# flat remotes
.remote_get_path_rel_flat <- function(path,
                                      path_append_label,
                                      label,
                                      structure,
                                      version,
                                      empty) {
  .assert_string(path)
  .assert_flag(path_append_label, TRUE)
  .assert_in_single(structure, .opt_remote_get_structure(), TRUE)
  if (path_append_label) {
    .assert_in(label, .opt_dir_get_label_send(NULL), TRUE)
  }
  path_rel <- if (!is.null(path) && !identical(path, "")) path else character()

  # ensure that we use the label
  # if we don't specify the path
  if (!.is_len_0(path_rel) && path_append_label) {
    path_rel <- paste0(path_rel, "-", label)
  } else if (.is_len_0(path_rel)) {
    path_rel <- label
  }
  if (structure == "archive") {
    version_add <- if (is.null(version)) {
      .version_get_v()
    } else {
      version |> .version_v_add()
    }
    path_rel <- paste0(path_rel, "-", version_add)
  }
  if (empty) {
    path_rel <- paste0(path_rel, "-empty")
  }
  path_rel
}

# ========================
# Delete an unused empty remote directory
# ========================

.remote_final_rm_if_empty <- function(type,
                                      remote,
                                      structure) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_rm_if_empty_local(
      remote = remote, structure = structure
    ),
    "osf" = .remote_final_rm_if_empty_osf(
      remote = remote, structure = structure
    ),
    "github" = .remote_final_rm_if_empty_github()
  )
}

# =======================
# Get information about the final remote
# =======================

.remote_final_get_info <- function(type,
                                   remote_final) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = NULL,
    "osf" = NULL,
    "github" = .remote_final_get_info_github(remote_final)
  )
}

# ========================
# Empty a final remote
# ========================

# Will not necessarily delete the remote,
# but will remove all contents.
# In the case of flat remotes, however,
# it will delete the final remote, e.g.
# a specific asset in a GitHub release.
.remote_final_empty <- function(type,
                                remote,
                                output_level = "std") {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_empty_local(remote),
    "osf" = .remote_final_empty_osf(remote),
    "github" = .remote_final_empty_github(
      remote,
      output_level = output_level
    )
  )
}

# ========================
# Hash a particular remote
# ========================

.remote_hash <- function(type,
                         remote_final,
                         version,
                         label) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  hash_tbl <- .change_get_file_dir( # nolint
    type, remote_final
  ) |>
    .hash_dir(version) |>
    .manifest_hash_cache_filter(label)
  if (nrow(hash_tbl) == 0) {
    .zero_tbl_get_manifest()
  } else {
    cbind(
      data.frame(label = rep(label, nrow(hash_tbl))),
      hash_tbl
    )
  }
}

# ========================
# Download all files
# ========================

# return the path to which it's downloaded
.remote_file_get_all <- function(type,
                                 remote,
                                 path_dir_save_local) {
  .assert_string(path_dir_save_local, TRUE)
  .assert_in(type, .opt_remote_get_type(), TRUE)
  .dir_create(path_dir_save_local)
  switch(type,
    "local" = .remote_file_get_all_local(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    ),
    "osf" = .remote_file_get_all_osf(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    ),
    "github" = .remote_file_get_all_github(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    )
  )
}

# ========================
# Download a single file
# ========================

.remote_file_get <- function(type,
                             remote,
                             fn,
                             path_dir_save_local) {
  .assert_string(path_dir_save_local, TRUE)
  .assert_in(type, .opt_remote_get_type(), TRUE)
  .dir_create(path_dir_save_local)
  switch(type,
    "local" = .remote_file_get_local(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    ),
    "osf" = .remote_file_get_osf(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    ),
    "github" = .remote_file_get_github(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    )
  )
}

# ========================
# List all contents
# ========================

.remote_file_ls <- function(type,
                            remote,
                            output_level = "std"
                            ) {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  result <- switch(type,
    "local" = .remote_file_ls_local(remote),
    "osf" = .remote_file_ls_osf(remote),
    "github" = .remote_file_ls_github(remote)
  )

  .cli_debug(
    "Remote file list: type={type}, found {length(result)} file(s)",
    output_level = output_level
  )

  result
}

# ========================
# Delete individual files
# ========================

# pre-specified files
.remote_file_rm <- function(type,
                            fn,
                            remote,
                            output_level = "std") {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  .cli_debug(
    "Remote file remove: type={type}, removing {length(fn)} file(s)",
    output_level = output_level
  )

  switch(type,
    "local" = .remote_file_rm_local(fn = fn, remote = remote),
    "osf" = .remote_file_rm_osf(fn = fn, remote = remote),
    "github" = .remote_file_rm_github(fn = fn, remote = remote)
  )
}

# ========================
# Add individual files
# ========================

.remote_file_add <- function(type,
                             remote,
                             path_dir_local,
                             fn,
                             output_level = "std"
                             ) {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  .cli_debug(
    "Remote file add: type={type}, adding {length(fn)} file(s) from {path_dir_local}",
    output_level = output_level
  )

  switch(type,
    "local" = .remote_file_add_local(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    ),
    "osf" = .remote_file_add_osf(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    ),
    "github" = .remote_file_add_github(
      fn = fn,
      path_dir_local = path_dir_local,
      remote = remote,
      output_level = output_level
    )
  )
}
