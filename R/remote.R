# ========================
# check existence
# ========================

#' @title Check whether a remote exists
#' @description Verifies that the requested remote (local directory
#'   or GitHub release/tag) exists before attempting downstream operations.
#' @param type Character scalar identifying the remote backend (`local`
#'   or `github`).
#' @param id Backend-specific identifier (filesystem path, GitHub tag, OSF id).
#' @param ... Additional arguments forwarded to backend-specific helpers (e.g.,
#'   authentication or API parameters).
#' @return Logical flag indicating whether the remote exists.
#' @keywords internal
#' @noRd
.remote_check_exists <- function(type,
                                 id,
                                 ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_check_exists_local(path = id),
    "github" = .remote_check_exists_github(
      tag = .remote_misc_github_tag_get(id),
      ...
    )
  )
}

# ========================
# check existence of remote_final
# ========================

#' @title Check whether a final remote destination exists
#' @description Determines whether the fully qualified destination (label +
#'   version + optional sub-path) already exists for the requested remote type.
#' @inheritParams .remote_final_get
#' @return Logical flag indicating whether the final remote exists.
#' @keywords internal
#' @noRd
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
      id, label, structure, path, path_append_label,
      version, empty
    ),
    "github" = .remote_final_check_exists_github(
      remote_pre, structure, label, version, empty
    )
  )
}

# ========================
# check existence of remote_final directly
# ========================

#' @title Check existence of a fully specified remote object
#' @description Skips remote composition logic and directly checks whether the
#'   supplied remote handle exists for the backend.
#' @inheritParams .remote_check_exists
#' @param remote Backend-specific remote handle produced by helper functions.
#' @return Logical flag indicating whether the handle targets an existing
#'   remote.
#' @keywords internal
#' @noRd
.remote_final_check_exists_direct <- function(type,
                                              remote,
                                              ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_check_exists_direct_local(remote),
    "github" = .remote_final_check_exists_direct_github(remote, ...)
  )
}


# ========================
# create remotes
# ========================

# return a character giving us the key information to
# find the remote again

#' @title Create a new remote resource
#' @description Creates the underlying remote container (local directory
#'   or GitHub release) needed for subsequent uploads.
#' @inheritParams .remote_check_exists
#' @param name Human readable name/title for the remote destination.
#' @param ... Additional backend-specific arguments (e.g., OSF parent, GitHub
#'   release settings).
#' @return Backend-specific identifier for the created remote.
#' @keywords internal
#' @noRd
.remote_create <- function(type,
                           id,
                           name,
                           ...) {
  .cli_debug(
    "Remote create: type={type}, id={id}"
  )

  switch(type,
    "local" = .remote_create_local(path = id),
    "github" = .remote_create_github(
      tag = .remote_misc_github_tag_get(id),
      ...
    )
  )
}

# ========================
# List all final remotes in a particular pre-remote
# ========================

#' @title List final remotes underneath a pre-remote
#' @description Enumerates all final remote destinations (e.g., assets inside a
#'   release or directories inside a destination path).
#' @inheritParams .remote_check_exists
#' @param remote_pre Backend-specific object representing the parent remote
#'   (e.g., base path, OSF node, GitHub release tag).
#' @return Character vector or list of backend objects describing available
#'   final remotes.
#' @keywords internal
#' @noRd
.remote_ls_final <- function(type,
                             remote_pre,
                             ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_ls_final_local(remote_pre),
    "github" = .remote_ls_final_github(remote_pre, ...)
  )
}

# ========================
# Get remote
# =======================

# gets remote in a way that we can work
# with it locally.
# For local and GitHub remotes, that simply
# means the path (local) and tag (GitHub).
# The main thing is that it must be an object
# we can use to interact with the remote,
# such as checking existence of files,
# uploading files, etc.

#' @title Retrieve a backend-specific remote handle
#' @description Resolves the identifier for a remote into an object that helper
#'   functions can use for file and metadata operations.
#' @inheritParams .remote_check_exists
#' @return Backend-specific remote handle (path, GitHub tag list, OSF object).
#' @keywords internal
#' @noRd

.remote_get <- function(type,
                        id) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_get_local(id = id),
    "github" = .remote_get_github(.remote_misc_github_tag_get(id)),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# =====================
# Get final remote
# =====================

#' @title Compose a final remote destination
#' @description Creates the backend-specific representation of a final remote
#'   destination (path, OSF object, GitHub asset name) using consistent rules
#'   for labels, versions, and optional custom paths.
#' @inheritParams .remote_check_exists
#' @param label Directory label whose contents will live at the remote.
#' @param structure Remote structure (`latest` or `archive`).
#' @param path Optional base path or prefix configured in `_projr.yml`.
#' @param path_append_label Logical flag indicating whether the label should be
#'   appended when composing hierarchical paths or filenames.
#' @param version Optional project version override; defaults to the current
#'   package version when `NULL`.
#' @param pre Logical flag requesting the parent of the final remote (e.g.
#'   directory containing label/version).
#' @param empty Logical flag indicating whether an "empty" variant (used for
#'   GitHub placeholder assets) should be produced.
#' @details
#' The final remote is created for hierarchical remotes (local, OSF),
#' but not for flat remotes (GitHub assets), as such a final
#' remote cannot be created empty.
#' @return Backend-specific remote handle suitable for existence checks and
#'   file operations. For local remotes, this is a path; for GitHub, a character
#'   vector with names `tag` and `fn` for the release tag and asset name, respectively.
#' @keywords internal
#' @noRd
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
      pre = pre,
      empty = empty
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

# =====================
# Get final remote
# =====================

#' @title Compose a final remote destination
#' @description Creates the backend-specific representation of a final remote
#'   destination (path, OSF object, GitHub asset name) using consistent rules
#'   for labels, versions, and optional custom paths.
#' @inheritParams .remote_check_exists
#' @param label Directory label whose contents will live at the remote.
#' @param structure Remote structure (`latest` or `archive`).
#' @param path Optional base path or prefix configured in `_projr.yml`.
#' @param path_append_label Logical flag indicating whether the label should be
#'   appended when composing hierarchical paths or filenames.
#' @param version Optional project version override; defaults to the current
#'   package version when `NULL`.
#' @param pre Logical flag requesting the parent of the final remote (e.g.
#'   directory containing label/version).
#' @param empty Logical flag indicating whether an "empty" variant (used for
#'   GitHub placeholder assets) should be produced.
#' @details
#' The final remote is created for hierarchical remotes (local),
#' but not for flat remotes (GitHub assets), as such a final
#' remote cannot be created empty.
#' @return Backend-specific remote handle suitable for existence checks and
#'   file operations. For local remotes, this is a path; for GitHub, a character
#'   vector with names `tag` and `fn` for the release tag and asset name, respectively.
#' @keywords internal
#' @noRd
.remote_final_empty_get <- function(type,
                                    id,
                                    label,
                                    structure,
                                    path = NULL,
                                    path_append_label = TRUE,
                                    version = NULL,
                                    ...) {
  .cli_debug(
    "remote_final_empty_get: type={type}, label={label}, structure={structure}, version={version}",
    type = type,
    label = label,
    structure = structure,
    version = version
  )

  # pre: "one up" from the final remote, e.g. the directory
  # above for hierarchical. Does not apply to flat.
  switch(type,
    "local" = .remote_final_empty_get_local(
      path = id,
      label = label,
      structure = structure,
      path_append_label = path_append_label,
      version = version
    ),
    "github" = .remote_final_empty_get_github(
      id = .remote_misc_github_tag_get(id),
      label = label,
      structure = structure,
      path = path,
      path_append_label = path_append_label,
      version = version,
      ...
    ),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# =====================
# Get final remote, if it exists
# =====================

#' @title Retrieve a final remote if it exists
#' @description Resolve and return the composed final remote handle only when
#'   the destination actually exists; otherwise return `NULL` to avoid
#'   creating handles that point at non-existent destinations. By default the
#'   function prefers the non-empty variant of a remote; when `empty` is
#'   supplied as `TRUE` or `FALSE` the function will only test that specific
#'   variant. When `empty` is `NULL` (the default) a non-empty remote is
#'   attempted first and, if missing, the empty variant is tried.
#' @inheritParams .remote_final_get
#' @param empty Logical (or `NULL`) indicating whether to test the empty
#'   placeholder variant of the final remote. When `NULL`, attempt non-empty
#'   variant then fallback to empty variant if not found. When `TRUE`/`FALSE`,
#'   test only that variant.
#' @return Backend-specific remote handle or `NULL` if the destination does not
#'   exist.
#' @details The `pre` parameter (inherited from `.remote_final_get`) requests
#'   the parent (pre-)remote rather than the final handle; this behavior is
#'   preserved by this helper. The `empty` flag is mainly relevant for flat
#'   remotes (GitHub asset names) and is ignored for hierarchical backends.
#' @keywords internal
#' @noRd
.remote_final_get_if_exists <- function(type,
                                        id,
                                        label,
                                        structure,
                                        path,
                                        path_append_label,
                                        version,
                                        pre,
                                        empty = NULL) {
  # always work with non-empty remote, and adapt it to be empty
  # remote if that is what is needed
  empty_init <- empty %||% FALSE
  remote_final_empty_init <- .remote_final_get_if_exists_impl(
    type, id, label, structure, path, path_append_label,
    version, pre, empty_init
  )
  remote_exists <- !is.null(remote_final_empty_init)
  empty_specified <- !is.null(empty)
  if (remote_exists || empty_specified) {
    return(remote_final_empty_init)
  }
  .remote_final_get_if_exists_impl(
    type, id, label, structure, path, path_append_label,
    version, pre, !empty_init
  )
}

#' @title Internal helper: Retrieve a final remote if it exists (impl)
#' @description Low-level implementation used by
#'   `.remote_final_get_if_exists()`.
#'   Performs a direct existence check for the composed final remote and, when
#'   present, returns the backend-specific final remote handle created by
#'   `.remote_final_get()`. Returns `NULL` when the destination is absent.
#' @inheritParams .remote_final_get
#' @param empty Logical indicating whether to test the "empty" placeholder
#'   variant (`-empty`) of the final remote; defaults to `FALSE` for the
#'   implementation. This field is mainly relevant for flat remotes (GitHub
#'   assets) and is ignored for hierarchical backends.
#' @return Backend-specific remote handle or `NULL` if the destination does not
#'   exist.
#' @details This implementation performs the actual existence check and
#'   composition; the public wrapper `.remote_final_get_if_exists()` implements
#'   fallback logic when `empty = NULL` (attempt non-empty then empty).
#' @keywords internal
#' @noRd
.remote_final_get_if_exists_impl <- function(type,
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

#' @title Build relative path or asset name for a remote
#' @description Helper that composes the relative path/filename fragment used by
#'   backend-specific implementations, delegating to hierarchy or flat helpers as
#'   needed.
#' @param path Optional user-specified path prefix.
#' @param path_append_label Logical flag indicating whether to append the label
#'   when composing the path.
#' @param label Directory label associated with the remote.
#' @param structure Remote structure (`latest` or `archive`).
#' @param type Remote type identifier.
#' @param version Optional version override.
#' @param pre Logical flag requesting the parent directory/asset prefix rather
#'   than the final destination.
#' @param empty Logical flag indicating whether the empty placeholder variant is
#'   required (GitHub only).
#' @return Character scalar representing the relative path/asset fragment.
#' @keywords internal
#' @noRd
.remote_get_path_rel <- function(path,
                                 path_append_label,
                                 label,
                                 structure,
                                 type,
                                 version,
                                 pre,
                                 empty) {
  switch(type,
    "local" = .remote_get_path_rel_hierarchy(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version,
      pre = pre,
      empty = empty
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
#' @title Compose relative paths for hierarchical remotes
#' @description Implements the label/version stacking rules for hierarchical
#'   remotes (local directories and OSF objects).
#' @inheritParams .remote_get_path_rel
#' @return Character relative path built according to structure/pre options.
#' @keywords internal
#' @noRd
.remote_get_path_rel_hierarchy <- function(path,
                                           path_append_label,
                                           label,
                                           structure,
                                           version,
                                           pre,
                                           empty) {
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
  if (!is.null(empty) && isTRUE(empty)) {
    args_list[[length(args_list)]] <- paste0(
      args_list[[length(args_list)]], "-empty"
    )
  }

  do.call(file.path, args_list)
}


# flat remotes
#' @title Compose relative asset names for flat remotes
#' @description Handles naming for flat remotes (GitHub assets) including label
#'   suffixes, version postfixes, and optional "-empty" variants.
#' @inheritParams .remote_get_path_rel
#' @return Character scalar describing the asset filename prefix.
#' @keywords internal
#' @noRd
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

#' @title Remove empty remote destinations
#' @description Deletes backend-specific remotes when they are empty and no
#'   longer needed (e.g., cleanup for cache directories).
#' @inheritParams .remote_final_get
#' @param remote Backend-specific remote handle produced by
#'   `.remote_final_get()`.
#' @return Invisibly returns `TRUE` when removal succeeds.
#' @keywords internal
#' @noRd
.remote_final_rm_if_empty <- function(type,
                                      remote,
                                      ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_rm_if_empty_local(remote),
    "github" = .remote_final_rm_if_empty_github(remote, ...)
  )
}

# ========================
# Delete a remote
# ========================

#' @title Delete a remote resource
#' @description Removes the underlying remote container (local directory
#'  or GitHub release) and all its contents.
#' @inheritParams .remote_check_exists
#' @param remote Backend-specific remote handle produced by `.remote_get()`.
#' @return Invisibly returns `TRUE` when removal succeeds.
#' @keywords internal
#' @noRd
.remote_rm <- function(type,
                       remote,
                       ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_rm_local(remote),
    "github" = .remote_rm_github(remote, ...)
  )
}

# ========================
# Delete a final remote
# ========================

.remote_final_rm <- function(type,
                             remote,
                             ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_rm_local(
      remote = remote
    ),
    "github" = .remote_final_rm_github(
      remote = remote, ...
    )
  )
}


# =======================
# Get information about the final remote
# =======================

#' @title Gather metadata about a final remote
#' @description Retrieves backend-specific information (currently GitHub asset
#'   metadata) about a final remote destination.
#' @inheritParams .remote_final_get
#' @param remote_final Backend-specific object returned by
#'   `.remote_final_get()`.
#' @return Metadata list or `NULL` when the backend does not expose information.
#' @keywords internal
#' @noRd
.remote_final_get_info <- function(type,
                                   remote_final,
                                   ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = NULL,
    "github" = .remote_final_get_info_github(remote_final, ...)
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
#' @title Empty a final remote destination
#' @description Removes all contents from the supplied remote handle. For flat
#'   remotes (GitHub assets) this deletes the asset entirely.
#' @inheritParams .remote_final_rm_if_empty
#' @return Invisibly returns `TRUE` after the remote is emptied.
#' @keywords internal
#' @noRd
.remote_final_empty <- function(type,
                                remote,
                                ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .remote_final_empty_local(remote),
    "github" = .remote_final_empty_github(
      remote,
      ...
    )
  )
}

# ========================
# Hash a particular remote
# ========================

#' @title Hash files inside a remote destination
#' @description Calculates file hashes for a remote handle and formats them for
#'   manifest comparisons.
#' @inheritParams .remote_final_get
#' @param remote_final Backend-specific handle returned by
#'   `.remote_final_get()`.
#' @param version Version string used when labelling hashes.
#' @return Manifest-like tibble containing filename/hash pairs.
#' @keywords internal
#' @noRd
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
#' @title Download all files from a remote destination
#' @description Fetches every file from the supplied remote handle and stores
#'   them in a local directory, creating it if needed.
#' @inheritParams .remote_final_get
#' @param remote Backend-specific remote handle (final destination).
#' @param path_dir_save_local Local directory where files should be written.
#' @return Absolute path to the directory containing downloaded files.
#' @keywords internal
#' @noRd
.remote_file_get_all <- function(type,
                                 remote,
                                 path_dir_save_local,
                                 ...) {
  .assert_string(path_dir_save_local, TRUE)
  .assert_in(type, .opt_remote_get_type(), TRUE)
  .dir_create(path_dir_save_local)
  switch(type,
    "local" = .remote_file_get_all_local(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    ),
    "github" = .remote_file_get_all_github(
      remote = remote,
      path_dir_save_local = path_dir_save_local,
      ...
    )
  )
}

# ========================
# Download a single file
# ========================

#' @title Download a single file from a remote
#' @description Retrieves one file from the remote and saves it inside the
#'   provided directory.
#' @inheritParams .remote_file_get_all
#' @param fn Filename (relative to the remote) to download.
#' @return Absolute path to the saved file.
#' @keywords internal
#' @noRd
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

#' @title List files contained in a remote
#' @description Enumerates files (or asset entries) stored at the remote and
#'   logs the count for debugging purposes.
#' @inheritParams .remote_file_get_all
#' @return Character vector (or list for complex backends) of filenames.
#' @keywords internal
#' @noRd
.remote_file_ls <- function(type,
                            remote) {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  result <- switch(type,
    "local" = .remote_file_ls_local(remote),
    "github" = .remote_file_ls_github(remote)
  )

  .cli_debug(
    "Remote file list: type={type}, found {length(result)} file(s)"
  )

  result
}

# ========================
# Delete individual files
# ========================

# pre-specified files
#' @title Remove specific files from a remote
#' @description Deletes the requested filenames from the remote destination,
#'   logging the count removed.
#' @inheritParams .remote_file_ls
#' @param fn Character vector of filenames to delete.
#' @return Invisibly returns `TRUE` after deletion completes.
#' @keywords internal
#' @noRd
.remote_file_rm <- function(type,
                            fn,
                            remote) {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  .cli_debug(
    "Remote file remove: type={type}, removing {length(fn)} file(s)"
  )

  switch(type,
    "local" = .remote_file_rm_local(fn = fn, remote = remote),
    "github" = .remote_file_rm_github(fn = fn, remote = remote)
  )
}

# ========================
# Add individual files
# ========================

#' @title Upload files to a remote destination
#' @description Adds specific files from a local directory to the remote
#'   destination, logging how many are uploaded.
#' @inheritParams .remote_file_rm
#' @param path_dir_local Local directory containing the files listed in `fn`.
#' @return Invisibly returns `TRUE` after uploads finish.
#' @keywords internal
#' @noRd
.remote_file_add <- function(type,
                             remote,
                             path_dir_local,
                             fn,
                             ...) {
  .assert_in(type, .opt_remote_get_type(), TRUE)

  .cli_debug(
    "Remote file add: type={type}, adding {num_files} file(s) from '{path}' to remote='{remote}'",
    type = type,
    num_files = length(fn),
    path = path_dir_local,
    remote = remote
  )

  switch(type,
    "local" = .remote_file_add_local(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    ),
    "github" = .remote_file_add_github(
      fn = fn,
      path_dir_local = path_dir_local,
      remote = remote,
      ...
    )
  )
}
