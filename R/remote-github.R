# GitHub Release API - Compatibility Layer
# ==========================================
# This file provides backwards-compatible function names that delegate to
# the actual implementations in remote-github-httr.R
#
# New naming convention:
# - .gh_release_*_httr() for direct httr implementations
# - .gh_release_*_pb() for piggyback implementations (in remote-github-piggyback.R)
# - Old .github_* names maintained here for backwards compatibility

# ========================
# check existence
# ========================
.remote_check_exists_github <- function(tag,
                                        api_url = NULL,
                                        token   = NULL,
                                        pause_second = 2,
                                        output_level = "std",
                                        max_attempts = 3,
                                        max_delay = 300,
                                        max_total_time = 600) {
  .assert_string(tag, TRUE)

  repo <- tryCatch(
    .gh_repo_get(),
    error = function(e) NULL
  )

  if (is.null(repo)) {
    .cli_debug(
      "GitHub release: Could not get repo info when checking existence of GitHub release '{tag}'", # nolint
      output_level = output_level
    )
    stop(call. = FALSE)
  }
  .retry_with_backoff(
    fn = function() {
      .remote_check_exists_github_httr(
        repo   = repo,
        tag    = tag,
        api_url = api_url,
        token  = token
      )
    },
    max_attempts = max_attempts,
    max_delay = max_delay,
    initial_delay  = pause_second,
    operation_name = paste0(
      "check if release '", tag, "' exists in repo '", repo, "'"
    ),
    output_level   = output_level,
    check_success  = function(x) TRUE
  )
}

# ========================
# check existence of remote_final
# ========================

.remote_final_check_exists_github <- function(remote_pre,
                                              structure,
                                              label,
                                              version,
                                              empty,
                                              api_url = NULL,
                                              token   = NULL) {
  .assert_attr(remote_pre, "names")
  .assert_has(names(remote_pre), c("tag"))
  if (!.remote_check_exists("github", remote_pre[["tag"]], max_attempts = 2)) {
    return(FALSE)
  }
  # if there's an error for some reason, assume it's not there
  fn <- if (structure == "archive") {
    paste0(label, "-", version, ".zip")
  } else {
    paste0(label, ".zip")
  }
  fn <- if (empty) {
    gsub("\\.zip$", "-empty.zip", fn)
  } else {
    fn
  }
  remote <- remote_pre
  remote[["fn"]] <- fn
  tryCatch(
    .remote_final_check_exists_direct(
      "github",
      remote = remote,
      api_url = api_url,
      token  = token
    ),
    error = function(e) {
      .cli_debug(
        "GitHub release: Error checking for asset '{fn}' in release '{tag}': {condition}. Assuming asset does not exist.", # nolint
        tag = remote_pre[["tag"]],
        condition = e$message
      )
      FALSE
    }
  )
}

.remote_final_check_exists_direct_github <- function(remote,
                                                     api_url = NULL,
                                                     token   = NULL) {
  .remote_final_check_exists_github_httr(
    repo = .gh_repo_get(),
    tag  = remote[["tag"]],
    asset = remote[["fn"]],
    api_url = api_url,
    token  = token
  )
}

# ========================
# create release
# ========================

.remote_create_github <- function(tag,
                                  description   = NULL,
                                  pause_second  = 3,
                                  max_attempts  = 1,
                                  max_delay     = 300,
                                  max_total_time = 600,
                                  output_level  = "std",
                                  ensure_exists = FALSE) {
  .assert_string(tag, TRUE)
  .assert_string(description)
  .assert_number(pause_second, TRUE)

  if (is.null(description)) {
    description <- "Release created automatically by `projr`"
  }

  # Single attempt to create the release
  result <- .remote_create_github_attempt(
    tag          = tag,
    description  = description,
    output_level = output_level
  )

  if (.is_try_error(result)) {
    .cli_debug(
      "GitHub release: Failed to create GitHub release '{tag}'",
      output_level = "debug"
    )
    return(invisible(character()))
  }

  # Only confirm existence if requested
  if (isTRUE(ensure_exists)) {
    .cli_debug(
      "GitHub release: Confirming existence of GitHub release '{tag}'",
      output_level = output_level
    )

    # Approximate previous 300s wait, but via retry helper
    remote_exists <- .remote_check_exists(
      "github",
      id = tag,
      pause_second  = pause_second,
      max_attempts  = max_attempts,
      max_delay     = max_delay,
      max_total_time = max_total_time,
      output_level  = output_level
    )

    if (isTRUE(remote_exists)) {
      .cli_debug(
        "GitHub release: Confirmed existence of GitHub release '{tag}'",
        output_level = output_level
      )
    } else {
      .cli_debug(
        "GitHub release: Could not confirm existence of GitHub release '{tag}' after waiting", # nolint
        output_level = output_level
      )
    }
  }

  invisible(tag)
}

.remote_create_github_attempt <- function(tag,
                                          description,
                                          output_level = "std") {
  repo <- .gh_repo_get()

  result <- try(
    .gh_release_create_httr(
      repo = repo,
      tag = tag,
      description = description
    ),
    silent = TRUE
  )

  if (inherits(result, "try-error")) {
    error_msg <- attr(result, "condition")$message
    .cli_debug(
      "GitHub release: pb_release_create() failed for tag '{tag}': {error_msg}",
      output_level = output_level
    )
  }

  result
}

# =======================
# list releases
# =======================

.remote_ls_final_github <- function(remote_pre) {
  .assert_given_full(remote_pre)
  .remote_ls_final_github_httr(
    repo = .gh_repo_get(),
    tag  = remote_pre[["tag"]]
  )
}

# ========================
# Get remote
# =======================

# github
.remote_get_github <- function(id) {
  .assert_string(id, TRUE)
  c("tag" = id)
}

# ========================
# Get final remote
# =======================

.remote_final_get_github <- function(id,
                                     path,
                                     path_append_label,
                                     label,
                                     structure,
                                     version = NULL,
                                     pre,
                                     empty) {
  .assert_string(id, TRUE)
  .assert_in(label, .opt_dir_get_label_send(NULL), TRUE)
  tag <- .remote_misc_github_tag_get(id)

  # For pre=TRUE, just return tag
  if (pre) {
    return(c("tag" = tag))
  }

  fn <- .remote_get_path_rel(
    type = "github",
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    version = version,
    empty = empty
  )

  # everything uploaded to a gh release
  # is a single file, and all other remotes
  # are just directories where files can
  # be uploaded to (and possibly folders,
  # but github releases don't do that), so
  # the remote for a github release
  # is the tag plus the file name
  c("tag" = tag, "fn" = fn)
}

# =====================
# Get relative paths
# =====================

.remote_get_path_rel_github <- function(path,
                                        path_append_label,
                                        label,
                                        structure,
                                        version,
                                        empty) {
  # keep it as NULL this way if it's already
  # NULL (otherwise it's character(),
  # which triggers an error when checking for a string later)
  if (!is.null(path)) {
    path <- path |> (\(x) gsub(pattern = "\\.zip$", replacement = "", x))()
  }
  paste0(
    .remote_get_path_rel_flat(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version,
      empty = empty
    ),
    ".zip"
  )
}

# ========================
# Delete an unused empty remote directory
# ========================

# github
.remote_final_rm_if_empty_github <- function() {
  # never any need to, as the release is only
  # created if it's to be uploaded to
  invisible(FALSE)
}

# ========================
# Delete a final remote
# ========================

.remote_final_rm_github <- function(remote,
                                    output_level = "std",
                                    api_url = NULL,
                                    token   = NULL) {
  .assert_given_full(remote)
  tag <- remote[["tag"]]
  fn  <- remote[["fn"]]

  if (!.remote_check_exists("github", tag, max_attempts = 2)) {
    .cli_debug(
      "GitHub release: Release '{tag}' does not exist, nothing to delete",
      tag = tag,
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  if (!.remote_final_check_exists_direct(
    "github",
    remote = remote,
    api_url = api_url,
    token  = token
  )) {
    .cli_debug(
      "GitHub release: Asset '{fn}' not found in release '{tag}', so no need to delete.", # nolint
      fn = fn,
      tag = tag,
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  .remote_final_rm_github_httr(
    repo = .gh_repo_get(),
    tag  = tag,
    fn   = fn,
    api_url = api_url,
    token  = token
  )
  invisible(TRUE)
}

# =======================
# Get information about the remote
# =======================

.remote_final_get_info_github <- function(remote_final,
                                          api_url = NULL,
                                          token   = NULL) {
  .remote_final_get_info_github_httr(
    repo = .gh_repo_get(),
    tag  = remote_final[["tag"]],
    asset_name = remote_final[["fn"]],
    api_url = api_url,
    token  = token
  )
}

# =======================
# Empty final remote
# ======================

# deletes the release asset

# github
.remote_final_empty_github <- function(remote,
                                       output_level = "std",
                                       api_url = NULL,
                                       token   = NULL) {
  # here, if remote specifies the file, it will only remove
  # that file, but if remote doesn't, then
  # it removes every file.
  # I think this is essentially the same as OSF and local,
  # as there you would specify the remote and then upload
  # all the directories to that remote as directories.
  # here those directories are uploaded as files,
  # which is different.
  .assert_chr_mid(remote, TRUE)
  tag <- .remote_misc_github_tag_get(remote)
  remote[["tag"]] <- tag
  .assert_chr_mid(tag, TRUE)

  .cli_debug(
    "GitHub release: Checking if tag '{tag}' exists for deletion",
    output_level = output_level
  )

  if (!.remote_check_exists("github", tag, max_attempts = 2)) {
    .cli_debug(
      "GitHub release: Tag '{tag}' does not exist, nothing to delete",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  if (!.remote_final_check_exists_direct(
    "github",
    remote = remote,
    api_url = api_url,
    token  = token
  )) {
    fn <- remote[["fn"]]
    .cli_debug(
      "GitHub release: Asset '{fn}' not found in tag '{tag}', so no need to delete.", # nolint
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  .remote_final_empty_github_httr(
    repo = .gh_repo_get(),
    tag = tag,
    fn = remote[["fn"]],
    api_url = api_url,
    token = token
  )
  invisible(TRUE)
}

# ========================
# Download all files
# ========================

.remote_file_get_all_github <- function(remote,
                                        path_dir_save_local,
                                        output_level = "std",
                                        api_url = NULL,
                                        token = NULL,
                                        overwrite = TRUE) {
  .assert_given_full(remote)

  if (!.remote_check_exists("github", remote[["tag"]])) {
    return(invisible(path_dir_save_local))
  }
  dir_save_zip <- .dir_get_tmp_random_path()
  path_vec_zip <- .remote_file_get_all_github_httr(
    repo = .gh_repo_get(),
    tag  = remote[["tag"]],
    fn = remote[["fn"]],
    dest_dir = dir_save_zip,
    output_level = output_level,
    overwrite = overwrite,
    api_url = api_url,
    token = token
  )

  for (path_zip in path_vec_zip) {
    # unzip
    if (file.exists(path_zip)) {
      .cli_debug(
        "GitHub release: Unzipping {basename(path_zip)} to {path_dir_save_local}",
        output_level = output_level
      )
      tryCatch({
        utils::unzip(path_zip, exdir = path_dir_save_local)
        .cli_debug(
          "GitHub release: Successfully unzipped {basename(path_zip)}",
          output_level = output_level
        )
      }, error = function(e) {
        .cli_debug(
          "GitHub release: Failed to unzip {basename(path_zip)}: {e$message}",
          output_level = output_level
        )
      })
      file.remove(path_zip)
    } else {
      .cli_debug(
        "GitHub release: Zip file {basename(path_zip)} does not exist, cannot unzip.",
        output_level = output_level
      )
    }
  }
  unlink(dir_save_zip, recursive = TRUE, force = TRUE)
  invisible(path_dir_save_local)
}

# ========================
# Download a single file
# ========================

.remote_file_get_github <- function(remote,
                                    fn,
                                    path_dir_save_local,
                                    output_level = "std"
                                    ) {
  if (!.remote_check_exists("github", remote[["tag"]])) {
    return(character(0L))
  }
  # I think this is to handle VERSION and manifest.csv entris
  path_dir_save_tmp <- .dir_get_tmp_random_path()
  .remote_file_get_all_github(
    remote = remote,
    path_dir_save_local = path_dir_save_tmp,
    output_level = output_level
  )
  path_fn <- file.path(path_dir_save_tmp, fn)
  if (!file.exists(path_fn)) {
    .cli_debug(
      "GitHub release: File '{fn}' does not exist in release '{tag}'",
      fn = fn,
      tag = remote[["tag"]],
      output_level = output_level
    )
    unlink(path_dir_save_tmp, recursive = TRUE, force = TRUE)
    return(character(0L))
  }
  if (!dir.exists(path_dir_save_local)) {
    dir.create(path_dir_save_local, recursive = TRUE, showWarnings = FALSE)
  }
  path_to <- file.path(path_dir_save_local, basename(fn))
  .cli_debug(
    "GitHub release: Moving file '{fn}' to '{path_to}'",
    fn = fn,
    path_to = path_dir_save_local,
    output_level = output_level
  )
  invisible(file.rename(from = path_fn, to = path_to))
  unlink(path_dir_save_tmp, recursive = TRUE, force = TRUE)
  invisible(path_to)
}

# ========================
# List all contents of a release asset
# ========================

.remote_file_ls_github <- function(remote) {
  path_dir_save_local <- .dir_create_tmp_random()
  .assert_given_full(remote)
  .assert_has(names(remote), c("tag", "fn"))
  .assert_string(remote[["fn"]])
  .assert_string(remote[["tag"]])
  .remote_file_get_all(
    "github",
    remote = remote, path_dir_save_local = path_dir_save_local
  )
  fn_vec <- .remote_file_ls("local", path_dir_save_local)
  unlink(path_dir_save_local, recursive = TRUE)
  fn_vec
}

# ========================
# Delete individual files
# ========================

.remote_file_rm_github <- function(fn,
                                   remote) {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_given_full(remote)
  path_dir_save_local <- .dir_create_tmp_random()
  .remote_file_get_all(
    "github",
    remote = remote,
    path_dir_save_local = path_dir_save_local
  )
  fn_vec <- .remote_file_ls("local", path_dir_save_local)
  fn_vec_to_rm <- fn_vec[fn_vec %in% fn]
  if (length(fn_vec_to_rm) == 0L) {
    return(invisible(FALSE))
  }

  fn_vec_to_upload <- setdiff(fn_vec, fn_vec_to_rm)
  .remote_final_empty(
    "github",
    remote = remote
  )
  if (!.is_len_0(fn_vec_to_upload)) {
    .remote_file_rm(
      "local",
      fn = fn_vec_to_rm,
      remote = path_dir_save_local
    )
    .remote_file_add(
      "github",
      fn = fn_vec_to_upload,
      path_dir_local = path_dir_save_local,
      remote = remote
    )
  }
  unlink(path_dir_save_local, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Add individual files
# ========================

.remote_file_add_github <- function(fn,
                                    path_dir_local,
                                    remote,
                                    output_level = "std",
                                    
                                    api_url = NULL,
                                    token   = NULL) {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_given_full(remote)
  label <- gsub("\\.zip", "", remote[["fn"]])
  if (length(fn) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)

  path_zip <- .zip_file(
    fn_rel = fn,
    path_dir_fn_rel = path_dir_local,
    fn_rel_zip = remote[["fn"]]
  )
  if (length(path_zip) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  tag <- .remote_misc_github_tag_format(remote[["tag"]])

  # Just check that the release exists
  if (!.remote_check_exists("github", tag)) {
    stop(paste0(
      "GitHub release '", tag, "' does not exist. "
    ))
  }

  # create a folder that has all the original
  # files, plus the new ones (that overwrite
  # any old ones)
  path_dir_tmp_save <- .dir_create_tmp_random()
  on.exit(
    try(unlink(path_dir_tmp_save, recursive = TRUE), silent = TRUE),
    add = TRUE
  )
  remote_exists <- .remote_final_check_exists_direct(
    "github",
    remote = remote, api_url = api_url, token  = token
  )
  if (remote_exists) {
    .remote_file_get_all("github", remote, path_dir_tmp_save)
  }

  for (fn_curr in fn) {
    path_fn_curr <- file.path(path_dir_local, fn_curr)
    path_fn_curr_to <- file.path(path_dir_tmp_save, fn_curr)
    if (!dir.exists(dirname(path_fn_curr_to))) {
      dir.create(dirname(path_fn_curr_to), recursive = TRUE)
    }
    fs::file_copy(
      path = path_fn_curr,
      new_path   = path_fn_curr_to,
      overwrite = TRUE
    )
  }

  path_zip <- .zip_dir(
    path_dir = path_dir_tmp_save,
    path_zip = file.path(tempdir(), remote[["fn"]])
  )

  .gh_release_asset_upload_httr(
    repo = .gh_repo_get(),
    tag  = tag,
    file_path = path_zip,
    asset_name = remote[["fn"]],
    overwrite = TRUE
  )
}

# ======================================
# Tags
# ======================================

.remote_misc_github_tag_get <- function(x) {
  .assert_given_full(x)
  if (!"tag" %in% names(x)) {
    .assert_string(x)
    tag <- x
  } else {
    tag <- x[["tag"]]
  }
  tag <- gsub("\\s", "-", tag)
  if (tag == "@version") .version_get_v() else tag
}

.remote_misc_github_tag_format <- function(tag) {
  tag <- switch(tag,
    `@version` = .version_get_v(),
    tag
  )
  tag <- gsub("^ +", "", tag)
  tag <- gsub(" +$", "", tag)
  gsub(" ", "-", tag)
}

