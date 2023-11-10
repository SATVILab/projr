# ========================
# create remotes
# ========================

# return a character giving us the key information to
# find the remote again

.projr_remote_create <- function(remote_type,
                                 remote_id,
                                 ...) {
  switch(remote_type,
    "local" = .projr_remote_create_local(path = remote_id),
    "osf" = .projr_remote_create_osf(title = remote_id, ...),
    "github" = .projr_remote_create_github(tag = remote_id, ...)
  )
}

# local
.projr_remote_create_local <- function(path) {
  .projr_remote_check_base(remote_type = "local", path = path)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}

# osf
.projr_remote_create_osf <- function(title,
                                     id = NULL,
                                     parent_id = NULL,
                                     category = NULL,
                                     body = NULL,
                                     public = FALSE) {
  .projr_dep_install("osfr")
  category <- .projr_remote_complete_osf_category(
    category = category
  )
  .projr_remote_check_base_osf(
    title = title,
    id = id,
    parent_id = parent_id,
    category = category,
    body = body,
    public = public,
    parent_title = NULL,
    path = NULL,
    path_append_label = NULL
  )
  .projr_remote_create_osf_node(
    title = title,
    parent_id = parent_id,
    category = category,
    body = body,
    public = public
  )
}

.projr_remote_create_osf_node <- function(title,
                                          parent_id,
                                          category,
                                          body,
                                          public) {
  switch(category,
    "project" = .projr_remote_create_osf_project(
      title = title,
      body = body,
      public = public
    ),
    .projr_remote_create_osf_component(
      title = title,
      parent_id = parent_id,
      category = category,
      body = body,
      public = public
    )
  )
}

.projr_remote_create_osf_project <- function(title,
                                             body,
                                             public) {
  id <- try(osfr::osf_create_project(
    title = title,
    description = body,
    public = public,
    category = "project"
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}

.projr_remote_create_osf_component <- function(title,
                                               parent_id,
                                               category,
                                               body,
                                               public) {
  id <- try(osfr::osf_create_component(
    x = .projr_remote_get_osf(id = parent_id),
    title = title,
    description = body,
    public = public,
    category = category
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}


# github
.projr_remote_create_github <- function(tag,
                                        body = NULL,
                                        pause_second = 3) {
  .projr_dep_install("piggyback")
  pb_release_create <- projr_remote_create_github_attempt(
    tag = tag, body = body
  )
  if (!inherits(pb_release_create, "try-error")) {
    return(invisible(tag))
  }
  Sys.sleep(pause_second)
  pb_release_create <- projr_remote_create_github_attempt(
    tag = tag, body = body
  )
  if (!inherits(pb_release_create, "try-error")) {
    return(invisible(tag))
  }
  invisible(character())
}

projr_remote_create_github_attempt <- function(tag, body) {
  try(suppressWarnings(suppressMessages(
    piggyback::pb_release_create(tag = tag, body = body)
  )))
}

# ========================
# check existence
# ========================

.projr_remote_check_exists <- function(remote_type,
                                       remote_id) {
  switch(remote_type,
    "local" = .projr_remote_check_exists_local(path = remote_id),
    "osf" = .projr_remote_check_exists_osf(id = remote_id),
    "github" = .projr_remote_check_exists_github(tag = remote_id)
  )
}

# local
.projr_remote_check_exists_local <- function(path) {
  dir.exists(path)
}

# osf
.projr_remote_check_exists_osf <- function(id) {
  !inherits(
    .projr_remote_get(remote_type = "osf", id = id),
    "try-error"
  )
}

# github
.projr_remote_check_exists_github <- function(tag) {
  release_tbl <- .projr_pb_get_release_tbl()
  if (inherits(release_tbl, "try-error")) {
    stop("Could not get GitHub release table")
  }
  "tag" %in% release_tbl[["release_name"]]
}

# =====================
# get just the remote itself,
# nothing more specific like sub-directories (OSF/local)
# or file names to upload to (GitHub).
# does not create remotes either.
# =====================

.projr_remote_get <- function(remote_type,
                              id) {
  switch(remote_type,
    "local" = .projr_remote_get_local(path = id),
    "osf" = .projr_remote_get_osf(id = id),
    "github" = .projr_remote_get_github(id),
    stop(paste0("remote_type '", remote_type, "' not recognized"))
  )
}

# local
.projr_remote_get_local <- function(id) {
  id
}

# osf
.projr_remote_get_osf <- function(id) {
  tryCatch(
    osfr::osf_retrieve_node(paste0("https://osf.io/", id)),
    error = function(e) {
      stop(paste0(
        "Could not retrieve OSF node (project/component):", id
      ))
    }
  )
}

# github
.projr_remote_get_github <- function(id) {
  c("tag" = id)
}

# =====================
# get final remote, including sub-directory (OSF/local)
# and files to upload to (GitHub)
# =====================

# wrapper functions to get the "final" remote,
# i.e. including any sub-directories for
# specifying the version and so on for
# hierarchical remotes (like "local" and "OSF")
.projr_remote_get_final <- function(remote_type,
                                    remote_name,
                                    id,
                                    path,
                                    path_append_label,
                                    label,
                                    remote_structure) {
  switch(remote_type,
    "local" = .projr_remote_get_final_local(
      path = path,
      path_append_label = path_append_label,
      label = label,
      remote_structure = remote_structure
    ),
    "osf" = .projr_remote_get_final_osf(
      id = id,
      path = path,
      path_append_label = path_append_label,
      label = label,
      remote_structure = remote_structure
    ),
    "github" = .projr_remote_get_final_github(
      remote_name = remote_name, label = label
    ),
    stop(paste0("remote_type '", remote_type, "' not recognized"))
  )
}

# ---------------------
# local
# ---------------------

.projr_remote_get_final_local <- function(path,
                                          path_append_label,
                                          label,
                                          remote_structure) {
  # the local destination is just the
  # local directory where files are get, so
  # it is just the path.
  # note that `output_run` does not matter,
  # as this is a "remote" in the sense of something
  # interacted without either before or after the build process,
  # unlike the direoctires specified in
  # _projr.yml[["directories"]]`
  .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure,
    remote_type = "local"
  )
}

# ---------------------
# osf
# ---------------------

.projr_remote_get_final_osf <- function(id,
                                        path,
                                        path_append_label,
                                        label,
                                        remote_structure) {
  path_rel <- .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure
  )
  osf_tbl <- .projr_remote_get(id = id, remote_type = "osf")
  if (length(path_rel) > 0L) {
    osf_tbl <- osfr::osf_mkdir(osf_tbl, path_rel)
  }
  osf_tbl
}

# ---------------------
# github
# ---------------------

.projr_remote_get_final_github <- function(remote_name, label) {
  # everything uploaded to a gh release
  # is a single file, and all other remotes
  # are just directories where files can
  # be uploaded to (and possibly folders,
  # but giithub releases don't do that), so
  # the remote for github release is just the
  # name of the tag
  # (given generally as the remote_name here)
  c("tag" = remote_name, "fn" = paste0(label, ".zip"))
}



# =====================
# get relative paths
# =====================

# path_rel
# ---------------------

# overall function
.projr_remote_get_path_rel <- function(path,
                                       path_append_label,
                                       label,
                                       remote_structure,
                                       remote_type) {
  fn <- switch(remote_type,
    "osf" = , # same as local
    "local" = .projr_remote_get_path_rel_hierarchy,
    "github" = .projr_remote_get_path_rel_flat
  )
  do.call(fn, list(
    path = path,
    path_append_label = path_append_label,
    label = label,
    remote_structure = remote_structure
  ))
}

# hierarchical remotes
.projr_remote_get_path_rel_hierarchy <- function(path,
                                                 path_append_label,
                                                 label,
                                                 remote_structure) {
  args_list <- path
  if (path_append_label) {
    args_list <- args_list |> append(label)
  }
  if (remote_structure == "version") {
    args_list <- args_list |> append(projr_version_get())
  }
  do.call(file.path, args_list)
}

# flat remotes
.projr_remote_get_path_rel_flat <- function(...) character()


# ========================
# delete an unused empty remote directory
# ========================

.projr_remote_rm_final_empty <- function(remote_final,
                                         remote_type,
                                         remote_structure) {
  if (!remote_structure == "version") {
    return(invisible(FALSE))
  }
  switch(remote_type,
    "local" = .projr_remote_rm_final_empty_local(
      remote_final = remote_final
    ),
    "osf" = .projr_remote_rm_final_empty_osf(
      remote_final = remote_final
    ),
    "github" = .projr_remote_rm_final_empty_github(
      remote_final = remote_final
    )
  )
}

# local
.projr_remote_rm_final_empty_local <- function(remote_final) {
  if (!dir.exists(remote_final)) {
    return(invisible(FALSE))
  }
  unlink(remote_final, recursive = TRUE)
}

# osf
.projr_remote_rm_final_empty_osf <- function(remote_final) {
  if (!inherits(remote_final, "osf_tbl_file")) {
    return(invisible(FALSE))
  }
  if (!nrow(osfr::osf_ls_files(remote_final)) > 0L) {
    return(invisible(FALSE))
  }
  osfr::osf_rm(x = remote_final, check = FALSE)
}

# github
.projr_remote_rm_final_empty_github <- function(remote_final) {
  # never any need to, as the release is only
  # created if it's to be uploaded to
  invisible(FALSE)
}

# ========================
# delete all contents of a remote
# ========================

# this is different to the above, as it
# only deletes the contents and not
# the remote directory itself (also,
# the above will only delete the remote if
# it's actually got nothing, so it's very different)
.projr_remote_file_rm_all <- function(remote,
                                      remote_type) {
  switch(remote_type,
    "local" = .projr_remote_file_rm_all_local(remote = remote),
    "osf" = .projr_remote_file_rm_all_osf(remote = remote),
    "github" = .projr_remote_file_rm_all_github(remote = remote)
  )
}

# local
.projr_remote_file_rm_all_local <- function(remote) {
  if (!dir.exists(remote)) {
    return(invisible(FALSE))
  }
  dir_vec <- list.dirs(remote, recursive = TRUE)
  if (length(dir_vec) > 0) {
    unlink(dir_vec, recursive = TRUE)
  }
  fn_vec <- list.files(remote, full.names = TRUE, all.files = TRUE)
  if (length(fn_vec) > 0) {
    file.remove(file.path(remote, fn_vec))
  }
  invisible(TRUE)
}

# osf
.projr_remote_file_rm_all_osf <- function(remote) {
  osf_tbl_file <- remote |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_rm(x = osf_tbl_file[i, ], recurse = TRUE, check = FALSE)
  }
  invisible(TRUE)
}

# github
.projr_remote_file_rm_all_github <- function(remote) {
  # the `piggyback::pb_delete` function
  # deletes all files by default and
  # pb_release_delete deletes the release itself,
  # so this should still just empty it
  piggyback::pb_delete(tag = remote)
}

# ========================
# download all contents of a remote
# ========================

# return the path to which it's downloaded
.projr_remote_file_get_all <- function(remote,
                                       remote_type,
                                       path_dir_save_local) {
  if (!dir.exists(path_dir_save_local)) {
    dir.create(path_dir_save_local, recursive = TRUE)
  }
  switch(remote_type,
    "local" = .projr_remote_file_get_all_local(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    ),
    "osf" = .projr_remote_file_get_all_osf(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    ),
    "github" = .projr_remote_file_get_all_github(
      remote = remote,
      path_dir_save_local = path_dir_save_local
    )
  )
}

# ---------------------
# local
# ---------------------

.projr_remote_file_get_all_local <- function(remote,
                                             path_dir_save_local) {
  remote <- normalizePath(remote, winsslash = "/")
  path_dir_save_local <- normalizePath(path_dir_save_local, winslash = "/")
  if (identical(remote, path_dir_save_local)) {
    return(path_dir_save_local)
  }
  fn_vec <- list.files(remote, recursive = TRUE)
  with_dir(remote, {
    file.copy(fn_vec, path_dir_save_local, recursive = TRUE)
  })
  invisible(path_dir_save_local)
}

# ---------------------
# osf
# ---------------------

.projr_remote_file_get_all_osf <- function(remote,
                                           path_dir_save_local) {
  osf_tbl_file <- remote |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(path_dir_save_local))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_get(
      x = osf_tbl_file[i, ],
      path = path_dir_save_local,
      check = FALSE
    )
  }
  invisible(path_dir_save_local)
}

# ---------------------
# github
# ---------------------

.projr_remote_file_get_all_github <- function(remote, path_dir_save_local) {
  path_file_save_init <- file.path(
    tempdir(), "github", signif(rnorm(1)), remote[["fn"]]
  )
  if (!dir.exists(dirname(path_file_save_init))) {
    dir.create(dirname(path_file_save_init), recursive = TRUE)
  }
  if (!remote[["fn"]] %in% piggyback::pb_list(tag = remote[["tag"]])) {
    return(invisible(path_dir_save_local))
  }
  piggyback::pb_download(
    file = remote[["fn"]],
    dest = path_file_save_init,
    tag = remote[["tag"]],
    overwrite = TRUE,
    use_timestamps = FALSE
  )
  unzip(path_file_save_init, exdir = path_dir_save_local)
  file.remove(path_file_save_init)
  invisible(path_dir_save_local)
}

# ========================
# list all contents of a remote
# ========================

.projr_remote_file_ls <- function(remote_type,
                                  remote,
                                  path_dir_save_local) {
  if (!dir.exists(path_dir_save_local)) {
    dir.create(path_dir_save_local, recursive = TRUE)
  }
  switch(remote_type,
    "local" = .projr_remote_file_ls_local(remote = remote),
    "osf" = .projr_remote_file_ls_osf(remote = remote),
    "github" = .projr_remote_file_ls_github(
      remote = remote, path_dir_save_local = path_dir_save_local
    )
  )
}

# local
.projr_remote_file_ls_local <- function(remote) {
  list.files(remote, recursive = TRUE)
}

# osf
.projr_remote_file_ls_osf <- function(remote,
                                      path_dir_parent = NULL) {
  osf_tbl_file <- remote |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character())
  }
  dir_vec_ind <- .projr_osf_is_dir(osf_tbl_file)
  if (any(!dir_vec_ind)) {
    fn_vec_fn <- osf_tbl_file[["name"]][!dir_vec_ind]
    if (!is.null(path_dir_parent)) {
      fn_vec_fn <- file.path(path_dir_parent, fn_vec_fn)
    }
  } else {
    fn_vec_fn <- NULL
  }
  fn_vec_dir <- NULL
  if (any(dir_vec_ind)) {
    dir_vec_int <- which(dir_vec_ind)
    for (i in seq_along(dir_vec_int)) {
      path_dir_osf <- osf_tbl_file[["name"]][dir_vec_int[i]]
      if (!is.null(path_dir_parent)) {
        path_dir_parent_curr <- file.path(
          basename(path_dir_parent), path_dir_osf
        )
      } else {
        path_dir_parent_curr <- path_dir_osf
      }
      fn_vec_dir_ind <- .projr_osf_ls_files(
        osf_tbl = osfr::osf_mkdir(x = osf_tbl, path = path_dir_osf),
        path_dir_parent = path_dir_parent_curr
      )
      if (length(fn_vec_dir_ind > 0L)) {
        fn_vec_dir <- c(fn_vec_dir, fn_vec_dir_ind)
      }
    }
  }
  c(fn_vec_fn, fn_vec_dir) |> unique()
}

# github
.projr_remote_file_ls_github <- function(remote,
                                         path_dir_save_local) {
  .projr_remote_file_get_all_github(
    remote = remote, path_dir_save_local = path_dir_save_local
  )
  fn_vec <- list.files(path_dir_save_local, recursive = TRUE)
  unlink(path_dir_save_local, recursive = TRUE)
  fn_vec
}

# ========================
# Delete individual files from a remote
# ========================

# pre-specified files
.projr_remote_file_rm <- function(fn,
                                  remote,
                                  remote_type) {
  switch(remote_type,
    "local" = .projr_remote_file_rm_local(fn = fn, remote = remote),
    "osf" = .projr_remote_file_rm_osf(fn = fn, remote = remote),
    "github" = .projr_remote_file_rm_github(fn = fn, remote = remote)
  )
}

# local
.projr_remote_file_rm_local <- function(fn,
                                        remote) {
  if (length(fn) == 0L) {
    return(invisible(FALSE))
  }
  fn_vec <- file.path(remote, fn)
  fn_vec <- fn_vec[file.exists(fn_vec)]
  if (length(fn_vec) == 0L) {
    return(invisible(FALSE))
  }
  suppressWarnings(file.remove(fn_vec))
  invisible(TRUE)
}

# osf
.projr_remote_file_rm_osf <- function(fn,
                                      remote) {
  if (length(fn) == 0) {
    return(invisible(FALSE))
  }
  plot_df <- data.frame(fn = fn, dir = dirname(fn))
  dir_vec <- unique(plot_df[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_rm <- osfr::osf_mkdir(x = remote, path = x)
    } else {
      osf_tbl_rm <- remote
    }
    osf_tbl_file <- osf_tbl_rm |> osfr::osf_ls_files()
    fn_vec <- plot_df[["fn"]][plot_df[["dir"]] == x]
    osf_tbl_file_rm <- osf_tbl_file[osf_tbl_file[["name"]] %in% fn_vec, ]

    # delete entire directory if it's a directory and
    # all files are being deleted
    if (nrow(osf_tbl_file_rm) == nrow(osf_tbl_file)) {
      if (inherits(osf_tbl_rm, "osf_tbl_file")) {
        osfr::osf_rm(x = osf_tbl_rm, check = FALSE, recurse = FALSE)
      }
      return(invisible(TRUE))
    }
    # delete files one by one if it's a node or if
    # not all files are deleted in directory
    for (i in seq_along(fn_vec)) {
      osfr::osf_rm(x = osf_tbl_file_rm[i, ], check = FALSE, recurse = FALSE)
    }
  }
  invisible(TRUE)
}

# github
.projr_remote_file_rm_github <- function(fn,
                                         remote) {
  if (length(fn) == 0L) {
    return(invisible(FALSE))
  }
  piggyback::pb_delete(file = fn, remote = remote[["tag"]])
  invisible(TRUE)
}

# ========================
# add individual files to a remote
# ========================

.projr_remote_file_add <- function(fn,
                                   label,
                                   path_dir_local,
                                   remote,
                                   remote_type) {
  switch(remote_type,
    "local" = .projr_remote_file_add_local(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    ),
    "osf" = .projr_remote_file_add_osf(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    ),
    "github" = .projr_remote_file_add_github(
      fn = fn, path_dir_local = path_dir_local, remote = remote
    )
  )
}

# local
.projr_remote_file_add_local <- function(fn,
                                         path_dir_local,
                                         remote) {
  if (length(fn) == 0L) {
    return(invisible(FALSE))
  }
  remote <- fs::path_abs(remote)
  withr::with_dir(
    path_dir_local,
    {
      fn <- fn[file.exists(fn)]
      if (length(fn_vec) == 0L) {
        return(invisible(FALSE))
      }
      suppressWarnings(file.copy(fn_vec, remote, recursive = TRUE))
      invisible(TRUE)
    }
  )
}

# osf
.projr_remote_file_add_osf <- function(fn,
                                       path_dir_local,
                                       remote) {
  if (length(fn) == 0) {
    return(invisible(FALSE))
  }
  .projr_dep_install("osfr")
  plot_df <- data.frame(fn = fn, dir = dirname(fn))
  dir_vec <- unique(plot_df[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_upload <- osfr::osf_mkdir(x = remote, path = x)
    } else {
      osf_tbl_upload <- remote
    }
    osfr::osf_upload(
      x = osf_tbl_upload,
      path = file.path(path_dir_local, plot_df[["fn"]][plot_df[["dir"]] == x]),
      conflicts = "overwrite"
    )
  }
  invisible(TRUE)
}

# github
.projr_remote_file_add_github <- function(fn,
                                          path_dir_local,
                                          remote) {
  label <- gsub("\\.zip", "", remote[["fn"]])
  if (length(fn) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  .projr_dep_install("piggyback")

  path_zip <- .projr_zip_file(
    fn_rel = fn,
    path_dir_fn_rel = path_dir_local,
    fn_rel_zip = remote[["fn"]]
  )
  if (length(path_zip) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  tag <- .projr_pb_tag_format(remote[["tag"]])
  if (!.projr_pb_create_release(tag = tag)) {
    warning(paste0("Could not create release with tag ", tag))
    return(invisible(FALSE))
  }
  if (length(path_zip) == 0L && label == "code") {
    return(invisible(TRUE))
  }
  .projr_pb_upload(path_zip = path_zip, tag = tag)
}
