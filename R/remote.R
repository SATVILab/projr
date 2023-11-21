# ========================
# create remotes
# ========================

# return a character giving us the key information to
# find the remote again

.projr_remote_create <- function(type,
                                 id,
                                 name,
                                 ...) {
  switch(type,
    "local" = .projr_remote_create_local(path = id),
    "osf" = .projr_remote_create_osf(title = name, ...),
    "github" = .projr_remote_create_github(tag = id, ...)
  )
}

# local
.projr_remote_create_local <- function(path) {
  .projr_remote_check_base(type = "local", path = path)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}

# osf
.projr_remote_create_osf <- function(title,
                                     id = NULL,
                                     id_parent = NULL,
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
    id_parent = id_parent,
    category = category,
    body = body,
    public = public,
    parent_title = NULL,
    path = NULL,
    path_append_label = NULL
  )
  .projr_remote_create_osf_node(
    title = title,
    id_parent = id_parent,
    category = category,
    body = body,
    public = public
  )
}

.projr_remote_create_osf_node <- function(title,
                                          id_parent,
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
      id_parent = id_parent,
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
                                               id_parent,
                                               category,
                                               body,
                                               public) {
  id <- try(osfr::osf_create_component(
    x = .projr_remote_get_osf(id = id_parent),
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
  if (is.null(body)) {
    body <- "Release created automatically by `projr`"
  }
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
  piggyback::.pb_cache_clear()
  try(suppressWarnings(suppressMessages(
    piggyback::pb_release_create(tag = tag, body = body)
  )))
}

# ========================
# check existence
# ========================

.projr_remote_check_exists <- function(type,
                                       id) {
  switch(type,
    "local" = .projr_remote_check_exists_local(path = id),
    "osf" = .projr_remote_check_exists_osf(id = id),
    "github" = .projr_remote_check_exists_github(tag = id)
  )
}

# local
.projr_remote_check_exists_local <- function(path) {
  dir.exists(path)
}

# osf
.projr_remote_check_exists_osf <- function(id) {
  !inherits(
    .projr_remote_get(type = "osf", id = id),
    "try-error"
  )
}

# github
.projr_remote_check_exists_github <- function(tag) {
  release_tbl <- .projr_pb_get_release_tbl()
  if (inherits(release_tbl, "try-error")) {
    stop("Could not get GitHub release table")
  }
  tag %in% release_tbl[["release_name"]]
}

# =====================
# get just the remote itself,
# nothing more specific like sub-directories (OSF/local)
# or file names to upload to (GitHub).
# does not create remotes either.
# =====================

.projr_remote_get <- function(type,
                              id) {
  switch(type,
    "local" = .projr_remote_get_local(id = id),
    "osf" = .projr_remote_get_osf(id = id),
    "github" = .projr_remote_get_github(id),
    stop(paste0("type '", type, "' not recognized"))
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
.projr_remote_get_final <- function(type,
                                    id,
                                    path = NULL,
                                    path_append_label = TRUE,
                                    label,
                                    structure) {
  switch(type,
    "local" = .projr_remote_get_final_local(
      path = id,
      path_append_label = path_append_label,
      label = label,
      structure = structure
    ),
    "osf" = .projr_remote_get_final_osf(
      id = id,
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure
    ),
    "github" = .projr_remote_get_final_github(
      id = id, label = label
    ),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# ---------------------
# local
# ---------------------

.projr_remote_get_final_local <- function(path,
                                          path_append_label,
                                          label,
                                          structure) {
  # the local destination is just the
  # local directory where files are get, so
  # it is just the path.
  # note that `output_run` does not matter,
  # as this is a "remote" in the sense of something
  # interacted without either before or after the build process,
  # unlike the direoctires specified in
  # _projr.yml[["directories"]]`
  path_dir <- .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    type = "local"
  )
  # create this, as we create the OSF sub-directory
  # if specified. Needs to be automated
  # due to versioning
  if (!dir.exists(path_dir)) {
    dir.create(path_dir, recursive = TRUE)
  }
  path_dir
}

# ---------------------
# osf
# ---------------------

.projr_remote_get_final_osf <- function(id,
                                        path,
                                        path_append_label,
                                        label,
                                        structure) {
  if (missing(label)) {
    if (!path_append_label) {
      label <- "abc"
    } else {
      stop(paste0("label must be supplied if to be appended"))
    }
  }
  path_rel <- .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    type = "osf"
  )
  osf_tbl <- .projr_remote_get(id = id, type = "osf")
  if (length(path_rel) > 0L) {
    osf_tbl <- osfr::osf_mkdir(osf_tbl, path_rel)
  }
  osf_tbl
}

# ---------------------
# github
# ---------------------

.projr_remote_get_final_github <- function(id, label) {
  # everything uploaded to a gh release
  # is a single file, and all other remotes
  # are just directories where files can
  # be uploaded to (and possibly folders,
  # but giithub releases don't do that), so
  # the remote for github release is just the
  # name of the tag
  # (given generally as the id here)
  c("tag" = id, "fn" = paste0(label, ".zip"))
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
                                       structure,
                                       type) {
  fn <- switch(type,
    "osf" = , # same as local
    "local" = .projr_remote_get_path_rel_hierarchy,
    "github" = .projr_remote_get_path_rel_flat
  )
  args_list <- list(
    path = path,
    path_append_label = path_append_label
  )
  if (missing(structure)) {
    stop("structure must be supplied")
  }
  if (missing(path_append_label)) {
    stop("path_append_label must be supplied")
  }
  args_list <- list(
    structure = structure, path_append_label = path_append_label
  )
  if (!missing(label)) {
    args_list <- args_list |> append(list(label = label))
  }
  if (missing(path) || is.null(path)) {
    path <- NULL
  }
  args_list <- args_list |> append(list(path = path))
  do.call(what = fn, args = args_list)
}

# hierarchical remotes
.projr_remote_get_path_rel_hierarchy <- function(path,
                                                 path_append_label,
                                                 label,
                                                 structure) {
  args_list <- list()
  if (!is.null(path)) {
    args_list <- list(path)
  }
  if (path_append_label) {
    args_list <- args_list |> append(list(label))
  }
  if (structure == "version") {
    args_list <- args_list |> append(list(paste0("v", projr_version_get())))
  }
  if (length(args_list) == 0L) {
    return(character())
  }

  do.call(file.path, args_list)
}

# flat remotes
.projr_remote_get_path_rel_flat <- function(...) character()


# ========================
# delete an unused empty remote directory
# ========================

.projr_remote_rm_final_if_empty <- function(type,
                                            remote,
                                            structure) {
  switch(type,
    "local" = .projr_remote_rm_final_if_empty_local(
      remote = remote, structure = structure
    ),
    "osf" = .projr_remote_rm_final_if_empty_osf(
      remote = remote, structure = structure
    ),
    "github" = .projr_remote_rm_final_if_empty_github()
  )
}

# local
.projr_remote_rm_final_if_empty_local <- function(remote, structure) {
  # only do this for versioned ones
  if (!structure == "version") {
    return(invisible(FALSE))
  }
  if (!dir.exists(remote)) {
    return(invisible(FALSE))
  }
  if (length(list.files(remote)) > 0L) {
    return(invisible(FALSE))
  }
  unlink(remote, recursive = TRUE)
  invisible(TRUE)
}

# osf
.projr_remote_rm_final_if_empty_osf <- function(remote, structure) {
  if (!structure == "version") {
    return(invisible(FALSE))
  }
  if (!inherits(remote, "osf_tbl_file")) {
    return(invisible(FALSE))
  }
  if (nrow(osfr::osf_ls_files(remote)) > 0L) {
    return(invisible(FALSE))
  }
  osfr::osf_rm(x = remote, check = FALSE)
  invisible(TRUE)
}

# github
.projr_remote_rm_final_if_empty_github <- function() {
  # never any need to, as the release is only
  # created if it's to be uploaded to
  invisible(FALSE)
}

# ========================
# delete a remote host
# ========================

# this is different to deleting a remote,
# at least for GitHub as we don't in that case
# delete the release itself - we actually delete the repo.

.projr_remote_host_rm <- function(type,
                                  remote_host) {
  switch(type,
    "local" = .projr_remote_host_rm_local(remote_host = remote_host),
    "osf" = .projr_remote_host_rm_osf(remote_host = remote_host),
    "github" = .projr_remote_host_rm_github(remote_host = remote_host)
  )
}

# local
.projr_remote_host_rm_local <- function(remote_host) {
  if (!dir.exists(remote_host)) {
    return(invisible(FALSE))
  }
  unlink(remote_host, recursive = TRUE)
  invisible(TRUE)
}

# osf
.projr_remote_host_rm_osf <- function(remote_host) {
  osfr::osf_rm(
    x = osfr::osf_retrieve_node(remote_host), check = FALSE, recurse = TRUE
  )
  invisible(TRUE)
}

# github
.projr_remote_host_rm_github <- function(remote_host) {
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  user <- if ("user" %in% names(remote_host)) remote_host[["user"]] else NULL
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")
  token <- if ("token" %in% names(remote_host)) remote_host[["token"]] else NULL # nolint
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- if ("repo" %in% names(remote_host)) remote_host[["repo"]] else NULL
  if (!nzchar(repo)) stop("No GitHub repo specified")
  # Define the URL of the GitHub API
  # take basename in case we've accidentally specified
  # the user as well in the repo specification
  repo <- basename(repo)
  if (repo == "projr") stop("Cannot delete the projr repo")

  try(
    gh::gh(
      "DELETE /repos/{username}/{pkg}",
      username = user,
      pkg = repo
    ),
    silent = TRUE
  )
}

# ========================
# delete all contents of a remote
# ========================

# this is different to the above, as it
# only deletes the contents and not
# the remote directory itself (also,
# the above will only delete the remote if
# it's actually got nothing, so it's very different)
.projr_remote_file_rm_all <- function(type,
                                      remote) {
  switch(type,
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
  piggyback::.pb_cache_clear()
  piggyback::pb_delete(tag = remote)
  invisible(TRUE)
}

# ========================
# download all contents of a remote
# ========================

# return the path to which it's downloaded
.projr_remote_file_get_all <- function(type,
                                       remote,
                                       path_dir_save_local) {
  if (!dir.exists(path_dir_save_local)) {
    dir.create(path_dir_save_local, recursive = TRUE)
  }
  switch(type,
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
  piggyback::.pb_cache_clear()
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

.projr_remote_file_ls <- function(type,
                                  remote,
                                  path_dir_save_local) {
  if (!dir.exists(path_dir_save_local)) {
    dir.create(path_dir_save_local, recursive = TRUE)
  }
  switch(type,
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
.projr_remote_file_rm <- function(type,
                                  fn,
                                  remote) {
  switch(type,
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
  piggyback::.pb_cache_clear()
  piggyback::pb_delete(file = fn, remote = remote[["tag"]])
  invisible(TRUE)
}

# ========================
# add individual files to a remote
# ========================

.projr_remote_file_add <- function(type,
                                   fn,
                                   label,
                                   path_dir_local,
                                   remote) {
  switch(type,
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
