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
  .assert_string(path)
  .dir_create(path)
  invisible(path)
}

# osf
.projr_remote_create_osf <- function(title,
                                     id_parent = NULL,
                                     category = NULL,
                                     description = NULL,
                                     public = FALSE) {
  .projr_dep_install("osfr")
  category <- .projr_remote_complete_osf_category(
    category = category
  )
  .assert_string(title, TRUE)
  .assert_string(id_parent)
  .assert_nchar(id_parent, 5L)
  .assert_in(category, .projr_opt_remote_get_osf_cat())
  .assert_string(description)
  .assert_flag(public, TRUE)

  .projr_remote_create_osf_node(
    title = title,
    id_parent = id_parent,
    category = category,
    description = description,
    public = public
  )
}

.projr_remote_create_osf_node <- function(title,
                                          id_parent,
                                          category,
                                          description,
                                          public) {
  switch(category,
    "project" = .projr_remote_create_osf_project(
      title = title,
      description = description,
      public = public
    ),
    .projr_remote_create_osf_component(
      title = title,
      id_parent = id_parent,
      category = category,
      description = description,
      public = public
    )
  )
}

.projr_remote_create_osf_project <- function(title,
                                             description,
                                             public) {
  id <- try(osfr::osf_create_project(
    title = title,
    description = description,
    public = public,
    category = "project"
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}

.projr_remote_create_osf_component <- function(title,
                                               id_parent,
                                               category,
                                               description,
                                               public) {
  id <- try(osfr::osf_create_component(
    x = .projr_remote_get_osf(id = id_parent),
    title = title,
    description = description,
    public = public,
    category = category
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}


# github
.projr_remote_create_github <- function(tag,
                                        description = NULL,
                                        pause_second = 3) {
  .projr_dep_install("piggyback")
  .assert_string(tag, TRUE)
  .assert_string(description)
  .assert_number(pause_second, TRUE)
  if (is.null(description)) {
    description <- "Release created automatically by `projr`"
  }
  pb_release_create <- .projr_remote_create_github_attempt(
    tag = tag, description = description
  )
  if (!.is_try_error(pb_release_create)) {
    return(invisible(tag))
  }
  Sys.sleep(pause_second)
  pb_release_create <- .projr_remote_create_github_attempt(
    tag = tag, description = description
  )
  if (!.is_try_error(pb_release_create)) {
    return(invisible(tag))
  }
  invisible(character())
}

.projr_remote_create_github_attempt <- function(tag, description) {
  piggyback::.pb_cache_clear()
  try(suppressWarnings(suppressMessages(
    piggyback::pb_release_create(tag = tag, body = description)
  )))
}

# ========================
# check existence
# ========================

.projr_remote_check_exists <- function(type,
                                       id) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_check_exists_local(path = id),
    "osf" = .projr_remote_check_exists_osf(id = id),
    "github" = .projr_remote_check_exists_github(tag = id)
  )
}

# local
.projr_remote_check_exists_local <- function(path) {
  .assert_path_not_file(path)
  dir.exists(path)
}

# osf
.projr_remote_check_exists_osf <- function(id) {
  .assert_nchar_single(id, 5L, TRUE)
  !.is_try_error(.projr_remote_get(type = "osf", id = id))
}

# github
.projr_remote_check_exists_github <- function(tag) {
  .assert_string(tag, TRUE)
  release_tbl <- .projr_pb_release_tbl_get()
  if (.is_try_error(release_tbl)) {
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
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_get_local(id = id),
    "osf" = .projr_remote_get_osf(id = id),
    "github" = .projr_remote_get_github(id),
    stop(paste0("type '", type, "' not recognized"))
  )
}

# local
.projr_remote_get_local <- function(id) {
  .assert_string(id, TRUE)
  id
}

# osf
.projr_remote_get_osf <- function(id) {
  .assert_nchar_single(id, 5L, TRUE)
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
  .assert_string(id, TRUE)
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
  .assert_string(path, TRUE)
  .assert_path_not_file(path)
  .assert_flag(path_append_label)
  .assert_in(label, .projr_opt_dir_get_label_send(NULL))
  .assert_in_single(structure, .projr_opt_remote_get_structure())

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
  .dir_create(path_dir)
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
  .assert_nchar_single(id, 5L, TRUE)
  .assert_string(path)
  .assert_flag(path_append_label)
  .assert_in(label, .projr_opt_dir_get_label_send(NULL))
  .assert_in_single(structure, .projr_opt_remote_get_structure())
  label <- .projr_remote_get_final_osf_get_label(
    label, path_append_label
  )
  path_rel <- .projr_remote_get_path_rel(
    type = "osf",
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure
  )
  osf_tbl <- .projr_remote_get(id = id, type = "osf")
  if (length(path_rel) > 0L) {
    osf_tbl <- osfr::osf_mkdir(osf_tbl, path_rel)
  }
  osf_tbl
}

.projr_remote_get_final_osf_get_label <- function(label, path_append_label) {
  if (missing(label)) {
    if (!path_append_label) {
      label <- "abc"
    } else {
      stop(paste0("label must be supplied if to be appended"))
    }
  }
  label
}

# ---------------------
# github
# ---------------------

.projr_remote_get_final_github <- function(id, label) {
  .assert_string(id, TRUE)
  .assert_in(label, .projr_opt_dir_get_label_send(NULL), TRUE)
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

  args_list <- list(
    structure = structure, path_append_label = path_append_label
  )
  if (!missing(label)) {
    args_list <- args_list |> append(list(label = label))
  }
  if (!.is_given_mid(path)) {
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
  .assert_string(path)
  .assert_flag(path_append_label, TRUE)
  .assert_in_single(structure, .projr_opt_remote_get_structure(), TRUE)
  if (path_append_label) {
    .assert_in(label, .projr_opt_dir_get_label_send(NULL), TRUE)
  }

  args_list <- list()
  if (!is.null(path)) {
    args_list <- list(path)
  }
  if (path_append_label) {
    args_list <- args_list |> append(list(label))
  }
  if (structure == "version") {
    args_list <- args_list |> append(list(.projr_version_get_v()))
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
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
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
  .assert_in(structure, .projr_opt_remote_get_structure(), TRUE)
  .assert_string(remote, TRUE)
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
  .assert_in(structure, .projr_opt_remote_get_structure(), TRUE)
  .assert_given_full(remote)
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
                                  host) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_host_rm_local(host),
    "osf" = .projr_remote_host_rm_osf(host),
    "github" = .projr_remote_host_rm_github(host)
  )
}

# local
.projr_remote_host_rm_local <- function(host) {
  .assert_string(host, TRUE)
  if (!dir.exists(host)) {
    return(invisible(FALSE))
  }
  unlink(host, recursive = TRUE)
  invisible(TRUE)
}

# osf
.projr_remote_host_rm_osf <- function(host) {
  .assert_given_full(host)
  osfr::osf_rm(
    x = osfr::osf_retrieve_node(host), check = FALSE, recurse = TRUE
  )
  invisible(TRUE)
}

# github
.projr_remote_host_rm_github <- function(host) {
  .assert_given_full(host)
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    .projr_dep_install_only("gh")
  }

  # defaults
  user <- if ("user" %in% names(host)) host[["user"]] else NULL
  .projr_dep_install("gh")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!.is_string(user)) stop("No GitHub user found")
  token <- if ("token" %in% names(host)) host[["token"]] else NULL # nolint
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!.is_string(token)) stop("No GitHub token found")
  repo <- if ("repo" %in% names(host)) host[["repo"]] else NULL
  if (!.is_string(repo)) stop("No GitHub repo specified")
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
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_file_rm_all_local(remote),
    "osf" = .projr_remote_file_rm_all_osf(remote),
    "github" = .projr_remote_file_rm_all_github(remote)
  )
}

# local
.projr_remote_file_rm_all_local <- function(remote) {
  .assert_string(remote, TRUE)
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
  .assert_given_full(remote)
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
  .assert_chr_mid(remote, TRUE)
  .assert_in("tag", names(remote), TRUE)
  .assert_in("fn", names(remote), TRUE)
  .projr_dep_install("piggyback")
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
  .assert_string(path_dir_save_local, TRUE)
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  .dir_create(path_dir_save_local)
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
  .assert_string(remote, TRUE)
  .dir_copy(remote, path_dir_save_local)
}

# ---------------------
# osf
# ---------------------

.projr_remote_file_get_all_osf <- function(remote,
                                           path_dir_save_local) {
  .assert_given_full(remote)
  osf_tbl_file <- remote |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(path_dir_save_local))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    osfr::osf_download(
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
  .assert_given_full(remote)
  if (!.projr_remote_check_exists("github", remote[["tag"]])) {
    return(invisible(FALSE))
  }
  .projr_remote_file_get_all_github_file(
    remote = remote, path_dir_save_local = path_dir_save_local
  )
}

.projr_remote_file_get_all_github_file <- function(remote, path_dir_save_local) {
  piggyback::.pb_cache_clear()
  path_dir_save_init <- .dir_create_tmp_random()
  if (!remote[["fn"]] %in% piggyback::pb_list(tag = remote[["tag"]])) {
    return(invisible(path_dir_save_local))
  }
  piggyback::pb_download(
    file = remote[["fn"]],
    dest = path_dir_save_init,
    tag = remote[["tag"]],
    overwrite = TRUE,
    use_timestamps = FALSE
  )
  utils::unzip(
    file.path(path_dir_save_init, remote[["fn"]]),
    exdir = path_dir_save_local
  )
  file.remove(file.path(path_dir_save_init, remote[["fn"]]))
  invisible(path_dir_save_local)
}

# ========================
# list all contents of a remote (without versioning)
# ========================

.projr_remote_file_ls <- function(type,
                                  remote) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_file_ls_local(remote),
    "osf" = .projr_remote_file_ls_osf(remote),
    "github" = .projr_remote_file_ls_github(remote)
  )
}

# local
.projr_remote_file_ls_local <- function(remote) {
  .assert_string(remote, TRUE)
  .file_ls(path_dir = remote)
}

# osf
.projr_remote_file_ls_osf <- function(remote,
                                      path_dir_parent = NULL) {
  .assert_given_full(remote)
  .assert_chr(path_dir_parent)
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
        osf_tbl = osfr::osf_mkdir(x = remote, path = path_dir_osf),
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
.projr_remote_file_ls_github <- function(remote) {
  .assert_given_full(remote)
  path_dir_save_local <- .dir_create_tmp_random()
  .projr_remote_file_get_all(
    "github",
    remote = remote, path_dir_save_local = path_dir_save_local
  )
  fn_vec <- .projr_remote_file_ls("local", path_dir_save_local)
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
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_file_rm_local(fn = fn, remote = remote),
    "osf" = .projr_remote_file_rm_osf(fn = fn, remote = remote),
    "github" = .projr_remote_file_rm_github(fn = fn, remote = remote)
  )
}

# local
.projr_remote_file_rm_local <- function(fn,
                                        remote) {
  .assert_chr_min(fn, TRUE)
  if (length(fn) == 0L) {
    return(invisible(FALSE))
  }
  .assert_string(remote, TRUE)
  fn_vec <- .file_filter_exists(file.path(remote, fn))
  if (length(fn_vec) == 0L) {
    return(invisible(FALSE))
  }
  suppressWarnings(file.remove(fn_vec))
  invisible(TRUE)
}

# osf
.projr_remote_file_rm_osf <- function(fn,
                                      remote) {
  .assert_chr(fn, TRUE)
  if (length(fn) == 0) {
    return(invisible(FALSE))
  }
  .assert_given_full(remote)
  # osfr requires doing deletions directory by directory
  dir_vec <- unique(dirname(fn))
  # do deeper directories first, to give a
  # chance to delete entire directories in one go later,
  # which may be faster
  dir_vec <- dir_vec[order(.projr_dir_count_lvl(dir_vec), decreasing = TRUE)]
  vapply(
    dir_vec, .projr_remote_file_rm_osf_dir, logical(1),
    osf_tbl = remote, fn = fn
  )
  invisible(TRUE)
}

.projr_remote_file_rm_osf_dir <- function(dir, fn, osf_tbl) {
  osf_tbl_rm <- .projr_remote_file_rm_osf_rm_get(
    path = dir, node = osf_tbl
  )
  osf_tbl_rm_file <- osf_tbl_rm |> osfr::osf_ls_files(n_max = Inf)
  fn_vec_dir <- basename(fn)[dirname(fn) == dir]
  .projr_remote_file_rm_osf_detailed(
    fn_dir = fn_vec_dir, osf_tbl = osf_tbl_rm, osf_tbl_file = osf_tbl_rm_file
  )
}

.projr_remote_file_rm_osf_rm_get <- function(path,
                                             node) {
  .assert_string(path, TRUE)
  .assert_given_full(node)
  if (path != ".") {
    node <- osfr::osf_mkdir(x = node, path = path)
  }
  node
}

.projr_remote_file_rm_osf_detailed <- function(fn_dir,
                                               osf_tbl,
                                               osf_tbl_file) {
  .assert_chr(fn_dir, TRUE)
  .assert_given_full(osf_tbl)

  fn_vec_osf <- osf_tbl_file[["name"]]
  # might be faster to just delete the whole directory
  remove_dir <- setequal(fn_dir, fn_vec_osf) &&
    inherits(osf_tbl, "osf_tbl_file")
  if (remove_dir) {
    osfr::osf_rm(x = osf_tbl, check = FALSE, recurse = FALSE)
    return(invisible(TRUE))
  }
  fn_vec_to_rm <- fn_vec_osf[fn_vec_osf %in% fn_dir]
  if (length(fn_vec_to_rm) == 0L) {
    return(invisible(FALSE))
  }
  .assert_given_full(osf_tbl_file)
  .projr_remote_file_rm_osf_fn(
    fn_rm = fn_vec_to_rm, osf_tbl_file = osf_tbl_file
  )
  invisible(TRUE)
}

.projr_remote_file_rm_osf_fn <- function(fn_rm, osf_tbl_file) {
  .assert_chr(fn_rm, TRUE)
  .assert_given_full(osf_tbl_file)
  # osfr requires deleting individual files one-by-one
  # by passing a table
  for (i in seq_along(fn_rm)) {
    osf_tbl_file_ind <- osf_tbl_file[
      osf_tbl_file[["name"]] == fn_rm[[i]],
    ]
    osfr::osf_rm(x = osf_tbl_file_ind, check = FALSE, recurse = FALSE)
  }
}

# github
.projr_remote_file_rm_github <- function(fn,
                                         remote) {
  .assert_chr(fn, TRUE)
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_given_full(remote)
  piggyback::.pb_cache_clear()
  path_dir_save_local <- .dir_create_tmp_random()
  .projr_remote_file_get_all(
    "github",
    remote = remote, path_dir_save_local = path_dir_save_local
  )
  fn_vec <- .projr_remote_file_ls("local", path_dir_save_local)
  fn_vec_to_rm <- fn_vec[fn_vec %in% fn]
  if (length(fn_vec_to_rm) == 0L) {
    return(invisible(FALSE))
  }
  .projr_remote_file_rm(
    "local",
    fn = fn_vec_to_rm, remote = path_dir_save_local
  )
  fn_vec_to_upload <- setdiff(fn_vec, fn_vec_to_rm)
  .projr_remote_file_add(
    "github",
    fn = fn_vec_to_upload,
    path_dir_local = path_dir_save_local,
    remote = remote
  )
  unlink(path_dir_save_local, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# add individual files to a remote
# ========================

.projr_remote_file_add <- function(type,
                                   remote,
                                   path_dir_local,
                                   fn) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
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
  .assert_chr_min(fn, TRUE)
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)
  .assert_string(remote, TRUE)
  .dir_copy_file(
    fn = fn,
    path_dir_from = path_dir_local,
    path_dir_to = remote
  )
}

# osf
.projr_remote_file_add_osf <- function(fn,
                                       path_dir_local,
                                       remote) {
  .assert_chr(fn, TRUE)
  if (length(fn) == 0) {
    return(invisible(FALSE))
  }
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)
  .assert_given_full(remote)
  .projr_dep_install("osfr")
  plot_tbl <- data.frame(fn = fn, dir = dirname(fn))
  dir_vec <- unique(plot_tbl[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_upload <- osfr::osf_mkdir(x = remote, path = x)
    } else {
      osf_tbl_upload <- remote
    }
    osfr::osf_upload(
      x = osf_tbl_upload,
      path = file.path(
        path_dir_local, plot_tbl[["fn"]][plot_tbl[["dir"]] == x]
      ),
      conflicts = "overwrite"
    )
  }
  invisible(TRUE)
}

# github
.projr_remote_file_add_github <- function(fn,
                                          path_dir_local,
                                          remote) {
  .assert_given_full(remote)
  .assert_chr(fn, TRUE)
  label <- gsub("\\.zip", "", remote[["fn"]])
  if (length(fn) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  .projr_dep_install("piggyback")
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)

  path_zip <- .projr_zip_file(
    fn_rel = fn,
    path_dir_fn_rel = path_dir_local,
    fn_rel_zip = remote[["fn"]]
  )
  if (length(path_zip) == 0L && label != "code") {
    return(invisible(FALSE))
  }
  tag <- .projr_pb_tag_format(remote[["tag"]])
  release_tbl <- .projr_pb_release_tbl_get()
  if (!tag %in% release_tbl[["release_name"]]) {
    .projr_remote_create("github", id = tag)
    Sys.sleep(3)
  }
  # if only needing code uploaded, then it's done already
  # by creating the release
  if (length(path_zip) == 0L && label == "code") {
    return(invisible(TRUE))
  }
  .projr_remote_file_add_github_zip(path_zip = path_zip, tag = tag)
}

.projr_remote_file_add_github_zip <- function(path_zip,
                                              tag,
                                              pause_second = 3) {
  pb_upload <- .projr_remote_file_add_github_zip_attempt(
    path_zip = path_zip, tag = tag
  )
  if (!inherits(pb_upload, "try-error")) {
    return(invisible(TRUE))
  }
  Sys.sleep(pause_second)
  pb_upload <- .projr_remote_file_add_github_zip_attempt(
    path_zip = path_zip, tag = tag
  )
  if (!inherits(pb_upload, "try-error")) {
    return(invisible(TRUE))
  }
  warning(paste0(
    "Could not upload ", # nolint
    basename(path_zip),
    " to GitHub release with tag ", # nolint
    tag
  ))
  invisible(FALSE)
}

.projr_remote_file_add_github_zip_attempt <- function(path_zip, tag) {
  try(suppressWarnings(suppressMessages(
    piggyback::pb_upload(file = path_zip, tag = tag)
  )))
}

# ===================================
# Miscellaneous
# ===================================

.projr_remote_ls <- function() {
  remote_vec <- c(
    .projr_remote_ls_source(),
    .projr_remote_ls_dest(),
    "github"[.projr_git_push_check()]
  ) |>
    unique()
  remote_vec[nzchar(remote_vec)]
}

.projr_remote_ls_source <- function() {
  yml_projr_dir <- .projr_yml_dir_get(NULL)
  lapply(yml_projr_dir, function(x) {
    remote_vec <- c("github", "osf")
    remote_vec[remote_vec %in% names(x)]
  }) |>
    unlist() |>
    unique()
}

.projr_remote_ls_dest <- function() {
  yml_projr_build <- .projr_yml_build_get(NULL)
  remote_vec <- c("github", "osf")
  remote_vec[remote_vec %in% names(yml_projr_build)]
}

.projr_git_push_check <- function() {
  setting_push_explicit <-
    .projr_yml_build_get(NULL)[["git"]][["push"]]
  if (is.null(setting_push_explicit)) TRUE else setting_push_explicit
}
