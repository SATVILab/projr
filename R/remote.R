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
    "github" = .projr_remote_create_github(
      tag = .projr_remote_misc_get_github_tag(id),
      ...
    )
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
  id <- try(.projr_osf_create_project(
    title = title,
    description = description,
    public = public,
    category = "project"
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}

#' Create a new project on OSF
#'
#' This function creates a new project on the Open Science Framework (OSF)
#' with the specified title, description, and visibility settings.
#'
#' @param title character. Title of the project.
#' @param description character. Description of the project.
#' @param public logical.
#' Whether the project should be public (TRUE) or private (FALSE).
#'
#' @return A character string containing the ID of the newly created project.
#' @export
#'
#' @examples
#' \dontrun{
#' projr_osf_create_project(
#'   title = "My New Project",
#'   description = "This is a description of my new project.",
#'   public = TRUE # because open science
#' )
#' }
#'
#' @seealso \url{https://osf.io/} for more information about OSF.
projr_osf_create_project <- function(title,
                                     description,
                                     public) {
  id <- .projr_remote_create_osf_project(
    title = title,
    description = description,
    public = public
  )
  if (!.is_string(id)) {
    stop(paste0("Failed to create OSF project"))
  }
  id
}

.projr_remote_create_osf_component <- function(title,
                                               id_parent,
                                               category,
                                               description,
                                               public) {
  id <- try(.projr_osf_create_component(
    x = .projr_remote_get_osf(id = id_parent),
    title = title,
    description = description,
    public = public,
    category = category
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}

.projr_remote_create_osf_component_check_id_parent <- function(id_parent) {
  id_parent <- try(force(id_parent), silent = TRUE)
  if (is.null(id_parent) || inherits(id_parent, "try-error")) {
    # if the OSF category is not a project,
    # then the parent id must be supplied.
    # here we tell them that, and tell them to
    # either create it directly on OSF or
    # use the `projr_osf_create_project` function, seeing
    # `?projr_osf_create_project` for details.
    stop(paste0(
      "The parent ID must be supplied if the OSF category is not a project.", # nolint
      "\n",
      "Please create the project directly on OSF or use `projr_osf_create_project`." # nolint
    ), call. = FALSE)
  }
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
    "github" = .projr_remote_check_exists_github(
      tag = .projr_remote_misc_get_github_tag(id)
    )
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
  .projr_dep_install("piggyback")
  .assert_string(tag, TRUE)
  release_tbl <- .projr_pb_release_tbl_get()
  if (.is_try_error(release_tbl)) {
    stop("Could not get GitHub release table")
  }
  tag %in% release_tbl[["release_name"]]
}

# ========================
# check existence of remote_final
# ========================

.projr_remote_final_check_exists <- function(type,
                                             id,
                                             label,
                                             structure,
                                             path,
                                             path_append_label,
                                             version) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  version <- if (is.null(version)) {
    .projr_version_get_v()
  }  else {
    version |> .projr_version_v_add()
  }
  remote_pre <- .projr_remote_get_final(
    type, id, label, structure, path, path_append_label,
    version, TRUE
  )
  switch(type,
    "local" = .projr_remote_final_check_exists_local(
      remote_pre, structure, label, version
    ),
    "osf" = .projr_remote_final_check_exists_osf(
      remote_pre, structure, label, version
    ),
    "github" = .projr_remote_final_check_exists_github(
      remote_pre, structure, label, version
    )
  )
}

.projr_remote_final_check_exists_local <- function(remote_pre,
                                                   structure,
                                                   label,
                                                   version) {
  remote_final_pseudo <- if (structure == "archive") {
    file.path(remote_pre, .projr_version_v_add(version))
  } else {
    file.path(remote_pre, label)
  }
  dir.exists(remote_final_pseudo)
}

.projr_remote_final_check_exists_osf <- function(remote_pre,
                                                 structure,
                                                 label,
                                                 version) {
  dir_basename <- if (structure == "archive") {
    version |> .projr_version_v_add()
  } else {
    label
  }
  osf_tbl_file <- remote_pre |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(FALSE)
  }
  dir_basename %in% osf_tbl_file[["name"]]
}

.projr_remote_final_check_exists_github <- function(remote_pre,
                                                    structure,
                                                    label,
                                                    version) {
  .assert_attr(remote_pre, "names")
  .assert_has(names(remote_pre), c("tag"))
  .projr_dep_install("piggyback")
  if (!.projr_remote_check_exists("github", remote_pre[["tag"]])) {
    return(FALSE)
  }
  asset_tbl <- .projr_pb_asset_tbl_get(remote_pre[["tag"]])
  # if there's an error for some reason, assume it's not there
  fn <- if (structure == "archive") {
    paste0(label, "-", version, ".zip")
  } else {
    paste0(label, ".zip")
  }
  tryCatch(
    fn %in% asset_tbl[["file_name"]],
    error = function(e) {
      FALSE
    }
  )
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
    "github" = .projr_remote_get_github(.projr_remote_misc_get_github_tag(id)),
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
    .projr_osf_retrieve_node(paste0("https://osf.io/", id)),
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
.projr_remote_get_final <- function(type,
                                    id,
                                    label,
                                    structure,
                                    path = NULL,
                                    path_append_label = TRUE,
                                    version = NULL,
                                    pre = FALSE) {
  # pre: "one up" from the final remote, e.g. the directory
  # above for hierarchical. Does not apply to flat.
  switch(type,
    "local" = .projr_remote_get_final_local(
      path = id,
      label = label,
      structure = structure,
      path_append_label = path_append_label,
      version = version,
      pre = pre
    ),
    "osf" = .projr_remote_get_final_osf(
      id = id,
      label = label,
      structure = structure,
      path = path,
      path_append_label = path_append_label,
      version = version,
      pre = pre
    ),
    "github" = .projr_remote_get_final_github(
      id = .projr_remote_misc_get_github_tag(id),
      label = label,
      structure = structure,
      path = path,
      path_append_label = path_append_label,
      version = version,
      pre = pre
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
                                          structure,
                                          version,
                                          pre) {
  .assert_string(path, TRUE)
  .assert_path_not_file(path)
  .assert_flag(path_append_label)
  .assert_in(label, .projr_opt_dir_get_label_send(NULL))
  .assert_in_single(structure, .projr_opt_remote_get_structure())
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
  path_dir <- .projr_remote_get_path_rel(
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    type = "local",
    version = version,
    pre = pre
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
                                        structure,
                                        version) {
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
    structure = structure,
    version = version
  )
  osf_tbl <- .projr_remote_get(id = id, type = "osf")
  if (length(path_rel) > 0L) {
    osf_tbl <- .projr_osf_mkdir(osf_tbl, path_rel)
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

.projr_remote_get_final_github <- function(id,
                                           path,
                                           path_append_label,
                                           label,
                                           structure,
                                           version,
                                           pre) {
  .assert_string(id, TRUE)
  .assert_in(label, .projr_opt_dir_get_label_send(NULL), TRUE)
  tag <- .projr_remote_misc_get_github_tag(id)
  if (!pre) {
    .projr_remote_create_github(tag = tag)
  }
  if (pre) {
    return(c("tag" = id))
  }

  fn <- .projr_remote_get_path_rel(
    type = "github",
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    version = version
  )
  # everything uploaded to a gh release
  # is a single file, and all other remotes
  # are just directories where files can
  # be uploaded to (and possibly folders,
  # but github releases don't do that), so
  # the remote for a github release
  # is the tag plus the file name
  c("tag" = id, "fn" = fn)
}

# wrapper if it returns NULL because we
# already know it does not exist
.projr_remote_get_final_if_exists <- function(type,
                                              id,
                                              label,
                                              structure,
                                              path = NULL,
                                              path_append_label = TRUE,
                                              version = NULL,
                                              pre = FALSE) {
  exists <- .projr_remote_final_check_exists(
    type, id, label, structure, path, path_append_label, version
  )
  if (!exists) {
    return(NULL)
  }
  .projr_remote_get_final(
    type, id, label, structure, path, path_append_label, version, pre
  )
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
                                       type,
                                       version,
                                       pre) {
  switch(type,
    "osf" = , # same as local
    "local" = .projr_remote_get_path_rel_hierarchy(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version,
      pre = pre
    ),
    "github" = .projr_remote_get_path_rel_github(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version
    )
  )
}

# hierarchical remotes
.projr_remote_get_path_rel_hierarchy <- function(path,
                                                 path_append_label,
                                                 label,
                                                 structure,
                                                 version,
                                                 pre) {
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
  if (structure == "archive") {
    version_add <- if (is.null(version)) .projr_version_get_v() else version
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

.projr_remote_get_path_rel_github <- function(path,
                                              path_append_label,
                                              label,
                                              structure,
                                              version) {
  # keep it as NULL this way if it's already
  # NULL (otherwise it's character(),
  # which triggers an error when checking for a string later)
  if (!is.null(path)) {
    path <- path |> gsub(pattern = "\\.zip$", replacement = "", x = _)
  }
  paste0(
    .projr_remote_get_path_rel_flat(
      path = path,
      path_append_label = path_append_label,
      label = label,
      structure = structure,
      version = version
    ),
    ".zip"
  )
}

# flat remotes
.projr_remote_get_path_rel_flat <- function(path,
                                            path_append_label,
                                            label,
                                            structure,
                                            version) {
  .assert_string(path)
  .assert_flag(path_append_label, TRUE)
  .assert_in_single(structure, .projr_opt_remote_get_structure(), TRUE)
  if (path_append_label) {
    .assert_in(label, .projr_opt_dir_get_label_send(NULL), TRUE)
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
    version_add <- if (is.null(version)) .projr_version_get_v() else version
    path_rel <- paste0(path_rel, "-", version_add)
  }
  path_rel
}


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
  if (!structure == "archive") {
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
  if (!structure == "archive") {
    return(invisible(FALSE))
  }
  if (!inherits(remote, "osf_tbl_file")) {
    return(invisible(FALSE))
  }
  if (nrow(.projr_osf_ls_files(remote)) > 0L) {
    return(invisible(FALSE))
  }
  .projr_osf_rm(x = remote, check = FALSE)
  invisible(TRUE)
}

# github
.projr_remote_rm_final_if_empty_github <- function() {
  # never any need to, as the release is only
  # created if it's to be uploaded to
  invisible(FALSE)
}

# ========================
# Hash a particular remote
# ========================


.projr_remote_hash <- function(type,
                               remote_final,
                               version,
                               label) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  hash_tbl <- .projr_change_file_dir( # nolint
    type, remote_final
  ) |>
    .projr_hash_dir(version) |>
    .projr_manifest_hash_cache_filter(label)
  if (nrow(hash_tbl) == 0) {
    .projr_zero_tbl_get_manifest()
  } else {
    cbind(
      data.frame(label = rep(label, nrow(hash_tbl))),
      hash_tbl
    )
  }
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
  .projr_osf_rm(
    x = .projr_osf_retrieve_node(host), check = FALSE, recurse = TRUE
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
  osf_tbl_file <- remote |> .projr_osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    .projr_osf_rm(x = osf_tbl_file[i, ], recurse = TRUE, check = FALSE)
  }
  invisible(TRUE)
}

# github
.projr_remote_file_rm_all_github <- function(remote) {
  # here, if remote specifies the file, it will only remove
  # that file, but if remote doesn't, then
  # it removes every file.
  # I think this is essentially the same as OSF and local,
  # as there you would specify the remote and then upload
  # all the directories to that remote as directories.
  # here those directories are uploaded as files,
  # which is different.
  .assert_chr_mid(remote, TRUE)
  tag <- .projr_remote_misc_get_github_tag(remote)
  .assert_chr_mid(tag, TRUE)
  .projr_dep_install("piggyback")
  # the `piggyback::pb_delete` function
  # deletes all files by default and
  # pb_release_delete deletes the release itself,
  # so this should still just empty it
  piggyback::.pb_cache_clear()
  release_tbl <- try(.projr_pb_release_tbl_get())
  if (inherits(release_tbl, "try-error")) {
    stop("Could not get GitHub release table")
  }
  if (!(tag %in% release_tbl[["release_name"]])) {
    return(invisible(FALSE))
  }
  # delete individual zipped file
  # if it is in the release
  if ("fn" %in% names(remote)) {
    if (!.projr_remote_file_rm_all_github_check_fn(remote[["fn"]], tag)) {
      return(invisible(FALSE))
    }
    piggyback::pb_delete(tag = tag, file = remote[["fn"]])
  } else {
    try(piggyback::pb_delete(tag = tag))
  }
  invisible(TRUE)
}

.projr_remote_file_rm_all_github_check_fn <- function(fn, tag) {
  .projr_dep_install("piggyback")
  asset_tbl <- try(.projr_pb_asset_tbl_get(tag = tag))
  if (inherits(asset_tbl, "try-error")) {
    stop("Could not get the assets for the GitHub release")
  }
  # assume that NULL asset tbl's mean nothing is there
  if (is.null(asset_tbl)) {
    return(invisible(FALSE))
  }
  if (nrow(asset_tbl) == 0L) {
    return(invisible(FALSE))
  }
  fn %in% asset_tbl[["file_name"]]
}

# ========================
# download single file from a remote
# ========================

.projr_remote_file_get_ind <- function(type,
                                       remote,
                                       fn,
                                       path_dir_save_local) {
  .assert_string(path_dir_save_local, TRUE)
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  .dir_create(path_dir_save_local)
  switch(
    type,
    "local" = .projr_remote_file_get_ind_local(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    ),
    "osf" = .projr_remote_file_get_ind_osf(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    ),
    "github" = .projr_remote_file_get_ind_github(
      remote = remote,
      fn = fn,
      path_dir_save_local = path_dir_save_local
    )
  )
}

.projr_remote_file_get_ind_local <- function(remote,
                                             fn,
                                             path_dir_save_local) {
  path_remote_fn <- file.path(remote, fn)
  if (!file.exists(path_remote_fn)) {
    return(character(0L))
  } else {
    path_fn <- file.path(path_dir_save_local, fn)
    file.copy(path_remote_fn, path_fn)
    path_fn
  }
}

.projr_remote_file_get_ind_osf <- function(remote,
                                           fn,
                                           path_dir_save_local) {
  .assert_given_full(remote)
  osf_tbl_file <- remote |> .projr_osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character(0L))
  }
  if (!fn %in% osf_tbl_file[["name"]]) {
    return(character(0L))
  }
  .projr_osf_download(
    x = osf_tbl_file[osf_tbl_file[["name"]] == fn, ],
    path = path_dir_save_local,
    check = FALSE
  )
  path_fn <- file.path(path_dir_save_local, fn)
  if (file.exists(path_fn)) path_fn else character(0L)
}

.projr_remote_file_get_ind_github <- function(remote,
                                              fn,
                                              path_dir_save_local) {
  .projr_dep_install("piggyback")
  if (!.projr_remote_check_exists("github", remote[["tag"]])) {
    return(character(0L))
  }
  fn_zip <- if (!grepl("\\.zip$", fn)) paste0(fn, ".zip") else fn
  fn_no_zip <- if (grepl("\\.zip$", fn)) gsub("\\.zip$", "", fn) else fn
  remote[["fn"]] <- fn_zip
  .projr_remote_file_get_all_github_file(
    remote = remote, path_dir_save_local = path_dir_save_local
  )
  path_fn <- file.path(path_dir_save_local, fn_no_zip)
  if (file.exists(path_fn)) path_fn else character(0L)
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
  osf_tbl_file <- remote |> .projr_osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(path_dir_save_local))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    .projr_osf_download(
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
  .projr_dep_install("piggyback")
  .assert_given_full(remote)

  if (!.projr_remote_check_exists("github", remote[["tag"]])) {
    return(invisible(FALSE))
  }
  .projr_remote_file_get_all_github_file(
    remote = remote, path_dir_save_local = path_dir_save_local
  )
}

.projr_remote_file_get_all_github_file <- function(remote,
                                                   path_dir_save_local) {
  .projr_dep_install("piggyback")
  piggyback::.pb_cache_clear()
  .assert_attr(remote, "names")
  .assert_has(names(remote), c("tag", "fn"))
  path_dir_save_init <- .dir_create_tmp_random()
  fn_vec_release <- piggyback::pb_list(tag = remote[["tag"]])[["file_name"]]
  if (.is_len_0(fn_vec_release)) {
    return(invisible(path_dir_save_local))
  }
  if (!remote[["fn"]] %in% fn_vec_release) {
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
# Update manifest
# ========================

.projr_remote_write_manifest <- function(type,
                                         remote_pre,
                                         manifest) {
  path_dir_save <- .dir_create_tmp_random()
  .projr_manifest_write(manifest, file.path(path_dir_save, "manifest.csv"))
  remote_pre <- if (type == "github") {
    remote_pre <- remote_pre |> c("fn" = "manifest.csv")
  } else {
    remote_pre
  }
  switch(type,
    "project" = NULL,
    .projr_remote_file_add(
      type, remote_pre, path_dir_save, "manifest.csv"
    )
  )
  unlink(path_dir_save, recursive = TRUE)
  invisible(TRUE)
}

# ========================
# Update VERSION file
# ========================

.projr_remote_write_version_file <- function(type,
                                             remote_pre,
                                             version_file) {
  path_dir_save <- .dir_create_tmp_random()
  writeLines(version_file, file.path(path_dir_save, "VERSION"))
  remote_pre <- if (type == "github") {
    remote_pre <- remote_pre |> c("fn" = "VERSION")
  } else {
    remote_pre
  }
  switch(type,
    "project" = NULL,
    .projr_remote_file_add(
      type, remote_pre, path_dir_save, "VERSION"
    )
  )
}

# ========================
# Get manifests
# ========================

.projr_remote_get_manifest <- function(type,
                                       remote_pre) {
  switch(type,
    "project" = .projr_remote_get_manifest_project(),
    .projr_remote_get_manifest_non_project(type, remote_pre)
  )
}

.projr_remote_get_manifest_project <- function() {
  # just the actual project
  .projr_manifest_read(.path_get("manifest.csv"))
}

.projr_remote_get_manifest_non_project <- function(type,
                                                   remote_pre) {
  manifest_actual <- .projr_remote_get_manifest_non_project_raw(
    type, remote_pre
  )
  if (is.null(manifest_actual)) {
    .projr_remote_get_manifest_project()
  } else {
    manifest_actual
  }
}

.projr_remote_get_manifest_non_project_raw <- function(type, remote_pre) {
  path_dir_save <- .dir_create_tmp_random()
  path_manifest <- .projr_remote_file_get_ind(
    type, remote_pre, "manifest.csv", path_dir_save
  )
  manifest <- .projr_manifest_read(path_manifest)
  unlink(path_dir_save, recursive = TRUE)
  manifest
}

# ========================
# Get VERSION
# ========================

.projr_remote_get_version_file <- function(type,
                                           remote_pre) {
  switch(type,
    "project" = character(0L),
    .projr_remote_get_version_file_non_project(type, remote_pre)
  )
}

.projr_remote_get_version_file_non_project <- function(type,
                                                       remote_pre) {
  path_dir_save <- .dir_create_tmp_random()
  path_version <- .projr_remote_file_get_ind(
    type, remote_pre, "VERSION", path_dir_save
  )
  version_file <- .projr_remote_get_version_file_read(path_version)
  unlink(path_dir_save, recursive = TRUE)
  version_file
}

.projr_remote_get_version_file_read <- function(path) {
  if (!.is_string(path) || !file.exists(path)) {
    return(character(0L))
  }
  readLines(path, warn = FALSE)
}

# ========================
# Get latest version of a particular label from a remote
# ========================

.projr_remote_get_version_label <- function(remote_pre,
                                            type,
                                            label,
                                            structure) {
  if (type == "project") {
    .projr_remote_get_version_project()
  } else {
    .projr_remote_get_version_label_non_project(
      remote_pre, type, label, structure
    )
  }
}

.projr_remote_get_version_project <- function() {
  .projr_version_get() |> .projr_version_v_rm()
}

.projr_remote_get_version_label_non_project <- function(remote_pre, # nolint
                                                        type,
                                                        label,
                                                        structure) {

  if (structure == "archive") {
    version_archive <- .projr_remote_get_version_label_non_project_archive(
      remote_pre, type, label, structure
    )
    if (
      !.is_string(version_archive) ||
        !.projr_version_check_error_free(version_archive)
      ) { # nolint
      return(character(0L))
    }
  } else {
    version_archive <- NULL
  }

  # use the versioned files (raw-data-project: v1.0.0)
  version_file <- .projr_remote_get_version_label_non_project_file(
    remote_pre, type, label
  )
  # if it's not correctly formatted (which may happen because
  # it's not found) or if it does not match version_archive,
  # then return nothing
  if (!.projr_version_check_error_free(version_file)) {
    return(character(0L))
  }
  if (!identical(version_archive, version_file)) {
    return(character(0L))
  }

  version_thus_far <- version_archive

  # check that the manifest matches
  manifest_project <- .projr_remote_get_manifest_project() |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(version_thus_far)
  manifest_remote <- .projr_remote_get_manifest(type, remote_pre) |>
    .projr_manifest_filter_label(label) |>
    .projr_manifest_filter_version(version_thus_far)
  rownames(manifest_project) <- NULL
  rownames(manifest_remote) <- NULL
  if (!identical(manifest_project, manifest_remote)) {
    return(character(0L))
  }

  version_thus_far
}

.projr_remote_get_version_label_non_project_archive <- function(remote_pre,
                                                                type,
                                                                label,
                                                                structure) {
  if (structure != "archive") {
    return(NULL)
  }
  remote_final_vec_basename <- .projr_remote_final_ls(
    type, remote_pre
  )
  .projr_remote_version_latest_get(remote_final_vec_basename, type, label) |>
    .projr_version_v_rm()
}

.projr_remote_version_latest_get <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  fn <- .projr_remote_version_latest_filter(fn, type, label)
  .projr_remote_version_latest_extract(fn, label)
}

.projr_remote_version_latest_filter <- function(fn, type, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  version_format_regex_dev_n <- .projr_remote_version_latest_filter_get_regex(
    type, label
  )
  grep(version_format_regex_dev_n, fn, value = TRUE)
}

.projr_remote_version_latest_filter_get_regex <- function(type, label)  {
  version_format <- .projr_yml_metadata_get_version_format(NULL)
  version_format_regex <- gsub("major", "\\\\d\\+", version_format)
  version_format_regex <- gsub("minor", "\\\\d\\+", version_format_regex)
  version_format_regex <- gsub("patch", "\\\\d\\+", version_format_regex)
  version_format_regex <- gsub("\\.dev$|\\-dev", "", version_format_regex)
  version_format_regex <- paste0(label, "-v", version_format_regex)
  if (type == "github") {
    version_format_regex <- paste0(version_format_regex, ".zip")
  }
  utils::glob2rx(version_format_regex)
}

.projr_remote_version_latest_extract <- function(fn, label) {
  if (.is_len_0(fn)) {
    return(character(0L))
  }
  fn_no_zip <- sub("\\.zip$", "", fn)
  version_vec <- sub(".*-v(.*)", "\\1", fn_no_zip)
  version_vec <- setdiff(version_vec, "")
  if (.is_len_0(version_vec)) {
    return(character(0L))
  }
  version_format_correct <- try(
    vapply(
      version_vec,
      function(x) .projr_version_format_check(x), logical(1)
    ) |>
      all(),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(character(0L))
  }
  .projr_version_get_latest(version_vec)
}

.projr_remote_get_version_label_non_project_file <- function(remote_pre,
                                                             type,
                                                             label) {
  version_file <- .projr_remote_get_version_file(type, remote_pre)
  .projr_remote_get_version_label_non_project_file_extract(
    version_file, label
  )
}

.projr_remote_get_version_label_non_project_file_extract <-
  function(version_file,
           label) {
    match_str <- utils::glob2rx(label) |>
      gsub("\\$", "", x = _) |>
      paste0(": ")
    label_regex <- grep(match_str, version_file, value = TRUE)
    if (.is_len_0(label_regex)) {
      return(character(0L))
    }
    # return character() if ends in an asterisk
    if (grepl("\\*$", label_regex)) {
      return(character(0L))
    }
    gsub(match_str, "", label_regex) |>
      trimws() |>
      .projr_version_v_rm()
  }



# ==========================
# Get the most recent remote
# ==========================

.projr_remote_get_recent <- function(remote_final,
                                     type) {
  switch(type,
    "local" = .projr_remote_get_recent_local(remote_final),
    "osf" = .projr_remote_get_recent_osf(remote_final),
    "github" = .projr_remote_get_recent_github(remote_final)
  )
}

.projr_remote_get_recent_local <- function(remote_final) {
  stop("Not defined yet")
}

.projr_remote_get_recent_osf <- function(remote_final) {
  stop("Not defined yet")
}

.projr_remote_get_recent_github <- function(remote_final) {
  stop("Not defined yet")
}

# ========================
# Detect whether a remote is version or latest
# based on the remote itself
# ========================

.projr_remote_detect_structure <- function(remote, type) {
  switch(type,
    "local" = .projr_remote_detect_structure_local(remote),
    "osf" = .projr_remote_detect_structure_osf(remote),
    "github" = .projr_remote_detect_structure_github(remote)
  )
}

.projr_remote_detect_structure_local <- function(remote) {
  version_format_correct <- try(
    .projr_version_format_check(basename(remote)),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "version"
}

.projr_remote_detect_structure_osf <- function(remote) {
  version_format_correct <- try(
    remote[["name"]][[1]],
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return("latest")
  }
  "version"
}

.projr_remote_detect_structure_github <- function(remote) {
  .projr_dep_install("piggyback")
  version_remote <- .projr_version_get_remote_github(remote)
  if (is.null(version_remote)) "latest" else "version"
}

.projr_version_get_remote <- function(remote, type) {
  switch(type,
    "github" = .projr_version_get_remote_github(remote),
    "local" = .projr_version_get_remote_local(remote),
    stop("type not recognized")
  )
}

.projr_version_get_remote_local <- function(remote) {
  version <- basename(remote)
  version_format_correct <- try(
    .projr_version_format_check(version),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(NULL)
  }
  version
}

.projr_version_get_remote_github <- function(remote) {
  version <- sub(".*-v(.*)\\.zip$", "\\1", remote[["fn"]])
  version_format_correct <- try(
    .projr_version_format_check(version),
    silent = TRUE
  )
  if (inherits(version_format_correct, "try-error")) {
    return(NULL)
  }
  version
}

# ========================
# List all final remotes in a particular pre-remote
# ========================

.projr_remote_final_ls <- function(type,
                                   remote_pre) {
  .assert_in(type, .projr_opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .projr_remote_final_ls_local(remote_pre),
    "osf" = .projr_remote_final_ls_osf(remote_pre),
    "github" = .projr_remote_final_ls_github(remote_pre)
  )
}

.projr_remote_final_ls_local <- function(remote_pre) {
  list.dirs(remote_pre, full.names = FALSE, recursive = FALSE)
}

.projr_remote_final_ls_osf <- function(remote_pre) {
  .assert_given_full(remote_pre)
  osf_tbl_file <- remote_pre |> .projr_osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character())
  }
  dir_vec_int <- which(.projr_osf_is_dir(osf_tbl_file))
  osf_tbl_file[["name"]][dir_vec_int]
}

.projr_remote_final_ls_github <- function(remote_pre) {
  .assert_given_full(remote_pre)
  .projr_dep_install("piggyback")
  pb_tbl <- piggyback::pb_list(tag = remote_pre[["tag"]])
  if ("file_name" %in% names(pb_tbl)) {
    fn_vec <- pb_tbl[["file_name"]]
    setdiff(fn_vec, "")
  } else {
    character(0L)
  }
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
                                      path_dir_parent = NULL,
                                      fn_vec = character(),
                                      recurse = TRUE) {
  # this function is to be applied to every directory.
  # it does the following:
  # 1. Lists all the files
  # 2. Steps into each directory, and recurses, listing all the
  #    files and then stepping into each sub-directory, and so on.
  .assert_given_full(remote)
  .assert_chr(path_dir_parent)
  osf_tbl_file <- remote |> .projr_osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character())
  }
  # add all files
  fn_vec_fn <- .projr_remote_file_ls_osf_fn(
    osf_tbl_file = osf_tbl_file,
    path_dir_parent = path_dir_parent
  )
  # recurse through directories
  fn_vec_dir <- .projr_remote_file_ls_osf_dir(
    remote = remote,
    osf_tbl_file = osf_tbl_file,
    path_dir_parent = path_dir_parent,
    fn_vec = fn_vec
  )
  c(fn_vec_fn, fn_vec_dir) |> unique()
}

.projr_remote_file_ls_osf_fn <- function(osf_tbl_file,
                                         path_dir_parent) {
  dir_vec_ind <- .projr_osf_is_dir(osf_tbl_file)
  if (all(dir_vec_ind)) {
    return(NULL)
  }
  fn_vec_fn <- osf_tbl_file[["name"]][!dir_vec_ind]
  if (!is.null(path_dir_parent)) {
    fn_vec_fn <- file.path(path_dir_parent, fn_vec_fn)
  }
  fn_vec_fn
}

.projr_remote_file_ls_osf_dir <- function(remote,
                                          osf_tbl_file,
                                          path_dir_parent,
                                          fn_vec) {
  dir_vec_int <- which(.projr_osf_is_dir(osf_tbl_file))
  if (!any(.projr_osf_is_dir(osf_tbl_file))) {
    return(NULL)
  }
  path_dir_osf <- osf_tbl_file[["name"]][dir_vec_int]
  # if there are directories, go through them
  .projr_remote_file_ls_osf_dir_non_null(
    remote = remote,
    path_dir_osf = path_dir_osf,
    path_dir_parent = path_dir_parent,
    fn_vec = fn_vec
  )
}

.projr_remote_file_ls_osf_dir_non_null <- function(remote,
                                                   path_dir_osf,
                                                   path_dir_parent,
                                                   fn_vec) {
  for (i in seq_along(path_dir_osf)) {
    fn_vec_add <- .projr_remote_file_ls_osf_dir_non_null_ind(
      path_dir_osf = path_dir_osf[[i]],
      remote = remote,
      path_dir_parent = path_dir_parent,
      fn_vec = fn_vec
    )
    fn_vec <- c(fn_vec, fn_vec_add)
  }
  fn_vec
}

.projr_remote_file_ls_osf_dir_non_null_ind <- function(path_dir_osf,
                                                       remote,
                                                       path_dir_parent,
                                                       fn_vec) {
  if (!is.null(path_dir_parent)) {
    path_dir_parent_curr <- file.path(
      basename(path_dir_parent), path_dir_osf
    )
  } else {
    path_dir_parent_curr <- path_dir_osf
  }
  # recurse into directory
  fn_vec_ind <- .projr_remote_file_ls_osf(
    remote = .projr_osf_mkdir(x = remote, path = path_dir_osf),
    path_dir_parent = path_dir_parent_curr, fn_vec = fn_vec
  )
  if (length(fn_vec_ind > 0L)) {
    fn_vec <- c(fn_vec, fn_vec_ind)
  }
  fn_vec
}


# github
.projr_remote_file_ls_github <- function(remote) {
  .projr_dep_install("piggyback")
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
  .assert_chr_min(fn, TRUE)
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
  osf_tbl_rm_file <- osf_tbl_rm |> .projr_osf_ls_files(n_max = Inf)
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
    node <- .projr_osf_mkdir(x = node, path = path)
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
    .projr_osf_rm(x = osf_tbl, check = FALSE, recurse = FALSE)
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
    .projr_osf_rm(x = osf_tbl_file_ind, check = FALSE, recurse = FALSE)
  }
}

# github
.projr_remote_file_rm_github <- function(fn,
                                         remote) {
  .projr_dep_install("piggyback")
  .assert_chr_min(fn, TRUE)
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
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
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
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
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
      osf_tbl_upload <- .projr_osf_mkdir(x = remote, path = x)
    } else {
      osf_tbl_upload <- remote
    }
    .projr_osf_upload(
      x = osf_tbl_upload,
      path = file.path(
        path_dir_local, plot_tbl[["fn"]][plot_tbl[["dir"]] == x]
      )
    )
  }
  invisible(TRUE)
}

# github
.projr_remote_file_add_github <- function(fn, # nolint: cyclocomp_linter.
                                          path_dir_local,
                                          remote) {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_given_full(remote)
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
  .projr_dep_install("piggyback")
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
  setting_git <-
    .projr_yml_build_get(NULL)[["git"]]
  switch(class(setting_git),
    "NULL" = TRUE,
    "logical" = setting_git,
    list = {
      setting_push <- setting_git[["push"]]
      if (is.null(setting_push)) TRUE else setting_push
    },
    stop(paste0("git setting '", class(setting_git), "' not recognized"))
  )
}


.projr_remote_misc_get_github_tag <- function(x) {
  .assert_given_full(x)
  if (!"tag" %in% names(x)) {
    .assert_string(x)
    tag <- x
  } else {
    tag <- x[["tag"]]
  }
  tag <- gsub("\\s", "-", tag)
  if (tag == "@version") .projr_version_get_v() else tag
}
