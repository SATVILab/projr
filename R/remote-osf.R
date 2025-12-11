# ========================
# check existence
# ========================

# osf
.remote_check_exists_osf <- function(id) {
  .assert_nchar_single(id, 5L, TRUE)
  !.is_try_error(.remote_get(type = "osf", id = id))
}

# ========================
# check existence of final remote
# ========================

.remote_final_check_exists_osf <- function(remote_pre,
                                           structure,
                                           label,
                                           version) {
  dir_basename <- if (structure == "archive") {
    version |> .version_v_add()
  } else {
    label
  }
  osf_tbl_file <- remote_pre |> osfr::osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(FALSE)
  }
  dir_basename %in% osf_tbl_file[["name"]]
}

# ========================
# create remote
# ========================

# osf
.remote_create_osf <- function(title,
                               id_parent = NULL,
                               category = NULL,
                               description = NULL,
                               public = FALSE) {
  .dep_install("osfr")
  .auth_check_osf("creating OSF node")
  category <- .remote_complete_osf_category(
    category = category
  )
  .assert_string(title, TRUE)
  .assert_string(id_parent)
  .assert_nchar(id_parent, 5L)
  .assert_in(category, .opt_remote_get_osf_cat())
  .assert_string(description)
  .assert_flag(public, TRUE)

  .remote_create_osf_node(
    title = title,
    id_parent = id_parent,
    category = category,
    description = description,
    public = public
  )
}

.remote_create_osf_node <- function(title,
                                    id_parent,
                                    category,
                                    description,
                                    public) {
  switch(category,
    "project" = .remote_create_osf_project(
      title = title,
      description = description,
      public = public
    ),
    .remote_create_osf_component(
      title = title,
      id_parent = id_parent,
      category = category,
      description = description,
      public = public
    )
  )
}

.remote_create_osf_project <- function(title,
                                       description,
                                       public) {
  id <- try(.osf_create_project(
    title = title,
    description = description,
    public = public,
    category = "project"
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}


.osf_is_dir <- function(x) {
  if (nrow(x) == 0L) {
    return(logical(0))
  }
  vapply(seq_len(nrow(x)), function(i) {
    x$meta[[i]][["attributes"]][["kind"]] == "folder"
  }, logical(1))
}

.osf_ls_files <- function(osf_tbl,
                          path_dir_parent = NULL) {
  osf_tbl_file <- osf_tbl |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  dir_vec_ind <- .osf_is_dir(osf_tbl_file)
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
      fn_vec_dir_ind <- .osf_ls_files(
        osf_tbl = .osf_mkdir(x = osf_tbl, path = path_dir_osf),
        path_dir_parent = path_dir_parent_curr
      )
      if (length(fn_vec_dir_ind > 0L)) {
        fn_vec_dir <- c(fn_vec_dir, fn_vec_dir_ind)
      }
    }
  }
  c(fn_vec_fn, fn_vec_dir) |> unique()
}

.remote_complete_osf_category <- function(category) {
  if (!is.null(category)) category else "project"
}
.remote_complete_osf_public <- function(public) {
  if (!is.null(public)) public else FALSE
}

.osf_upload <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("uploading to OSF")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_upload,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_create_project <- function(..., n_try = NULL, n_sleep = 3) {
  .dep_install("osfr")
  .auth_check_osf("creating OSF project")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_create_project,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_create_component <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("creating OSF component")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_create_component,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_retrieve_node <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("retrieving OSF node")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_retrieve_node,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_mkdir <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("creating OSF directory")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_mkdir,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_ls_files <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("listing OSF files")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_ls_files,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_rm <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("removing OSF files")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_rm,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_download <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("downloading from OSF")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_download,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_retrieve_user <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("retrieving OSF user")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_retrieve_user,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
}

.osf_ls_nodes <- function(..., n_try = NULL, n_sleep = 3) {
  .auth_check_osf("listing OSF nodes")
  args_list <- list(...)
  .try_repeat(
    osfr::osf_ls_nodes,
    args_list,
    n_try = n_try,
    n_sleep = n_sleep
  )
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
  id <- .remote_create_osf_project(
    title = title,
    description = description,
    public = public
  )
  if (!.is_string(id)) {
    stop(paste0("Failed to create OSF project"))
  }
  id
}

.remote_create_osf_component <- function(title,
                                         id_parent,
                                         category,
                                         description,
                                         public) {
  id <- try(.osf_create_component(
    x = .remote_get_osf(id = id_parent),
    title = title,
    description = description,
    public = public,
    category = category
  )[["id"]])
  if (inherits(id, "try-error")) character() else id
}

.remote_create_osf_component_check_id_parent <- function(id_parent) {
  id_parent <- try(force(id_parent), silent = TRUE)
  if (is.null(id_parent) || inherits(id_parent, "try-error")) {
    # if the OSF category is not a project,
    # then the parent id must be supplied.
    # here we tell them that, and tell them to
    # either create it directly on OSF or
    # use the .osf_create_project` function, seeing
    # `.osf_create_project` for details.
    stop(paste0(
      "The parent ID must be supplied if the OSF category is not a project.", # nolint
      "\n",
      "Please create the project directly on OSF or use .osf_create_project`." # nolint
    ), call. = FALSE)
  }
}

# ========================
# Get remote
# =======================

.remote_get_osf <- function(id) {
  .assert_nchar_single(id, 5L, TRUE)
  .auth_check_osf("retrieving OSF node")
  tryCatch(
    .osf_retrieve_node(paste0("https://osf.io/", id)),
    error = function(e) {
      stop(paste0(
        "Could not retrieve OSF node (project/component):", id
      ))
    }
  )
}

# ========================
# list all directories in pre-remote
# ========================

.remote_ls_final_osf <- function(remote_pre) {
  .assert_given_full(remote_pre)
  osf_tbl_file <- remote_pre |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character())
  }
  dir_vec_int <- which(.osf_is_dir(osf_tbl_file))
  osf_tbl_file[["name"]][dir_vec_int]
}

# ========================
# Get final remote
# =======================


.remote_final_get_osf <- function(id,
                                  path,
                                  path_append_label,
                                  label,
                                  structure,
                                  version = NULL,
                                  pre = NULL,
                                  empty = NULL) {
  .assert_nchar_single(id, 5L, TRUE)
  .assert_string(path)
  .assert_flag(path_append_label)
  .assert_in(label, .opt_dir_get_label_send(NULL))
  .assert_in_single(structure, .opt_remote_get_structure())
  if (!is.null(pre) && isTRUE(pre)) {
    stop(paste0("pre not supported for OSF"))
  }
  label <- .remote_final_get_osf_get_label(
    label, path_append_label
  )
  path_rel <- .remote_get_path_rel(
    type = "osf",
    path = path,
    path_append_label = path_append_label,
    label = label,
    structure = structure,
    version = version,
    pre = FALSE,
    empty = empty
  )
  osf_tbl <- .remote_get(id = id, type = "osf")
  if (length(path_rel) > 0L) {
    osf_tbl <- .osf_mkdir(osf_tbl, path_rel)
  }
  osf_tbl
}

.remote_final_get_osf_get_label <- function(label, path_append_label) {
  if (missing(label)) {
    if (!path_append_label) {
      label <- "abc"
    } else {
      stop(paste0("label must be supplied if to be appended"))
    }
  }
  label
}

.remote_final_check_exists_direct_osf <- function(remote) {
  stop(paste0("Direct final remote existence check not supported for OSF"))
}

# ========================
# Delete an unused empty remote directory
# ========================

# osf
.remote_final_rm_if_empty_osf <- function(remote, output_level = "std") {
  .assert_given_full(remote)

  # Only process if it's a file/directory object (not a node)
  if (!inherits(remote, "osf_tbl_file")) {
    .cli_debug(
      "OSF remote: Not an osf_tbl_file, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  # Infer structure from the remote's name
  # If name starts with "v" followed by version pattern, it's archive
  remote_name <- remote[["name"]]
  if (is.null(remote_name) || length(remote_name) == 0L || !nzchar(remote_name)) {
    .cli_debug(
      "OSF remote: No name found, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  # Check if name matches version pattern (e.g., "v0.0.1", "v0.0.0-1")
  # Archive directories have names starting with "v" followed by a digit
  is_archive <- grepl("^v[0-9]", remote_name)

  if (!is_archive) {
    .cli_debug(
      "OSF remote: Name '{remote_name}' does not match archive pattern, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  # Check if empty
  osf_files <- .osf_ls_files(remote)
  if (nrow(osf_files) > 0L) {
    num_files <- nrow(osf_files)
    .cli_debug(
      "OSF remote: Directory '{remote_name}' has {num_files} file(s), not removing, returning FALSE",
      output_level = output_level
    )
    return(invisible(FALSE))
  }

  .cli_debug(
    "OSF remote: Directory '{remote_name}' is empty archive directory, removing it",
    output_level = output_level
  )
  .osf_rm(x = remote, check = FALSE)
  invisible(TRUE)
}

# ========================
# Delete a final remote
# ========================

.remote_final_rm_osf <- function(remote, output_level = "std") {
  stop("Final remote deletion not supported for OSF")
}


# ========================
# Empty a final remote
# ========================

.remote_final_empty_osf <- function(remote) {
  .assert_given_full(remote)
  osf_tbl_file <- remote |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    .osf_rm(x = osf_tbl_file[i, ], recurse = TRUE, check = FALSE)
  }
  invisible(TRUE)
}

# ========================
# Download all files
# ========================

.remote_file_get_all_osf <- function(remote,
                                     path_dir_save_local) {
  .assert_given_full(remote)
  osf_tbl_file <- remote |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(invisible(path_dir_save_local))
  }
  for (i in seq_len(nrow(osf_tbl_file))) {
    .osf_download(
      x = osf_tbl_file[i, ],
      path = path_dir_save_local,
      check = FALSE
    )
  }
  invisible(path_dir_save_local)
}

# ========================
# Download a single file
# ========================


.remote_file_get_osf <- function(remote,
                                 fn,
                                 path_dir_save_local) {
  # TODO: Add a check that this isn't an empty remote
  # (ends with -empty; not sure how you do that here)
  .assert_given_full(remote)
  osf_tbl_file <- remote |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character(0L))
  }
  if (!fn %in% osf_tbl_file[["name"]]) {
    return(character(0L))
  }
  .osf_download(
    x = osf_tbl_file[osf_tbl_file[["name"]] == fn, ],
    path = path_dir_save_local,
    check = FALSE
  )
  path_fn <- file.path(path_dir_save_local, fn)
  if (file.exists(path_fn)) path_fn else character(0L)
}

# ========================
# List all contents
# ========================

.remote_file_ls_osf <- function(remote,
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
  osf_tbl_file <- remote |> .osf_ls_files(n_max = Inf)
  if (nrow(osf_tbl_file) == 0L) {
    return(character())
  }
  # add all files
  fn_vec_fn <- .remote_file_ls_osf_fn(
    osf_tbl_file = osf_tbl_file,
    path_dir_parent = path_dir_parent
  )
  # recurse through directories
  fn_vec_dir <- .remote_file_ls_osf_dir(
    remote = remote,
    osf_tbl_file = osf_tbl_file,
    path_dir_parent = path_dir_parent,
    fn_vec = fn_vec
  )
  c(fn_vec_fn, fn_vec_dir) |> unique()
}

.remote_file_ls_osf_fn <- function(osf_tbl_file,
                                   path_dir_parent) {
  dir_vec_ind <- .osf_is_dir(osf_tbl_file)
  if (all(dir_vec_ind)) {
    return(NULL)
  }
  fn_vec_fn <- osf_tbl_file[["name"]][!dir_vec_ind]
  if (!is.null(path_dir_parent)) {
    fn_vec_fn <- file.path(path_dir_parent, fn_vec_fn)
  }
  fn_vec_fn
}

.remote_file_ls_osf_dir <- function(remote,
                                    osf_tbl_file,
                                    path_dir_parent,
                                    fn_vec) {
  dir_vec_int <- which(.osf_is_dir(osf_tbl_file))
  if (!any(.osf_is_dir(osf_tbl_file))) {
    return(NULL)
  }
  path_dir_osf <- osf_tbl_file[["name"]][dir_vec_int]
  # if there are directories, go through them
  .remote_file_ls_osf_dir_non_null(
    remote = remote,
    path_dir_osf = path_dir_osf,
    path_dir_parent = path_dir_parent,
    fn_vec = fn_vec
  )
}

.remote_file_ls_osf_dir_non_null <- function(remote,
                                             path_dir_osf,
                                             path_dir_parent,
                                             fn_vec) {
  for (i in seq_along(path_dir_osf)) {
    fn_vec_add <- .remote_file_ls_osf_dir_non_null_ind(
      path_dir_osf = path_dir_osf[[i]],
      remote = remote,
      path_dir_parent = path_dir_parent,
      fn_vec = fn_vec
    )
    fn_vec <- c(fn_vec, fn_vec_add)
  }
  fn_vec
}

.remote_file_ls_osf_dir_non_null_ind <- function(path_dir_osf,
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
  fn_vec_ind <- .remote_file_ls_osf(
    remote = .osf_mkdir(x = remote, path = path_dir_osf),
    path_dir_parent = path_dir_parent_curr, fn_vec = fn_vec
  )
  if (length(fn_vec_ind > 0L)) {
    fn_vec <- c(fn_vec, fn_vec_ind)
  }
  fn_vec
}

# ========================
# Delete individual files
# ========================

.remote_file_rm_osf <- function(fn,
                                remote,
                                rm_if_empty = TRUE,
                                output_level = "std") {
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
  dir_vec <- dir_vec[order(.dir_count_lvl(dir_vec), decreasing = TRUE)]
  vapply(
    dir_vec, .remote_file_rm_osf_dir, logical(1),
    osf_tbl = remote, fn = fn
  )
  if (rm_if_empty) {
    .cli_debug(
      "Checking to remove empty OSF directories after file removal",
      output_level = output_level
    )
    .remote_final_rm_if_empty(
      "osf", remote
    )
  }
  invisible(TRUE)
}

.remote_file_rm_osf_dir <- function(dir, fn, osf_tbl) {
  osf_tbl_rm <- .remote_file_rm_osf_rm_get(
    path = dir, node = osf_tbl
  )
  osf_tbl_rm_file <- osf_tbl_rm |> .osf_ls_files(n_max = Inf)
  fn_vec_dir <- basename(fn)[dirname(fn) == dir]
  .remote_file_rm_osf_detailed(
    fn_dir = fn_vec_dir, osf_tbl = osf_tbl_rm, osf_tbl_file = osf_tbl_rm_file
  )
}

.remote_file_rm_osf_rm_get <- function(path,
                                       node) {
  .assert_string(path, TRUE)
  .assert_given_full(node)
  if (path != ".") {
    node <- .osf_mkdir(x = node, path = path)
  }
  node
}

.remote_file_rm_osf_detailed <- function(fn_dir,
                                         osf_tbl,
                                         osf_tbl_file) {
  .assert_chr(fn_dir, TRUE)
  .assert_given_full(osf_tbl)

  fn_vec_osf <- osf_tbl_file[["name"]]
  # might be faster to just delete the whole directory
  remove_dir <- setequal(fn_dir, fn_vec_osf) &&
    inherits(osf_tbl, "osf_tbl_file")
  if (remove_dir) {
    .osf_rm(x = osf_tbl, check = FALSE, recurse = FALSE)
    return(invisible(TRUE))
  }
  fn_vec_to_rm <- fn_vec_osf[fn_vec_osf %in% fn_dir]
  if (length(fn_vec_to_rm) == 0L) {
    return(invisible(FALSE))
  }
  .assert_given_full(osf_tbl_file)
  .remote_file_rm_osf_fn(
    fn_rm = fn_vec_to_rm, osf_tbl_file = osf_tbl_file
  )
  invisible(TRUE)
}

.remote_file_rm_osf_fn <- function(fn_rm, osf_tbl_file) {
  .assert_chr(fn_rm, TRUE)
  .assert_given_full(osf_tbl_file)
  # osfr requires deleting individual files one-by-one
  # by passing a table
  for (i in seq_along(fn_rm)) {
    osf_tbl_file_ind <- osf_tbl_file[
      osf_tbl_file[["name"]] == fn_rm[[i]],
    ]
    .osf_rm(x = osf_tbl_file_ind, check = FALSE, recurse = FALSE)
  }
}

# ========================
# Add individual files
# ========================

.remote_file_add_osf <- function(fn,
                                 path_dir_local,
                                 remote) {
  .assert_chr_min(fn, TRUE)
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_string(path_dir_local, TRUE)
  .assert_path_not_file(path_dir_local)
  .assert_given_full(remote)
  .dep_install("osfr")
  plot_tbl <- data.frame(fn = fn, dir = dirname(fn))
  dir_vec <- unique(plot_tbl[["dir"]])
  for (x in dir_vec) {
    if (x != ".") {
      osf_tbl_upload <- .osf_mkdir(x = remote, path = x)
    } else {
      osf_tbl_upload <- remote
    }
    .osf_upload(
      x = osf_tbl_upload,
      path = file.path(
        path_dir_local, plot_tbl[["fn"]][plot_tbl[["dir"]] == x]
      )
    )
  }
  invisible(TRUE)
}

# ========================
# Delete a remote
# ========================

.remote_rm_osf <- function(remote) {
  
}

# ========================
# Delete a final remote
# ========================
