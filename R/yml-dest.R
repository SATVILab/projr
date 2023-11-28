#' @title Add a remote as a destination
#'
#' @description
#'
#' @param type "github", "osf" or "local".
#' Type of remote to create.
#' Selecting "github" means a GitHub release,
#' "osf" means an OSF node (project or component),
#' and "local" means a local directory.
#' Must be selected.
#' @param title character.
#' Title of the remote.
#' in which case the GitHub releases's tag
#' will the project version (i.e. the result of `projr_version_get()`)
#' at the time of the upload.
#' Has no effect for `local` remotes.
#' For GitHub releases, can use title as `@version`,
#' Note that this implies that a new tag will be created with each
#' new version, so do not use with large files.
#' If not supplied, then will
#' automatically be generated from `content`.
#' @param description character.
#' Description of the remote.
#' Has no effect for `local` remotes.
#' Default is `NULL`.
#' @param content character vector.
#' Labels in the `directories` key of `projr_yml_get()`
#' to send to the remote e.g. `data-raw`, `cache`, `output`.
.projr_dest_add <- function(role,
                            type,
                            title = NULL,
                            content = NULL,
                            structure = NULL,
                            path = NULL,
                            path_append_label = NULL,
                            overwrite = FALSE,
                            public = FALSE,
                            category = NULL,
                            description = NULL,
                            id = NULL,
                            id_parent = NULL,
                            title_parent = NULL,
                            get_sync_approach = NULL,
                            get_conflict = NULL,
                            send_cue = NULL,
                            send_sync_approach = NULL,
                            send_version_source = NULL,
                            send_conflict = NULL) {
  # store original values
  # ---------------------

  yml_projr_orig_root <- .projr_yml_get_root_default()
  yml_projr <- projr_yml_get_unchecked()

  # gather get and send args
  # ------------------------
  get_list <- .projr_yml_remote_transfer_get(
    sync_approach = get_sync_approach,
    conflict = get_conflict
  )

  send_list <- .projr_yml_remote_transfer_get(
    cue = send_cue,
    sync_approach = send_sync_approach,
    version_source = send_version_source,
    conflict = send_conflict
  )

  # check inputs
  # ------------

  .projr_yml_remote_check(
    role = role,
    type = type,
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    overwrite = overwrite,
    public = public,
    category = category,
    description = description,
    id = id,
    id_parent = id_parent,
    title_parent = title_parent,
    get_list = get_list,
    send_list = send_list
  )

  # add
  # -----------------
  .projr_dest_add_add(
    type = type,
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list,
    overwrite = overwite,
    public = public,
    category = category,
    description = description,
    id = id,
    id_parent = id_parent,
    title_parent = title_parent
  )
}

# add now
.projr_dest_add_add <- function(type,
                                title,
                                content,
                                structure,
                                path,
                                path_append_label,
                                get_list,
                                send_list,
                                overwrite,
                                public,
                                category,
                                description,
                                id,
                                id_parent,
                                title_parent) {
  # get list to add
  # ---------------
  list_add <- .projr_dest_add_get_list_add(
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list
  )

  switch(type,
    "osf" = .projr_dest_add_add_osf(),
    .projr_dest_add_add_default(
      type = type, list_add = list_add
    )
  )
}

# get list to add
.projr_dest_add_get_list_add <- function(title,
                                         content,
                                         structure,
                                         path,
                                         path_append_label,
                                         get_list,
                                         send_list) {
  list_add <- list()
  # get title
  title <- .projr_yml_remote_title_get(title = title, content = content)
  if (!is.null(content)) {
    list_add[["content"]] <- content
  }
  if (!is.null(path)) {
    list_add[["path"]] <- path
  }
  if (!is.null(path_append_label)) {
    list_add[["path_append_label"]] <- path_append_label
  }
  if (!is.null(structure)) {
    list_add[["structure"]] <- structure
  }
  if (!length(get_list) == 0L) {
    list_add[["download"]] <- get_list
  }
  if (!length(send_list) == 0L) {
    list_add[["upload"]] <- send_list
  }
  list(list_add) |> stats::setNames(title)
}

# -----------------
# default
# -----------------

.projr_dest_add_add_default <- function(type,
                                        list_add) {
  yml_projr_root <- .projr_yml_get_root_default()
  if (!type %in% names(yml_projr_root[["build"]])) {
    yml_projr_root[["build"]][[type]] <- list_add
  } else {
    yml_projr_root_type <- yml_projr_root[["build"]][type]
    yml_projr_root_type[[type]] <- yml_projr_root_type[[type]] |>
      append(list_add)
    yml_projr_root[["build"]][type] <- yml_projr_root_type
  }
  .projr_yml_set_root(yml_projr_root)
}

# ===================
# osf
# ===================

# see yml-dest-osf.R
