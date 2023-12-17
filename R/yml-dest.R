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
.projr_yml_dest_add <- function(role,
                                type,
                                title,
                                profile,
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
  .projr_yml_dest_add_actual(
    type = type,
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list,
    overwrite = overwrite,
    public = public,
    category = category,
    description = description,
    id = id,
    id_parent = id_parent,
    title_parent = title_parent,
    profile = profile
  )
}

# add now
.projr_yml_dest_add_actual <- function(type,
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
                                       profile) {
  title <- .projr_yml_remote_title_get(title = title, content = content)
  .projr_yml_dest_add_get_list_add(
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list
  ) |>
    .projr_yml_dest_add_get_list_add_extra() |>
    .projr_yml_dest_set_title(
      title = title, type = type, profile = profile
    )
}

# get list to add
.projr_yml_dest_add_get_list_add <- function(content,
                                             path,
                                             path_append_label,
                                             structure,
                                             get_list,
                                             send_list) {
  list() |>
    .projr_list_add(content) |>
    .projr_list_add(path) |>
    .projr_list_add(path_append_label) |>
    .projr_list_add(structure) |>
    .projr_list_add(get_list, nm = "get") |>
    .projr_list_add(send_list, nm = "send")
}


.projr_yml_dest_add_get_list_add_extra <- function(list_add,
                                                   type,
                                                   id,
                                                   id_parent,
                                                   category,
                                                   public,
                                                   description) {
  switch(type,
    "osf" = .projr_yml_dest_add_get_list_add_extra_osf(
      list_add = list_add, id = id, id_parent = id_parent,
      category = category, public = public, description = description
    ),
    list_add
  )
}

.projr_yml_dest_add_get_list_add_extra_osf <- function(list_add,
                                                       id,
                                                       id_parent,
                                                       category,
                                                       public,
                                                       description) {
  switch(class(id)[[1]],
    "character" = .projr_yml_dest_add_get_list_add_extra_osf_id_chr(
      list_add, id
    ),
    "NULL" = .projr_yml_dest_add_get_list_add_extra_osf_id_null(
      list_add = list_add,
      id_parent = id_parent,
      category = category,
      public = public,
      description = description
    ),
    stop(
      paste0("id must be character or NULL, not ", class(id)[[1]]),
      call. = FALSE
    )
  )
}

.projr_yml_dest_add_get_list_add_extra_osf_id_chr <- function(list_add,
                                                              id) {
  if (!.projr_yml_dest_add_get_list_add_extra_osf_id_chr_check(id)) {
    stop(
      paste0("id ", id, " does not exist on OSF"),
      call. = FALSE
    )
  }
  list_add |>
    .projr_list_add(id)
}

.projr_yml_dest_add_get_list_add_extra_osf_id_chr_check <- function(id) {
  !inherits(.projr_remote_get("osf", id), "try-error")
}

.projr_yml_dest_add_get_list_add_extra_osf_id_null <- function(list_add,
                                                               title,
                                                               id_parent,
                                                               category,
                                                               description,
                                                               public) {
  id <- .projr_remote_create(
    type = "osf",
    name = title,
    id_parent = id_parent,
    category = category,
    public = public,
    description = description
  )
  if (.projr_state_z(id)) {
    stop(
      "Failed to create OSF node",
      call. = FALSE
    )
  }
  list_add |>
    .projr_list_add(id)
}



# -----------------
# default
# -----------------

.projr_yml_dest_add_add_default <- function(type,
                                            list_add,
                                            profile) {
  .projr_yml_dest_set_title(
    type = type,
    title = names(list_add),
    profile = profile
  )
}

.projr_yml_dest_rm_title <- function(title, type, profile) {
  yml_type <- .projr_yml_dest_get_type(type, profile)
  yml_type[[title]] <- NULL
  .projr_yml_dest_set_type(yml_type, type, profile)
}

# removing
.projr_yml_dest_rm_type <- function(type, profile) {
  .projr_yml_dest_set_type(NULL, type, profile)
}

.projr_yml_dest_rm_type_all <- function(profile) {
  type_vec <- .projr_yml_dest_used_get(profile)
  for (i in seq_along(type_vec)) {
    .projr_yml_dest_rm_type(type_vec[[i]], profile)
  }
  invisible(TRUE)
}

.projr_yml_dest_used_get <- function(profile) {
  .projr_yml_dest_opt_vec()[
    .projr_yml_dest_opt_vec() %in%
      names(projr_yml_get_unchecked(profile)[["build"]])
  ]
}

.projr_yml_dest_opt_vec <- function() {
  c("local", "osf", "github")
}

# basic
# -----------------

# title
.projr_yml_dest_set_title <- function(yml, title, type, profile) {
  yml_type <- .projr_yml_dest_get_type(type, profile) %||%
    (list(NULL) |> stats::setNames(type))
  yml_type[[title]] <- yml
  .projr_yml_dest_set_type(yml_type, type, profile)
}

.projr_yml_dest_get_title <- function(type, title, profile) {
  .projr_yml_dest_get_type(type, profile)[[title]] %@@% NULL
}

# most basic
# -----------------

# type
.projr_yml_dest_set_type <- function(yml_type, type, profile) {
  .projr_state_opt(type, c("osf", "local", "github"))
  .projr_yml_build_set_nm(yml_type, type, profile)
}

.projr_yml_dest_get_type <- function(type, profile) {
  .projr_state_opt(type, c("osf", "local", "github"))
  init_list <- projr_yml_get_unchecked(profile)[["build"]][[type]]
  if (length(init_list) == 0L) {
    return(NULL)
  }
  init_list
}
