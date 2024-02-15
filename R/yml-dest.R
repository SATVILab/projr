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
    profile = profile
  )

  invisible(TRUE)
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
    .projr_yml_dest_add_get_list_add_extra(
      type = type, id = id, id_parent = id_parent, title = title,
      category = category, public = public, description = description
    ) |>
    .projr_yml_dest_set_title(
      title = title, type = type,
      overwrite = overwrite, profile = profile
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
                                                   title,
                                                   category,
                                                   public,
                                                   description) {
  switch(type,
    "osf" = .projr_yml_dest_add_get_list_add_extra_osf(
      list_add = list_add, id = id, id_parent = id_parent, title,
      category = category, public = public, description = description
    ),
    list_add
  )
}

.projr_yml_dest_add_get_list_add_extra_osf <- function(list_add,
                                                       id,
                                                       id_parent,
                                                       title,
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
      title = title,
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
  .projr_remote_create(
    type = "osf",
    name = title,
    id_parent = id_parent,
    category = category,
    public = public,
    description = description
  ) |>
    .projr_yml_dest_add_get_list_add_extra_osf_id_null_check_success() |>
    .projr_list_add(list_base = list_add, x = _, nm = "id")
}

.projr_yml_dest_add_get_list_add_extra_osf_id_null_check_success <-
  function(id, list_add) {
    if (!nzchar(id)) {
      stop(
        "Failed to create OSF node",
        call. = FALSE
      )
    }
    id
  }

# -----------------
# complete
# -----------------

# main function
.projr_yml_dest_get_title_complete <- function(title, type, profile) {
  force(title)
  force(profile)
  .projr_yml_dest_get_title(title, type, profile) |>
    .projr_yml_dest_complete_title(title, type)
}


.projr_yml_dest_complete_title <- function(yml_title, title, type) {
  yml_title |>
    .projr_yml_dest_complete_title_structure(type) |>
    .projr_yml_dest_complete_title_cue(type) |>
    .projr_yml_dest_complete_title_upload(type) |>
    .projr_yml_dest_complete_title_path_append_label(type) |>
    .projr_yml_dest_complete_title_path(type) |>
    .projr_yml_dest_complete_title_id(type, title)
}

.projr_yml_dest_complete_title_structure <- function(yml, type) {
  .projr_yml_complete(yml, "structure", "version")
}

.projr_yml_dest_complete_title_cue <- function(yml, type) {
  .projr_yml_complete(yml, "cue", "patch")
}

.projr_yml_dest_complete_title_upload <- function(yml, type) {
  yml[["send"]] <- yml[["send"]] |>
    .projr_yml_dest_complete_title_upload_version_source(type) |>
    .projr_yml_dest_complete_title_upload_sync_approach(type) |>
    .projr_yml_dest_complete_title_upload_conflict(type)
  yml
}

.projr_yml_dest_complete_title_upload_version_source <- function(yml, type) {
  .projr_yml_complete(yml, "version-source", "manifest")
}

# sync-approach
.projr_yml_dest_complete_title_upload_sync_approach <- function(yml, type) {
  yml[["sync-approach"]] <- switch(type,
    "local" = ,
    "osf" = .projr_yml_dest_complete_title_sync_approach_hierarchy(
      yml[["sync-approach"]], yml[["version-source"]]
    ),
    "github" = .projr_yml_dest_complete_title_sync_approach_github(
      yml[["sync-approach"]], yml[["version-source"]]
    )
  )
  yml
}

.projr_yml_dest_complete_title_sync_approach_hierarchy <-
  function(sync_approach, version_source) {
    # default is sync-using-version
    sync_approach <- sync_approach %||% "sync-using-version"
    version_source <- version_source %||% "manifest"
    # if we cannot use versioning but must sync, the only option is
    # sync-using-deletion
    if (version_source == "none" && sync_approach == "sync-using-version") {
      return("sync-using-deletion")
    }
    sync_approach
  }

.projr_yml_dest_complete_title_sync_approach_github <-
  function(sync_approach, version_source) {
    # default is sync-using-version, for speeds
    sync_approach <- sync_approach %||% "sync-using-version"
    version_source <- version_source %||% "manifest"
    # only if we're allowed to use versioning and we're syncing
    # do we use sync-using-version (which is the default).
    # Otherwise, we use sync-using-deletion
    if (sync_approach == "sync-using-version" && version_source != "none") {
      return("sync-using-version")
    }
    "sync-using-deletion"
  }

.projr_yml_dest_complete_title_upload_conflict <- function(yml, type) {
  .projr_yml_complete(yml, "conflict", "overwrite")
}

.projr_yml_dest_complete_title_path_append_label <- function(yml, type) {
  .projr_yml_complete(yml, "path-append-label", TRUE)
}

.projr_yml_dest_complete_title_path <- function(yml, type) {
  .projr_yml_complete(yml, "path", NULL)
}

.projr_yml_dest_complete_title_id <- function(yml, type, title) {
  switch(type,
    "local" = .projr_yml_dest_complete_title_id_local(yml),
    "github" = .projr_yml_dest_complete_title_id_github(yml, title),
    yml
  )
}

.projr_yml_dest_complete_title_id_local <- function(yml) {
  yml[["id"]] <- yml[["path"]]
  yml
}

.projr_yml_dest_complete_title_id_github <- function(yml, title) {
  yml[["id"]] <- title
  yml
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
      names(.projr_yml_get(profile)[["build"]])
  ]
}

.projr_yml_dest_opt_vec <- function() {
  c("local", "osf", "github")
}

# basic
# -----------------

# title
.projr_yml_dest_set_title <- function(yml, title, type, overwrite, profile) {
  .projr_yml_dest_set_title_check(type, title, overwrite, profile)
  yml_type <- .projr_yml_dest_get_type(type, profile) %||%
    (list())
  yml_type[[title]] <- yml
  .projr_yml_dest_set_type(yml_type, type, profile)
}

.projr_yml_dest_set_title_check <- function(type, title, overwrite, profile) {
  if (!overwrite) {
    yml_type <- .projr_yml_dest_get_type(type, profile)
    if (title %in% names(yml_type)) {
      stop(
        paste0("Title ", title, " already exists"),
        call. = FALSE
      )
    }
  }
}

.projr_yml_dest_get_title <- function(title, type, profile) {
  .projr_yml_dest_get_type(type, profile)[[title]] %@@% NULL
}

# most basic
# -----------------

# type
.projr_yml_dest_set_type <- function(yml_type, type, profile) {
  .assert_in(type, c("osf", "local", "github"), TRUE)
  .projr_yml_build_set_nm(yml_type, type, profile)
}

.projr_yml_dest_get_type <- function(type, profile) {
  .assert_in(type, c("osf", "local", "github"), TRUE)
  init_list <- .projr_yml_get(profile)[["build"]][[type]]
  if (length(init_list) == 0L) {
    return(NULL)
  }
  init_list
}
