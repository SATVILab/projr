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
                                get_strategy = NULL,
                                get_conflict = NULL,
                                send_cue = NULL,
                                send_strategy = NULL,
                                send_inspect = NULL) {
  # gather get and send args
  # ------------------------
  get_list <- .projr_yml_remote_transfer_get(
    strategy = get_strategy,
    conflict = get_conflict
  )

  send_list <- .projr_yml_remote_transfer_get(
    cue = send_cue,
    strategy = send_strategy,
    inspect = send_inspect
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
    .projr_list_add(path_append_label, nm = "path-append-label") |>
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
  .assert_string(id, TRUE)
  .assert_nchar_single(id, 5L)
  if (!.projr_yml_dest_add_get_list_add_extra_osf_id_chr_check(id)) {
    stop(
      paste0("id ", id, " not found on OSF"),
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
    if (.is_len_0(id) || !.is_string(id)) {
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
.projr_yml_dest_get_title_complete <- function(title,
                                               type,
                                               profile,
                                               upload_github,
                                               upload_force) {
  force(title)
  force(profile)

  .projr_yml_dest_get_title_init(
    title, type, profile, upload_github, upload_force
  ) |>
    .projr_yml_dest_complete_title(title, type)
}

.projr_yml_dest_get_title_init <- function(title,
                                           type,
                                           profile,
                                           upload_github,
                                           upload_force) {
  is_github_param <-.projr_yml_dest_get_title_complete_is_github_param(
    type, title, upload_github
  )
  if (!is_github_param) {
    .projr_yml_dest_get_title(title, type, profile)
  } else {
    # construct equivalent yml as only specified via parameter
    # at this stage
    .projr_yml_dest_get_title_complete_param(
      title, type, upload_github, upload_force
    )
  }
}

.projr_yml_dest_get_title_complete_is_github_param <- function(type,
                                                               title,
                                                               upload_github) {
  is_github <- type == "github"
  is_archive_param <- title == "archive" && !isFALSE(upload_github)
  is_github_param <- is_github && is_archive_param
  is_github_param
}

.projr_yml_dest_get_title_complete_param <- function(title,
                                                     type,
                                                     upload_github,
                                                     upload_force) {
  .projr_yml_dest_get_title_complete_param_init(
    title, type, upload_github
  ) |>
    .projr_yml_dest_get_title_complete_param_force(upload_force)
}

.projr_yml_dest_get_title_complete_param_init <- function(title,
                                                          type,
                                                          upload_github) {
  content_vec <- .projr_dest_send_title_get_content(
    title, type, upload_github
  )
  list(
    "title" = title,
    "type" = type,
    content = content_vec
  )
}

.projr_yml_dest_get_title_complete_param_force <- function(yml_title,
                                                            upload_force) {
  if (!upload_force) {
    return(yml_title)
  }
  yml_title[["send"]] <- list(
    "inspect" = "none",
    "strategy" = "sync-purge",
    "conflict" = "overwrite"
  )
  yml_title
}

.projr_yml_dest_get_title_complete_param_nonforce <- function(yml_title) {
  yml_title |>
    .projr_yml_dest_complete_title()
}


.projr_yml_dest_complete_title <- function(yml_title, title, type) {
  yml_title |>
    .projr_yml_dest_complete_title_structure(type) |>
    .projr_yml_dest_complete_title_upload(type) |>
    .projr_yml_dest_complete_title_path_append_label(type) |>
    .projr_yml_dest_complete_title_path(type) |>
    .projr_yml_dest_complete_title_id(type, title)
}

.projr_yml_dest_complete_title_structure <- function(yml, type) {
  .projr_yml_complete(yml, "structure", "archive")
}


.projr_yml_dest_complete_title_upload <- function(yml, type) {
  yml[["send"]] <- yml[["send"]] |>
    .projr_yml_dest_complete_title_cue(type) |>
    .projr_yml_dest_complete_title_upload_inspect(type) |>
    .projr_yml_dest_complete_title_upload_strategy(type)
  yml
}

.projr_yml_dest_complete_title_cue <- function(yml, type) {
  .projr_yml_complete(yml, "cue", "if-change")
}

.projr_yml_dest_complete_title_upload_inspect <- function(yml, type) {
  .projr_yml_complete(yml, "inspect", "manifest")
}

# strategy
.projr_yml_dest_complete_title_upload_strategy <- function(yml, type) {
  yml[["strategy"]] <- switch(type,
    "local" = ,
    "osf" = .projr_yml_dest_complete_title_strategy_hierarchy(
      yml[["strategy"]], yml[["inspect"]]
    ),
    "github" = .projr_yml_dest_complete_title_strategy_github(
      yml[["strategy"]], yml[["inspect"]]
    )
  )
  yml
}

.projr_yml_dest_complete_title_strategy_hierarchy <-
  function(strategy, inspect) {
    # default is sync-diff
    strategy <- strategy %||% "sync-diff"
    inspect <- inspect %||% "manifest"
    # if we cannot use versioning but must sync, the only option is
    # sync-purge
    if (inspect == "none" && strategy == "sync-diff") {
      return("sync-purge")
    }
    strategy
  }

.projr_yml_dest_complete_title_strategy_github <-
  function(strategy, inspect) {
    # default is sync-diff, for speeds
    strategy <- strategy %||% "sync-diff"
    inspect <- inspect %||% "manifest"
    # only if we're allowed to use versioning and we're syncing
    # do we use sync-diff (which is the default).
    # Otherwise, we use sync-purge
    if (strategy == "sync-diff" && inspect != "none") {
      return("sync-diff")
    }
    "sync-purge"
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

# ----------------------
# set individual settings
# ----------------------

# strategy
.projr_yml_dest_set_send_strategy <- function(strategy,
                                                   title,
                                                   type,
                                                   profile) {
  .assert_in(strategy, .projr_opt_remote_strategy_get())
  yml_title <- .projr_yml_dest_get_title(
    title = title, type = type, profile = profile
  )
  yml_title[["send"]][["strategy"]] <- strategy
  .projr_yml_dest_set_title(
    yml = yml_title, title = title, type = type, profile = profile,
    overwrite = TRUE
  )
}

# cue
.projr_yml_dest_set_send_cue <- function(cue, title, type, profile) {
  .assert_in(cue, .projr_opt_cue_get())
  yml_title <- .projr_yml_dest_get_title(
    title = title, type = type, profile = profile
  )
  yml_title[["cue"]] <- cue
  .projr_yml_dest_set_title(
    yml = yml_title, title = title, type = type, profile = profile,
    overwrite = TRUE
  )
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
