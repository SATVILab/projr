.yml_dest_add <- function(role,
                          type,
                          title,
                          profile,
                          content = NULL,
                          structure = NULL,
                          path = NULL,
                          path_append_label = NULL,
                          overwrite = FALSE,
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
  get_list <- .yml_remote_transfer_get(
    strategy = get_strategy,
    conflict = get_conflict
  )

  send_list <- .yml_remote_transfer_get(
    cue = send_cue,
    strategy = send_strategy,
    inspect = send_inspect
  )

  # check inputs
  # ------------

  .yml_remote_check(
    role = role,
    type = type,
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    overwrite = overwrite,
    description = description,
    id = id,
    id_parent = id_parent,
    get_list = get_list,
    send_list = send_list
  )

  # add
  # -----------------
  .yml_dest_add_impl(
    type = type,
    title = title,
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list,
    overwrite = overwrite,
    description = description,
    id = id,
    id_parent = id_parent,
    profile = profile
  )

  invisible(TRUE)
}

# add now
.yml_dest_add_impl <- function(type,
                               title,
                               content,
                               structure,
                               path,
                               path_append_label,
                               get_list,
                               send_list,
                               overwrite,
                               description,
                               id,
                               id_parent,
                               profile) {
  title <- .yml_remote_title_get(title = title, content = content)
  .yml_dest_add_get_list_add(
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list
  ) |>
    .yml_dest_add_get_list_add_extra(
      type = type, id = id, id_parent = id_parent, title = title,
      description = description
    ) |>
    .yml_dest_set_title(
      title = title, type = type,
      overwrite = overwrite, profile = profile
    )
}

# get list to add
.yml_dest_add_get_list_add <- function(content,
                                       path,
                                       path_append_label,
                                       structure,
                                       get_list,
                                       send_list) {
  list() |>
    .list_add(content) |>
    .list_add(path) |>
    .list_add(path_append_label, nm = "path-append-label") |>
    .list_add(structure) |>
    .list_add(get_list, nm = "get") |>
    .list_add(send_list, nm = "send")
}


.yml_dest_add_get_list_add_extra <- function(list_add,
                                             type,
                                             id,
                                             id_parent,
                                             title,
                                             description) {
  # OSF support has been removed
  list_add
}

# -----------------
# complete
# -----------------

# main function
.yml_dest_get_title_complete <- function(title,
                                         type,
                                         profile,
                                         archive_type,
                                         always_archive) {
  force(title)
  force(profile)

  .yml_dest_get_title_init(
    title, type, profile, archive_type, always_archive
  ) |>
    .yml_dest_complete_title(title, type)
}

.yml_dest_get_title_init <- function(title,
                                     type,
                                     profile,
                                     archive_type,
                                     always_archive) {
  if (isFALSE(archive_type)) {
    # not from parameter, so read from yml
    .yml_dest_get_title(title, type, profile)
  } else {
    # construct equivalent yml as only specified via parameter
    # at this stage
    .yml_dest_get_title_complete_param(
      title, type, archive_type, always_archive
    )
  }
}

.yml_dest_get_title_complete_param <- function(title,
                                               type,
                                               archive_type,
                                               always_archive) {
  .yml_dest_get_title_complete_param_init(
    title, type, archive_type
  ) |>
    .yml_dest_get_title_complete_param_force(always_archive)
}

.yml_dest_get_title_complete_param_init <- function(title,
                                                    type,
                                                    archive_type) {
  content_vec <- .dest_send_title_get_content(
    title, type, archive_type
  )
  list(
    "title" = title,
    "type" = type,
    content = content_vec
  )
}

.yml_dest_get_title_complete_param_force <- function(yml_title,
                                                     always_archive) {
  yml_title[["send"]][["cue"]] <- if (always_archive) "always" else "if-change"
  yml_title
}


.yml_dest_complete_title <- function(yml_title, title, type) {
  yml_title |>
    .yml_dest_complete_title_structure(type) |>
    .yml_dest_complete_title_upload(type) |>
    .yml_dest_complete_title_path_append_label(type) |>
    .yml_dest_complete_title_path(type) |>
    .yml_dest_complete_title_id(type, title)
}

.yml_dest_complete_title_structure <- function(yml, type) {
  .yml_complete(yml, "structure", "archive")
}


.yml_dest_complete_title_upload <- function(yml, type) {
  yml[["send"]] <- yml[["send"]] |>
    .yml_dest_complete_title_cue(type) |>
    .yml_dest_complete_title_upload_inspect(type) |>
    .yml_dest_complete_title_upload_strategy(type)
  yml
}

.yml_dest_complete_title_cue <- function(yml, type) {
  .yml_complete(yml, "cue", "if-change")
}

.yml_dest_complete_title_upload_inspect <- function(yml, type) {
  .yml_complete(yml, "inspect", "manifest")
}

# strategy
.yml_dest_complete_title_upload_strategy <- function(yml, type) {
  yml[["strategy"]] <- switch(type,
    "local" = .yml_dest_complete_title_strategy_hierarchy(
      yml[["strategy"]], yml[["inspect"]]
    ),
    "github" = .yml_dest_complete_title_strategy_github(
      yml[["strategy"]], yml[["inspect"]]
    )
  )
  yml
}

# Generic strategy completion function for all remote types
.yml_dest_complete_title_strategy_default <-
  function(strategy, inspect) {
    if (!is.null(strategy)) {
      return(strategy)
    }
    if (inspect == "none") {
      return("upload-all")
    }
    "sync-diff"
  }

# Wrappers for backward compatibility and clarity
.yml_dest_complete_title_strategy_hierarchy <-
  .yml_dest_complete_title_strategy_default

.yml_dest_complete_title_strategy_github <-
  .yml_dest_complete_title_strategy_default

.yml_dest_complete_title_path_append_label <- function(yml, type) {
  .yml_complete(yml, "path-append-label", TRUE)
}

.yml_dest_complete_title_path <- function(yml, type) {
  .yml_complete(yml, "path", NULL)
}

.yml_dest_complete_title_id <- function(yml, type, title) {
  switch(type,
    "local" = .yml_dest_complete_title_id_local(yml),
    "github" = .yml_dest_complete_title_id_github(yml, title),
    yml
  )
}

.yml_dest_complete_title_id_local <- function(yml) {
  yml[["id"]] <- yml[["path"]]
  yml
}

.yml_dest_complete_title_id_github <- function(yml, title) {
  yml[["id"]] <- title
  yml
}

# ----------------------
# set individual settings
# ----------------------

# strategy
.yml_dest_set_send_strategy <- function(strategy,
                                        title,
                                        type,
                                        profile) {
  .assert_in(strategy, .opt_remote_strategy_get())
  yml_title <- .yml_dest_get_title(
    title = title, type = type, profile = profile
  )
  yml_title[["send"]][["strategy"]] <- strategy
  .yml_dest_set_title(
    yml = yml_title, title = title, type = type, profile = profile,
    overwrite = TRUE
  )
}

# cue
.yml_dest_set_send_cue <- function(cue, title, type, profile) {
  .assert_in(cue, .opt_cue_get())
  yml_title <- .yml_dest_get_title(
    title = title, type = type, profile = profile
  )
  yml_title[["cue"]] <- cue
  .yml_dest_set_title(
    yml = yml_title, title = title, type = type, profile = profile,
    overwrite = TRUE
  )
}

# -----------------
# default
# -----------------

.yml_dest_add_add_default <- function(type,
                                      list_add,
                                      profile) {
  .yml_dest_set_title(
    type = type,
    title = names(list_add),
    profile = profile
  )
}

.yml_dest_rm_title <- function(title, type, profile) {
  yml_type <- .yml_dest_get_type(type, profile)
  yml_type[[title]] <- NULL
  .yml_dest_set_type(yml_type, type, profile)
}

# removing
.yml_dest_rm_type <- function(type, profile) {
  .yml_dest_set_type(NULL, type, profile)
}

.yml_dest_rm_type_all <- function(profile) {
  type_vec <- .yml_dest_used_get(profile)
  for (i in seq_along(type_vec)) {
    .yml_dest_rm_type(type_vec[[i]], profile)
  }
  invisible(TRUE)
}

.yml_dest_used_get <- function(profile) {
  .yml_dest_opt_vec()[
    .yml_dest_opt_vec() %in%
      names(.yml_get(profile)[["build"]])
  ]
}

.yml_dest_opt_vec <- function() {
  c("local", "github")
}

# basic
# -----------------

# title
.yml_dest_set_title <- function(yml, title, type, overwrite, profile) {
  .yml_dest_set_title_check(type, title, overwrite, profile)
  yml_type <- .yml_dest_get_type(type, profile) %||%
    (list())
  yml_type[[title]] <- yml
  .yml_dest_set_type(yml_type, type, profile)
}

.yml_dest_set_title_check <- function(type, title, overwrite, profile) {
  if (!overwrite) {
    yml_type <- .yml_dest_get_type(type, profile)
    if (title %in% names(yml_type)) {
      stop(
        paste0("Title ", title, " already exists"),
        call. = FALSE
      )
    }
  }
}

.yml_dest_get_title <- function(title, type, profile) {
  .yml_dest_get_type(type, profile)[[title]] %@@% NULL
}

# most basic
# -----------------

# type
.yml_dest_set_type <- function(yml_type, type, profile) {
  .assert_in(type, c("local", "github"), TRUE)
  .yml_build_set_nm(yml_type, type, profile)
}

.yml_dest_get_type <- function(type, profile) {
  .assert_in(type, c("local", "github"), TRUE)
  init_list <- .yml_get(profile)[["build"]][[type]]
  if (length(init_list) == 0L) {
    return(NULL)
  }
  init_list
}
