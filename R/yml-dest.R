.yml_dest_add <- function(
  role,
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
  send_inspect = NULL,
  branch = NULL
) {
  .assert_string(branch)
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
    branch = branch,
    profile = profile
  )

  invisible(TRUE)
}

# add now
.yml_dest_add_impl <- function(
  type,
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
  profile,
  branch = NULL
) {
  title <- .yml_remote_title_get(title = title, content = content)
  .yml_dest_add_get_list_add(
    content = content,
    structure = structure,
    path = path,
    path_append_label = path_append_label,
    get_list = get_list,
    send_list = send_list
  ) |>
    .list_add(branch) |>
    .yml_dest_add_get_list_add_extra(
      type = type,
      id = id,
      id_parent = id_parent,
      title = title,
      description = description
    ) |>
    .yml_dest_set_title(
      title = title,
      type = type,
      overwrite = overwrite,
      profile = profile
    )
}

# get list to add
.yml_dest_add_get_list_add <- function(
  content,
  path,
  path_append_label,
  structure,
  get_list,
  send_list
) {
  list() |>
    .list_add(content) |>
    .list_add(path) |>
    .list_add(path_append_label, nm = "path-append-label") |>
    .list_add(structure) |>
    .list_add(get_list, nm = "get") |>
    .list_add(send_list, nm = "send")
}


.yml_dest_add_get_list_add_extra <- function(
  list_add,
  type,
  id,
  id_parent,
  title,
  description
) {
  # OSF support has been removed
  list_add
}

# -----------------
# complete
# -----------------

# main function
.yml_dest_get_title_complete <- function(
  title,
  type,
  profile,
  archive_type,
  always_archive,
  destination = NULL
) {
  force(title)
  force(profile)

  .yml_dest_get_title_init(
    title,
    type,
    profile,
    archive_type,
    always_archive
  ) |>
    .yml_dest_complete_title(title, type, destination)
}

# A registered remote remains available to readers even when a build doesn't publish to it.
.yml_remote_definition <- function(type, title, yml = .yml_get(NULL)) {
  registry <- yml[["remotes"]][[type]]
  if (title %in% names(registry)) {
    return(registry[[title]] %||% list())
  }
  yml[["build"]][[type]][[title]]
}

.yml_dest_get_title_init <- function(
  title,
  type,
  profile,
  archive_type,
  always_archive
) {
  yml <- .yml_get(profile)
  definition <- .yml_remote_definition(type, title, yml)
  if (title != "archive" || isFALSE(archive_type) || is.null(archive_type)) {
    if (title %in% names(yml[["remotes"]][[type]])) {
      selection <- yml[["build"]][[type]][[title]]
      definition[["content"]] <- selection[["content"]]
      send <- selection[intersect(names(selection), .opt_build_send_names())]
      if (length(send)) {
        definition[["send"]] <- utils::modifyList(
          definition[["send"]] %||% list(),
          send
        )
      }
    }
    return(definition)
  }
  one_off <- .yml_dest_get_title_complete_param(
    title,
    type,
    archive_type,
    always_archive
  )
  if (!title %in% names(yml[["remotes"]][[type]])) {
    return(one_off)
  }
  definition[["content"]] <- one_off[["content"]]
  definition[["send"]][["cue"]] <- definition[["send"]][["cue"]] %||%
    one_off[["send"]][["cue"]]
  definition
}

.yml_dest_get_title_complete_param <- function(
  title,
  type,
  archive_type,
  always_archive
) {
  .yml_dest_get_title_complete_param_init(
    title,
    type,
    archive_type
  ) |>
    .yml_dest_get_title_complete_param_force(always_archive)
}

.yml_dest_get_title_complete_param_init <- function(title, type, archive_type) {
  content_vec <- .dest_send_title_get_content(
    title,
    type,
    archive_type
  )
  list(
    "title" = title,
    "type" = type,
    path = if (type == "local") "_archive",
    content = content_vec
  )
}

.yml_dest_get_title_complete_param_force <- function(
  yml_title,
  always_archive
) {
  yml_title[["send"]][["cue"]] <- if (always_archive) "always" else "if-change"
  yml_title
}


.yml_dest_complete_title <- function(
  yml_title,
  title,
  type,
  destination = NULL
) {
  name <- yml_title[["title"]] %||% title
  if (type == "local") {
    yml_title[["title"]] <- if (is.null(destination)) {
      .template_expand(name)
    } else {
      name
    }
    path <- destination %||%
      .template_expand(yml_title[["path"]] %||% "_archive")
    yml_title[["path"]] <- as.character(fs::path_abs(
      fs::path_expand(path),
      start = .path_get()
    ))
  } else if (type == "github") {
    title <- destination %||%
      .remote_misc_github_tag_format(yml_title[["tag"]] %||% name)
    if (
      is.null(destination) &&
        !is.null(yml_title[["tag"]]) &&
        !is.null(yml_title[["title"]]) &&
        !identical(title, .remote_misc_github_tag_format(name))
    ) {
      stop("A GitHub release title and tag must match.", call. = FALSE)
    }
    yml_title[["title"]] <- title
  }
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
  yml[["strategy"]] <- switch(
    type,
    "local" = .yml_dest_complete_title_strategy_hierarchy(
      yml[["strategy"]],
      yml[["inspect"]]
    ),
    "github" = .yml_dest_complete_title_strategy_github(
      yml[["strategy"]],
      yml[["inspect"]]
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
  switch(
    type,
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
.yml_dest_set_send_strategy <- function(strategy, title, type, profile) {
  .assert_in_single(strategy, .opt_remote_strategy_get())
  yml_title <- .yml_dest_get_title(
    title = title,
    type = type,
    profile = profile
  )
  yml_title[["send"]][["strategy"]] <- strategy
  .yml_dest_set_title(
    yml = yml_title,
    title = title,
    type = type,
    profile = profile,
    overwrite = TRUE
  )
}

# cue
.yml_dest_set_send_cue <- function(cue, title, type, profile) {
  .assert_in_single(cue, .opt_cue_get())
  yml_title <- .yml_dest_get_title(
    title = title,
    type = type,
    profile = profile
  )
  yml_title[["send"]][["cue"]] <- cue
  .yml_dest_set_title(
    yml = yml_title,
    title = title,
    type = type,
    profile = profile,
    overwrite = TRUE
  )
}

# -----------------
# removing
# -----------------

.yml_dest_rm_title <- function(title, type, profile) {
  profile <- profile %||% "default"
  config <- .yml_get(profile)
  config[["remotes"]][[type]][[title]] <- NULL
  config[["build"]][["dest"]] <- .yml_build_dest_drop(
    config[["build"]][["dest"]],
    function(ref) ref %in% c(title, paste0(type, ":", title))
  )
  .yml_set(.yml_remotes_tidy(config), profile)
}

.yml_dest_rm_type <- function(type, profile) {
  profile <- profile %||% "default"
  config <- .yml_get(profile)
  titles <- names(config[["remotes"]][[type]])
  config[["remotes"]][[type]] <- NULL
  config[["build"]][["dest"]] <- .yml_build_dest_drop(
    config[["build"]][["dest"]],
    function(ref) ref %in% c(titles, paste0(type, ":", titles))
  )
  .yml_set(.yml_remotes_tidy(config), profile)
}

.yml_dest_rm_type_all <- function(profile) {
  for (type in .yml_dest_opt_vec()) {
    .yml_dest_rm_type(type, profile)
  }
  invisible(TRUE)
}

.yml_dest_opt_vec <- function() {
  c("local", "github")
}

# Remove emptied `remotes`/`build.dest` nodes so the YAML stays tidy.
.yml_remotes_tidy <- function(config) {
  config[["remotes"]] <- Filter(length, config[["remotes"]])
  if (!length(config[["remotes"]])) {
    config[["remotes"]] <- NULL
  }
  if (!length(config[["build"]][["dest"]])) {
    config[["build"]][["dest"]] <- NULL
  }
  config
}

# `build.dest` in mapping form, without the remote entries `drop` selects.
.yml_build_dest_drop <- function(dest, drop) {
  dest <- .yml_build_dest_mapping(dest)
  dest[!vapply(names(dest), drop, logical(1L))]
}

# `build.dest` in mapping form: global options, then one entry per remote.
.yml_build_dest_mapping <- function(dest) {
  if (is.null(dest)) {
    return(list())
  }
  parsed <- .yml_build_role_parse(
    dest,
    "build.dest",
    c("content", .opt_build_send_names())
  )
  remotes <- lapply(parsed[["remotes"]], function(x) {
    if (length(x)) x else structure(list(), names = character())
  })
  c(parsed[["global"]], remotes)
}

# basic
# -----------------

# Store a destination's settings under `remotes.<type>.<title>` and select it,
# with any `content`, under `build.dest`.
.yml_dest_set_title <- function(yml, title, type, overwrite, profile) {
  profile <- profile %||% "default"
  .yml_dest_set_title_check(type, title, overwrite, profile)
  config <- .yml_get(profile)
  settings <- yml[setdiff(names(yml), "content")]
  # Older internal setters use the shorter cue field.
  if (!is.null(settings[["cue"]])) {
    settings[["send"]][["cue"]] <- settings[["cue"]]
    settings[["cue"]] <- NULL
  }
  previous <- config[["remotes"]][[type]][[title]] %||%
    structure(list(), names = character())
  config[["remotes"]][[type]][title] <- list(
    utils::modifyList(previous, settings)
  )
  dest <- .yml_build_dest_mapping(config[["build"]][["dest"]])
  other <- setdiff(.yml_dest_opt_vec(), type)
  ref <- title
  if (title %in% names(config[["remotes"]][[other]])) {
    # A bare entry now refers to the other type; keep it unambiguous.
    names(dest)[names(dest) == title] <- paste0(other, ":", title)
    ref <- paste0(type, ":", title)
  }
  dest <- dest[!names(dest) %in% c(title, paste0(type, ":", title))]
  dest[ref] <- list(
    if ("content" %in% names(yml)) {
      yml["content"]
    } else {
      structure(list(), names = character())
    }
  )
  config[["build"]][["dest"]] <- dest
  .yml_set(config, profile)
}

.yml_dest_set_title_check <- function(type, title, overwrite, profile) {
  if (!overwrite && title %in% names(.yml_get(profile)[["remotes"]][[type]])) {
    stop(
      paste0("Title ", title, " already exists"),
      call. = FALSE
    )
  }
}

# A destination's settings with its selected `content`, as written.
.yml_dest_get_title <- function(title, type, profile) {
  profile <- profile %||% "default"
  config <- .yml_get(profile)
  definition <- config[["remotes"]][[type]][[title]]
  dest <- .yml_build_dest_mapping(config[["build"]][["dest"]])
  selection <- dest[[paste0(type, ":", title)]] %||% dest[[title]]
  c(definition, selection["content"][!is.null(selection[["content"]])])
}

# most basic
# -----------------

.yml_dest_get_type <- function(type, profile) {
  .assert_in_single(type, c("local", "github"), TRUE)
  init_list <- .yml_get(profile)[["build"]][[type]]
  if (length(init_list) == 0L) {
    return(NULL)
  }
  init_list
}

# build.dest
# -----------------

# Settings `build.dest` accepts beside remote names, globally or per remote.
.opt_build_send_names <- function() {
  c("cue", "inspect", "strategy")
}

# Option words cannot name a remote, because remote names and options share
# the mapping form of `build.dest` and `build.source`.
.opt_remote_reserved_titles <- function() {
  c("content", "cue", "inspect", "strategy", "conflict", "version")
}

# Expand `build.dest` into the `build.<type>.<title>` selections the rest of
# the package reads. Each entry names a remote under `remotes`.
.yml_build_dest_resolve <- function(yml) {
  .yml_build_legacy_stop(yml[["build"]])
  dest <- yml[["build"]][["dest"]]
  if (is.null(dest)) {
    return(yml)
  }
  yml[["build"]][["dest"]] <- NULL
  options <- c("content", .opt_build_send_names())
  parsed <- .yml_build_role_parse(dest, "build.dest", options)
  for (ref in names(parsed[["remotes"]])) {
    remote <- .yml_remote_ref_resolve(ref, yml[["remotes"]], "build.dest")
    type <- remote[["type"]]
    title <- remote[["title"]]
    if (title %in% names(yml[["build"]][[type]])) {
      stop(
        "Select remote ",
        title,
        " in build.dest or build.",
        type,
        ", not both.",
        call. = FALSE
      )
    }
    yml[["build"]][[type]][title] <- list(utils::modifyList(
      parsed[["global"]],
      parsed[["remotes"]][[ref]]
    ))
  }
  yml
}

# Split a `build.dest`/`build.source` value into global options and per-remote
# settings. Accepts a remote name, a list of names (each optionally a
# one-entry mapping with settings) or a mapping of options and remote names.
.yml_build_role_parse <- function(value, key, options) {
  if (is.character(value)) {
    value <- as.list(value)
  }
  if (!is.list(value)) {
    stop(key, " must name remotes declared under remotes.", call. = FALSE)
  }
  keys <- names(value) %||% rep("", length(value))
  global <- list()
  remotes <- list()
  for (i in seq_along(value)) {
    entry <- value[[i]]
    if (nzchar(keys[[i]])) {
      name <- keys[[i]]
    } else if (is.character(entry) && length(entry) == 1L) {
      name <- entry
      entry <- NULL
    } else if (
      is.list(entry) && length(entry) == 1L && .is_given_mid(names(entry))
    ) {
      name <- names(entry)
      entry <- entry[[1L]]
    } else {
      stop(
        key,
        " entries must be remote names or one-entry mappings.",
        call. = FALSE
      )
    }
    if (name %in% options) {
      if (!nzchar(keys[[i]])) {
        stop(key, " options belong in a mapping, not a list.", call. = FALSE)
      }
      global[name] <- list(entry)
      next
    }
    if (name %in% names(remotes)) {
      stop(key, " names remote ", name, " more than once.", call. = FALSE)
    }
    .yml_check_remote_mapping(entry, paste0(key, ".", name), options)
    remotes[name] <- list(entry %||% list())
  }
  list(global = global, remotes = remotes)
}

# Resolve `title` or `<type>:<title>` to a remote declared under `remotes`.
.yml_remote_ref_resolve <- function(ref, remotes, key) {
  parts <- regmatches(ref, regexec("^(local|github):(.+)$", ref))[[1L]]
  if (length(parts)) {
    title <- parts[[3L]]
    types <- parts[[2L]][title %in% names(remotes[[parts[[2L]]]])]
  } else {
    title <- ref
    types <- Filter(
      function(type) title %in% names(remotes[[type]]),
      .opt_remote_get_type()
    )
  }
  if (!length(types)) {
    stop(
      key,
      " names ",
      ref,
      ", which is not declared under remotes.",
      call. = FALSE
    )
  }
  if (length(types) > 1L) {
    stop(
      key,
      " names ",
      ref,
      ", which is declared under both remotes.local and remotes.github. ",
      "Use local:",
      ref,
      " or github:",
      ref,
      ".",
      call. = FALSE
    )
  }
  list(type = types, title = title)
}

# build.source
# -----------------

# Remotes selected under `build.source`, keyed `<type>:<title>`, with their
# content and fetch settings. Precedence: per-remote entry > whole
# `build.source` > `remotes.<type>.<title>.get` > package default.
.yml_build_source_get <- function(profile = NULL, source = NULL) {
  yml <- .yml_get(profile)
  source <- source %||% yml[["build"]][["source"]]
  if (is.null(source)) {
    return(list())
  }
  fetch <- .opt_remote_fetch_get()
  parsed <- .yml_build_role_parse(
    source,
    "build.source",
    c("content", names(fetch))
  )
  labels <- names(yml[["directories"]])
  out <- list()
  for (ref in names(parsed[["remotes"]])) {
    remote <- .yml_remote_ref_resolve(ref, yml[["remotes"]], "build.source")
    id <- paste0(remote[["type"]], ":", remote[["title"]])
    if (id %in% names(out)) {
      stop("build.source names remote ", id, " more than once.", call. = FALSE)
    }
    settings <- Reduce(
      function(x, y) utils::modifyList(x, Filter(Negate(is.null), y)),
      list(
        lapply(fetch, `[[`, 1L),
        yml[["remotes"]][[remote[["type"]]]][[remote[["title"]]]][["get"]] %||%
          list(),
        parsed[["global"]],
        parsed[["remotes"]][[ref]]
      )
    )
    out[[id]] <- c(
      remote,
      list(
        content = unlist(settings[["content"]]) %||%
          labels[.yml_dir_label_class_detect_raw(labels)],
        explicit = !is.null(settings[["content"]])
      ),
      settings[names(fetch)]
    )
  }
  out
}

# Archives used to be defined inline under `build.local`/`build.github`.
.yml_build_legacy_stop <- function(build) {
  legacy <- intersect(.opt_remote_get_type(), names(build))
  if (length(legacy)) {
    stop(
      "build.",
      legacy[[1L]],
      " is no longer supported. Define each archive under remotes.",
      legacy[[1L]],
      ".<title> and select it with build.dest.",
      call. = FALSE
    )
  }
}
