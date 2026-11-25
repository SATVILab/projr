#' @title Fetch raw inputs from their sources
#' @description
#' Checks and fetches directories from the remotes that supply them, outside a
#' build. Without `remote`, `projr_source()` uses the remotes selected under
#' `build.source`, or else every remote registered under `remotes`. A label
#' offered by more than one registered remote must be fetched with `remote`.
#'
#' Arguments override the configured `get` settings for this call. Files that
#' still match the last fetch are updated. Files changed locally since then
#' are conflicts: an interactive session asks whether to keep or replace them,
#' and otherwise `conflict` decides. Nothing changes until every selected label
#' has been checked.
#'
#' @param label Character. Directory labels to fetch. Defaults to each
#'   source's selection, which is every declared raw directory unless
#'   `build.source` lists content.
#' @param remote Character. A remote registered under `remotes`, named by its
#'   title or as `<type>:<title>` when the title is used by both types.
#' @param version Character. `"latest"`, a major version (`"1"`), a minor
#'   version (`"1.2"`), an exact build (`"1.2.3"`), or `"current"` to restore
#'   the inputs recorded for this checkout's completed build (see
#'   [projr_restore()]). Defaults to the configured version, or `"latest"`.
#' @param cue Character. `"if-change"` (the default here), `"always"`,
#'   `"if-empty"` or `"never"`.
#' @param strategy Character. `"sync-diff"`, `"sync-purge"`, `"download-all"`
#'   or `"download-missing"`.
#' @param inspect Character. `"manifest"`, `"file"` or `"none"`.
#' @param conflict Character. `"error"`, `"local"` (keep changed local files)
#'   or `"remote"` (replace them).
#' @return Invisibly returns `TRUE`.
#' @seealso [projr_restore()], [projr_get_local()], [projr_get_github()]
#' @examples
#' \dontrun{
#' projr_source()
#' projr_source("raw-survey", remote = "github:survey-team", version = "0.6")
#' projr_source(conflict = "remote")
#' }
#' @export
projr_source <- function(
  label = NULL,
  remote = NULL,
  version = NULL,
  cue = NULL,
  strategy = NULL,
  inspect = NULL,
  conflict = NULL
) {
  fetch <- .opt_remote_fetch_get()
  .assert_chr(label)
  .assert_string(remote)
  .assert_string(version)
  if (
    !is.null(version) &&
      !version %in% c("latest", "current") &&
      !grepl("^[0-9]+(\\.[0-9]+){0,2}$", version)
  ) {
    stop(
      "version must be latest, current or an x, x.y or x.y.z version.",
      call. = FALSE
    )
  }
  .assert_in_single(cue, fetch[["cue"]])
  .assert_in_single(strategy, fetch[["strategy"]])
  .assert_in_single(inspect, fetch[["inspect"]])
  .assert_in_single(conflict, fetch[["conflict"]])
  if (identical(version, "current")) {
    return(.source_current(label, remote, conflict))
  }
  overrides <- Filter(
    Negate(is.null),
    list(
      version = version,
      cue = cue %||% "if-change",
      strategy = strategy,
      inspect = inspect,
      conflict = conflict
    )
  )
  sources <- lapply(.source_targets(label, remote), function(source) {
    utils::modifyList(source, overrides)
  })
  workspace <- tempfile("projr-source-")
  fs::dir_create(workspace)
  on.exit(unlink(workspace, recursive = TRUE, expand = FALSE), add = TRUE)
  plans <- tryCatch(
    .source_plan(workspace, sources),
    projr_source_conflict = function(e) {
      if (!is.null(conflict) || !.is_interactive_and_not_test()) {
        stop(e)
      }
      choice <- .source_conflict_choice(conditionMessage(e))
      if (is.null(choice)) {
        stop("Cancelled. No files were changed.", call. = FALSE)
      }
      sources <- lapply(sources, function(source) {
        utils::modifyList(source, list(conflict = choice))
      })
      .source_plan(file.path(workspace, "resolved"), sources)
    }
  )
  .source_install(plans)
  invisible(TRUE)
}

.source_conflict_choice <- function(message) {
  cli::cli_alert_warning("{message}")
  choice <- utils::menu(
    c("Keep my local files", "Replace them with the remote's", "Cancel"),
    title = "How should these files be handled?"
  )
  switch(as.character(choice), "1" = "local", "2" = "remote", NULL)
}

# version = "current" restores the inputs this checkout recorded.
.source_current <- function(label, remote, conflict) {
  destination <- "archive"
  if (!is.null(remote)) {
    ref <- .yml_remote_ref_resolve(
      remote,
      .yml_get(NULL)[["remotes"]],
      "remote"
    )
    if (ref[["type"]] != "github") {
      stop(
        "version = \"current\" restores from a GitHub release.",
        call. = FALSE
      )
    }
    destination <- ref[["title"]]
  }
  if (identical(conflict, "local")) {
    stop(
      "version = \"current\" restores the recorded files; use conflict = ",
      "\"error\" or \"remote\".",
      call. = FALSE
    )
  }
  projr_restore(
    label = label,
    destination = destination,
    clear = identical(conflict, "remote")
  )
}

# The sources a call fetches from, in the shape `.yml_build_source_get()` returns.
.source_targets <- function(label, remote) {
  yml <- .yml_get(NULL)
  configured <- .yml_build_source_get(NULL)
  if (!is.null(remote)) {
    ref <- .yml_remote_ref_resolve(remote, yml[["remotes"]], "remote")
    id <- paste0(ref[["type"]], ":", ref[["title"]])
    source <- configured[[id]] %||%
      .yml_build_source_get(NULL, stats::setNames(list(NULL), id))[[id]]
    if (!is.null(label)) {
      source[["content"]] <- label
      source[["explicit"]] <- TRUE
    }
    return(stats::setNames(list(source), id))
  }
  if (length(configured)) {
    if (is.null(label)) {
      return(configured)
    }
    sources <- lapply(configured, function(source) {
      source[["content"]] <- intersect(source[["content"]], label)
      source[["explicit"]] <- TRUE
      source
    })
    sources <- Filter(function(source) length(source[["content"]]), sources)
    missing <- setdiff(label, unlist(lapply(sources, `[[`, "content")))
    if (length(missing)) {
      stop(
        "build.source does not supply ",
        paste(missing, collapse = ", "),
        ". Name a remote.",
        call. = FALSE
      )
    }
    return(sources)
  }
  .source_targets_discover(label, yml)
}

# Without build.source, assign each label to the one registered remote that
# has published it.
.source_targets_discover <- function(label, yml) {
  declared <- names(yml[["directories"]])
  labels <- label %||% declared[.yml_dir_label_class_detect_raw(declared)]
  ids <- unlist(lapply(.opt_remote_get_type(), function(type) {
    if (length(names(yml[["remotes"]][[type]]))) {
      paste0(type, ":", names(yml[["remotes"]][[type]]))
    }
  }))
  if (!length(ids)) {
    stop("No remotes are registered under remotes.", call. = FALSE)
  }
  workspace <- tempfile("projr-source-discover-")
  on.exit(unlink(workspace, recursive = TRUE, expand = FALSE), add = TRUE)
  holders <- list()
  for (id in ids) {
    ref <- .yml_remote_ref_resolve(id, yml[["remotes"]], "remote")
    ws <- file.path(workspace, gsub("[^A-Za-z0-9_-]", "_", id))
    fs::dir_create(ws)
    published <- tryCatch(
      .get_resolve(ref[["type"]], NULL, ref[["title"]], NULL, NULL, NULL, ws)[[
        "label"
      ]],
      error = function(e) character()
    )
    for (x in intersect(labels, published)) {
      holders[[x]] <- c(holders[[x]], id)
    }
  }
  missing <- setdiff(labels, names(holders))
  if (!is.null(label) && length(missing)) {
    stop(
      "No registered remote has published ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  shared <- Filter(function(x) length(x) > 1L, holders)
  if (length(shared)) {
    stop(
      paste0(
        names(shared),
        " is published by ",
        vapply(shared, paste, character(1L), collapse = " and "),
        collapse = "; "
      ),
      ". Choose one with remote.",
      call. = FALSE
    )
  }
  if (!length(holders)) {
    stop("No registered remote has published these labels.", call. = FALSE)
  }
  assigned <- split(names(holders), unlist(holders))
  sources <- lapply(names(assigned), function(id) {
    value <- stats::setNames(list(list(content = as.list(assigned[[id]]))), id)
    source <- .yml_build_source_get(NULL, value)[[id]]
    source[["explicit"]] <- !is.null(label)
    source
  })
  stats::setNames(sources, names(assigned))
}

# build.source: check and fetch raw inputs before hooks and scripts run.
#
# `.source_plan()` resolves each selected remote, compares its files with the
# local copy and the last fetch, downloads what is needed into `workspace` and
# stops on conflicts. It changes nothing locally. `.source_install()` then
# applies the plan and writes the fetch records.

.source_plan <- function(workspace, sources = .yml_build_source_get(NULL)) {
  sources <- Filter(function(source) source[["cue"]] != "never", sources)
  if (!length(sources)) {
    return(list())
  }
  .cli_step("Checking raw-input sources")
  .source_check_paths(sources)
  plans <- list()
  conflicts <- character()
  for (ref in names(sources)) {
    remote <- .source_plan_remote(
      sources[[ref]],
      ref,
      file.path(workspace, gsub("[^A-Za-z0-9_-]", "_", ref))
    )
    plans <- c(plans, remote[["plans"]])
    conflicts <- c(conflicts, remote[["conflicts"]])
  }
  if (length(conflicts)) {
    stop(errorCondition(
      paste0(
        paste(conflicts, collapse = "\n"),
        "\nRun projr_source() to choose, or set conflict: local to keep ",
        "these files or conflict: remote to replace them."
      ),
      class = "projr_source_conflict"
    ))
  }
  plans
}

# One label per directory, and no directory inside another.
.source_check_paths <- function(sources) {
  labels <- unlist(lapply(sources, `[[`, "content"), use.names = FALSE)
  repeated <- unique(labels[duplicated(labels)])
  if (length(repeated)) {
    stop(
      "build.source selects ",
      paste(repeated, collapse = ", "),
      " from more than one remote.",
      call. = FALSE
    )
  }
  paths <- vapply(
    labels,
    function(label) {
      as.character(fs::path_abs(
        projr_path_get_dir(label, create = FALSE, safe = FALSE)
      ))
    },
    character(1L)
  )
  for (i in seq_along(paths)) {
    for (j in seq_along(paths)[-seq_len(i)]) {
      if (
        fs::path_has_parent(paths[[i]], paths[[j]]) ||
          fs::path_has_parent(paths[[j]], paths[[i]])
      ) {
        stop(
          "build.source selects overlapping directories: ",
          labels[[i]],
          " (",
          paths[[i]],
          ") and ",
          labels[[j]],
          " (",
          paths[[j]],
          ").",
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

.source_plan_remote <- function(source, ref, workspace) {
  if (source[["cue"]] == "if-empty") {
    # Folders with files are left alone, so skip the remote (works offline).
    present <- vapply(
      source[["content"]],
      function(label) {
        length(.source_local_hashes(label)) > 0L
      },
      logical(1L)
    )
    if (all(present)) {
      plans <- lapply(source[["content"]], function(label) {
        list(label = label, remote = ref, status = "present")
      })
      return(list(plans = stats::setNames(plans, source[["content"]])))
    }
  }
  requested <- source[["version"]]
  exact <- grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", requested)
  if (exact && source[["cue"]] != "always") {
    # A pinned build already in place needs no network access.
    current <- vapply(
      source[["content"]],
      function(label) {
        record <- .source_record_read(label)
        local <- .source_local_hashes(label)
        files <- record[["files"]]
        identical(record[["version"]], .version_v_add(requested)) &&
          identical(local[sort(names(local))], files[sort(names(files))])
      },
      logical(1L)
    )
    if (all(current)) {
      plans <- lapply(source[["content"]], function(label) {
        list(label = label, remote = ref, status = "pinned")
      })
      return(list(plans = stats::setNames(plans, source[["content"]])))
    }
  }
  fs::dir_create(workspace)
  # Read the whole history so an earlier row can stand for an unchanged label.
  resolved <- .get_resolve(
    source[["type"]],
    NULL,
    source[["title"]],
    NULL,
    source[["content"]],
    NULL,
    workspace
  )
  store <- resolved[["sources"]][[1L]]
  version <- if (requested == "latest") {
    resolved[["version"]]
  } else {
    .source_version_match(store, requested, ref)
  }
  explicit <- isTRUE(source[["explicit"]])
  plans <- list()
  conflicts <- character()
  for (label in source[["content"]]) {
    label_version <- .source_label_version(store, label, version)
    if (is.null(label_version)) {
      message <- paste0(
        ref,
        " has no ",
        label,
        " in build ",
        version,
        "."
      )
      if (explicit) {
        stop("build.source: ", message, call. = FALSE)
      }
      .cli_info("Skipping {label}: {message}")
      next
    }
    if (label_version != version) {
      .cli_info(
        "{ref} last published {label} in {label_version}; using it for build {version}."
      )
    }
    plan <- .source_plan_label(
      source,
      ref,
      label,
      label_version,
      store,
      resolved,
      workspace
    )
    if (!is.null(plan[["conflict"]])) {
      conflicts <- c(conflicts, plan[["conflict"]])
    }
    plans[[label]] <- plan
  }
  list(plans = plans, conflicts = conflicts)
}

# The latest completed version within a requested major (`x`) or minor (`x.y`).
.source_version_match <- function(store, requested, ref) {
  versions <- if (.manifest_store_ready(store[["store"]])) {
    .manifest_store_index(store[["store"]])[["version"]]
  } else {
    .manifest_read(store[["csv"]])[["version"]]
  }
  versions <- unique(versions)
  numbers <- numeric_version(sub("^v", "", versions), strict = FALSE)
  prefix <- as.integer(strsplit(requested, ".", fixed = TRUE)[[1L]])
  exact <- length(prefix) == 3L
  keep <- vapply(
    unclass(numbers),
    function(parts) {
      if (exact) {
        return(identical(as.integer(parts), prefix))
      }
      length(parts) >= length(prefix) &&
        identical(as.integer(parts[seq_along(prefix)]), prefix)
    },
    logical(1L)
  )
  if (!any(keep)) {
    stop(
      "build.source: ",
      ref,
      " has no completed build matching version ",
      requested,
      ".",
      call. = FALSE
    )
  }
  versions <- versions[keep]
  versions[[order(numbers[keep], decreasing = TRUE)[[1L]]]]
}

# The label's state at `version`: its own row, or the latest earlier row when
# that build did not resend an unchanged label.
.source_label_version <- function(store, label, version) {
  versions <- if (.manifest_store_ready(store[["store"]])) {
    .manifest_store_index(store[["store"]], label)[["version"]]
  } else {
    history <- .manifest_read(store[["csv"]])
    history[["version"]][history[["label"]] == label]
  }
  versions <- unique(versions)
  numbers <- numeric_version(sub("^v", "", versions), strict = FALSE)
  earlier <- versions[numbers <= numeric_version(.version_v_rm(version))]
  if (!length(earlier)) {
    return(NULL)
  }
  earlier[[which.max(numeric_version(sub("^v", "", earlier)))]]
}

.source_plan_label <- function(
  source,
  ref,
  label,
  version,
  store,
  resolved,
  workspace
) {
  path <- projr_path_get_dir(label, create = FALSE, safe = FALSE)
  local <- .source_local_hashes(label)
  record <- .source_record_read(label)
  base <- record[["files"]]
  plan <- list(
    label = label,
    remote = ref,
    path = path,
    version = version,
    previous = record[["version"]],
    write = character(),
    delete = character()
  )
  if (source[["cue"]] == "if-empty" && length(local)) {
    plan[["status"]] <- "present"
    return(plan)
  }
  snapshot <- .restore_build_snapshot(
    store[["store"]],
    store[["csv"]],
    label,
    version
  )
  plan[["snapshot"]] <- snapshot[["id"]]
  # inspect: file confirms even an empty build from the remote's own files.
  remote <- if (source[["inspect"]] == "manifest") snapshot[["files"]]
  staging <- NULL
  download <- function() {
    if (is.null(staging)) {
      out <- file.path(workspace, "files")
      .get_install(
        resolved[["sources"]],
        label,
        version,
        resolved[["producer"]],
        out,
        TRUE,
        workspace
      )
      staging <<- file.path(out, label)
    }
    staging
  }
  if (is.null(remote)) {
    remote <- .restore_build_hashes(
      download(),
      resolved[["producer"]][["hashing"]]
    )
  }
  remote <- remote[setdiff(names(remote), .source_managed_files(label))]
  plan[["files"]] <- remote
  if (
    source[["cue"]] == "if-change" &&
      source[["inspect"]] != "none" &&
      identical(local, remote)
  ) {
    plan[["status"]] <- "current"
    return(plan)
  }

  changed <- names(remote)[
    is.na(local[names(remote)]) | local[names(remote)] != remote
  ]
  write <- if (source[["strategy"]] == "download-missing") {
    setdiff(names(remote), names(local))
  } else {
    changed
  }
  # ponytail: sync-purge behaves as sync-diff; the resulting files are the same.
  delete <- if (startsWith(source[["strategy"]], "sync-")) {
    setdiff(names(local), names(remote))
  } else {
    character()
  }
  touched <- c(intersect(write, names(local)), delete)
  edited <- if (is.null(base)) {
    touched
  } else {
    touched[is.na(base[touched]) | base[touched] != local[touched]]
  }
  if (length(edited)) {
    if (source[["conflict"]] == "error") {
      plan[["conflict"]] <- paste0(
        label,
        " (",
        ref,
        "): local changes would be overwritten or deleted: ",
        paste(utils::head(edited, 5L), collapse = ", "),
        if (length(edited) > 5L) paste0(" and ", length(edited) - 5L, " more"),
        "."
      )
    } else if (source[["conflict"]] == "local") {
      write <- setdiff(write, edited)
      delete <- setdiff(delete, edited)
      plan[["kept"]] <- edited
    }
  }
  plan[["write"]] <- write
  plan[["delete"]] <- delete
  plan[["status"]] <- if (length(write) || length(delete)) {
    "update"
  } else {
    "current"
  }
  if (length(write) && is.null(plan[["conflict"]])) {
    plan[["staging"]] <- download()
  }
  plan
}

.source_install <- function(plans) {
  for (plan in plans) {
    if (plan[["status"]] %in% c("present", "pinned")) {
      next
    }
    path <- plan[["path"]]
    unlink(file.path(path, plan[["delete"]]), expand = FALSE)
    # Drop folders the deletions emptied, so a file can take a folder's place.
    for (dir in unique(dirname(as.character(plan[["delete"]])))) {
      while (
        !dir %in% c(".", "") &&
          !length(dir(file.path(path, dir), all.files = TRUE, no.. = TRUE))
      ) {
        unlink(file.path(path, dir), recursive = TRUE, expand = FALSE)
        dir <- dirname(dir)
      }
    }
    for (file in plan[["write"]]) {
      target <- file.path(path, file)
      fs::dir_create(dirname(target))
      fs::file_copy(
        file.path(plan[["staging"]], file),
        target,
        overwrite = TRUE
      )
    }
    .source_record_write(plan)
    if (identical(plan[["status"]], "update")) {
      update <- paste0(
        "Updated ",
        plan[["label"]],
        " from ",
        plan[["remote"]],
        ": ",
        plan[["previous"]] %||% "none",
        " -> ",
        plan[["version"]],
        " (",
        length(plan[["write"]]),
        " written, ",
        length(plan[["delete"]]),
        " removed",
        if (length(plan[["kept"]])) {
          paste0(", ", length(plan[["kept"]]), " local kept")
        },
        ")."
      )
      .cli_info("{update}")
    }
    detail <- paste0(
      "Source ",
      plan[["label"]],
      ": ",
      plan[["status"]],
      " from ",
      plan[["remote"]],
      " at ",
      plan[["version"]],
      " (snapshot ",
      plan[["snapshot"]] %||% "unknown",
      ")."
    )
    .cli_debug("{detail}")
  }
  invisible(TRUE)
}

# Local file hashes, without the LICENSE projr writes into a licensed folder.
.source_local_hashes <- function(label) {
  hashes <- .restore_build_hashes(
    projr_path_get_dir(label, create = FALSE, safe = FALSE)
  )
  hashes[setdiff(names(hashes), .source_managed_files(label))]
}

.source_managed_files <- function(label) {
  if (!is.null(.yml_dir_get_license(label, NULL))) "LICENSE" else character()
}

# Fetch records live in the Git-ignored .projr/source/, one file per label.
.source_record_path <- function(label) {
  .path_get(".projr", "source", paste0(label, ".json"))
}

.source_record_read <- function(label) {
  path <- .source_record_path(label)
  if (!file.exists(path)) {
    return(NULL)
  }
  record <- jsonlite::read_json(path, simplifyVector = TRUE)
  record[["files"]] <- unlist(record[["files"]]) %||% character()
  record
}

.source_record_write <- function(plan) {
  path <- .source_record_path(plan[["label"]])
  fs::dir_create(dirname(path))
  jsonlite::write_json(
    list(
      label = plan[["label"]],
      remote = plan[["remote"]],
      version = plan[["version"]],
      snapshot = plan[["snapshot"]],
      fetched = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      kept = plan[["kept"]],
      files = as.list(plan[["files"]])
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

# Each selected label's recorded source, for the build log.
.buildlog_get_sources <- function() {
  sources <- tryCatch(.yml_build_source_get(NULL), error = function(e) list())
  sources <- Filter(function(source) source[["cue"]] != "never", sources)
  labels <- unlist(lapply(sources, `[[`, "content"), use.names = FALSE)
  records <- Filter(Negate(is.null), lapply(labels, .source_record_read))
  if (!length(records)) {
    return(character())
  }
  entries <- lapply(records, function(record) {
    list(
      remote = record[["remote"]],
      version = record[["version"]],
      snapshot = record[["snapshot"]]
    )
  })
  names(entries) <- vapply(records, `[[`, character(1L), "label")
  c("**Raw-input sources**", "", "```yaml", yaml::as.yaml(entries), "```", "")
}
