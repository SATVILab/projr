# Stage every selected label on the destination filesystem. Committing by rename
# lets a failed download leave all destination directories as they were.
.get_install <- function(
  sources,
  labels,
  version,
  producer,
  out,
  overwrite,
  workspace
) {
  hashing <- producer$hashing %||% list(approach = "cpp", cores = 1L)
  paths <- stats::setNames(as.character(file.path(out, labels)), labels)
  protected <- character()
  for (source in sources) {
    if (source$type == "local") {
      protected <- c(protected, source$root)
    }
    if (source$type == "project") {
      if (any(fs::path_has_parent(source$root, paths))) {
        stop(
          "The download destination overlaps the source project.",
          call. = FALSE
        )
      }
      dirs <- c(source$dirs, producer$yml$directories)
      for (dir in dirs) {
        if (is.null(dir$path)) {
          next
        }
        path <- .get_remote_target(
          "local",
          "",
          dir,
          NULL,
          version,
          producer$head,
          optional = TRUE
        )
        if (!is.null(path)) {
          protected <- c(
            protected,
            fs::path_abs(fs::path_expand(path), start = source$root)
          )
        }
      }
      protected <- c(
        protected,
        file.path(source$root, c(".git", ".projr", "_projr", "data"))
      )
    }
  }
  for (path in unique(protected)) {
    path <- .path_resolve_links(path)
    if (
      fs::path_has_parent(out, path) ||
        any(fs::path_has_parent(path, paths) | fs::path_has_parent(paths, path))
    ) {
      stop(
        "The download destination overlaps the source: ",
        path,
        call. = FALSE
      )
    }
  }
  fs::dir_create(out)
  lockpath <- file.path(out, ".projr-get.lock")
  if (fs::link_exists(lockpath)) {
    stop("Output lock is a symbolic link.", call. = FALSE)
  }
  lock <- filelock::lock(lockpath)
  on.exit(filelock::unlock(lock), add = TRUE)
  stage <- tempfile(".projr-get-", tmpdir = out)
  fs::dir_create(stage)
  backup <- file.path(stage, ".original")
  fs::dir_create(backup)
  moved <- installed <- character()
  done <- FALSE
  on.exit(
    {
      if (!done) {
        unlink(paths[installed], recursive = TRUE)
        for (label in moved) {
          if (!file.rename(file.path(backup, label), paths[[label]])) {
            warning(
              "Original files retained in ",
              file.path(backup, label),
              call. = FALSE
            )
          }
        }
      }
      if (done || !length(list.files(backup, all.files = TRUE, no.. = TRUE))) {
        unlink(stage, recursive = TRUE)
      }
    },
    add = TRUE,
    after = FALSE
  )
  for (label in labels) {
    path <- paths[[label]]
    if (fs::link_exists(path) || (file.exists(path) && !dir.exists(path))) {
      stop("Destination is a file or link: ", path, call. = FALSE)
    }
    existing <- .restore_build_hashes(path, hashing)
    expected <- NULL
    project_snapshot <- NULL
    found <- FALSE
    preferred <- (producer$record$yml %||% producer$yml)$directories[[
      label
    ]]$source
    priority <- vapply(
      sources,
      function(source) {
        if (source$type %in% c("metadata", "project")) {
          return(1L)
        }
        selected <- preferred[[source$type]]
        if (
          source$key %in%
            c(names(selected), unlist(selected, use.names = FALSE))
        ) {
          return(2L)
        }
        if (isTRUE(source$preferred) || label %in% source$preferred) {
          return(3L)
        }
        4L
      },
      integer(1)
    )
    for (source in sources[order(priority)]) {
      snap <- .restore_build_snapshot(source$store, source$csv, label, version)
      if (source$type %in% c("metadata", "project")) {
        project_snapshot <- snap
      }
      if (is.null(snap$files)) {
        if (
          !is.null(snap$id) &&
            !is.null(project_snapshot$id) &&
            snap$id != project_snapshot$id
        ) {
          stop(
            "Sources disagree about ",
            label,
            " at ",
            version,
            ".",
            call. = FALSE
          )
        }
        if (is.null(project_snapshot$files)) {
          next
        }
        snap <- project_snapshot
      } else {
        snap$versions <- unique(c(snap$versions, project_snapshot$versions))
      }
      if (!is.null(expected) && !identical(expected, snap$files)) {
        stop(
          "Sources disagree about ",
          label,
          " at ",
          version,
          ".",
          call. = FALSE
        )
      }
      expected <- snap$files
      if (source$type == "metadata") {
        next
      }
      if (source$type != "project") {
        snap$versions <- unique(c(
          snap$versions,
          .restore_matching_versions(source$store, label, snap)
        ))
      }
      if (dir.exists(path) && identical(existing, expected)) {
        found <- TRUE
        break
      }
      if (
        !overwrite &&
          length(existing) &&
          !identical(unname(existing), unname(expected[names(existing)]))
      ) {
        stop(
          "Local files in ",
          path,
          " differ from ",
          version,
          ". Use overwrite = TRUE to replace them.",
          call. = FALSE
        )
      }
      dest <- file.path(stage, label)
      fs::dir_create(dest)
      if (source$type == "github") {
        found <- .get_github_label(
          source,
          label,
          version,
          snap,
          dest,
          workspace,
          hashing
        )
      } else {
        found <- .get_local_label(
          source,
          producer,
          label,
          version,
          snap,
          dest,
          hashing
        )
      }
      if (found) {
        break
      }
      unlink(dest, recursive = TRUE)
    }
    if (!found) {
      stop("No matching files for ", label, " at ", version, ".", call. = FALSE)
    }
  }
  for (label in labels[dir.exists(file.path(stage, labels))]) {
    path <- paths[[label]]
    if (dir.exists(path)) {
      if (!file.rename(path, file.path(backup, label))) {
        stop("Could not move existing ", path, call. = FALSE)
      }
      moved <- c(moved, label)
    }
    if (!file.rename(file.path(stage, label), path)) {
      stop("Could not install ", path, call. = FALSE)
    }
    installed <- c(installed, label)
  }
  done <- TRUE
  .cli_success("Verified {paste(labels, collapse = ', ')} for build {version}")
  paths
}

.get_local_label <- function(
  source,
  producer,
  label,
  version,
  snap,
  dest,
  hashing
) {
  if (source$type == "project") {
    path <- if (label == "data") "data" else source$dirs[[label]]$path
    if (is.null(path)) {
      return(FALSE)
    }
    path <- .get_remote_target(
      "local",
      label,
      list(path = path),
      NULL,
      version,
      producer$head,
      optional = TRUE
    )
    if (is.null(path)) {
      return(FALSE)
    }
    candidates <- fs::path_abs(fs::path_expand(path), start = source$root)
  } else {
    candidates <- vapply(
      unique(c(version, snap$versions)),
      function(v) {
        .remote_get_path_rel_hierarchy(
          source$root,
          source$append,
          label,
          source$structure,
          v,
          pre = FALSE,
          empty = !length(snap$files)
        )
      },
      character(1)
    )
  }
  for (path in unique(candidates)) {
    if (
      source$type == "project" &&
        !length(snap$files) &&
        !file.exists(path) &&
        !fs::link_exists(path)
    ) {
      return(TRUE)
    }
    if (!dir.exists(path) || fs::link_exists(path)) {
      next
    }
    path <- as.character(fs::path_real(path))
    if (
      length(fs::dir_ls(path, recurse = TRUE, all = TRUE, type = "symlink"))
    ) {
      stop("Cannot verify symbolic links in ", path, ".", call. = FALSE)
    }
    if (
      source$type == "local" && !source$append && source$structure == "latest"
    ) {
      if (!length(snap$files)) {
        latest <- .remote_get_version_latest_label_non_project_file_extract(
          .remote_get_version_file_read(file.path(path, "VERSION")),
          label
        )
        if (
          length(latest) != 1L || !.version_v_add(latest) %in% snap$versions
        ) {
          next
        }
      }
      root <- structure(path, label_at_root = TRUE)
      files <- .remote_file_ls_local(root)
    } else {
      files <- .hash_dir_files(path, .build_label_get_dir_exc(label))
    }
    if (!setequal(files, names(snap$files))) {
      next
    }
    if (length(files)) {
      .dir_copy_file(files, path, dest)
    }
    if (identical(.restore_build_hashes(dest, hashing), snap$files)) {
      return(TRUE)
    }
    unlink(
      file.path(dest, list.files(dest, all.files = TRUE, no.. = TRUE)),
      recursive = TRUE
    )
  }
  FALSE
}

.get_github_label <- function(
  source,
  label,
  version,
  snap,
  dest,
  workspace,
  hashing
) {
  # A recorded empty directory has no payload to transfer.
  if (!length(snap$files)) {
    return(TRUE)
  }
  versions <- unique(c(version, snap$versions))
  candidates <- vapply(
    versions,
    function(v) {
      .remote_get_path_rel_github(
        source$prefix,
        source$append,
        label,
        source$structure,
        v,
        FALSE
      )
    },
    character(1)
  )
  # A concrete tag identifies a release directly, including its latest layout.
  if (isTRUE(source$direct_tag)) {
    candidates <- c(
      candidates,
      .remote_get_path_rel_github(NULL, TRUE, label, "latest", version, FALSE)
    )
  }
  asset <- intersect(candidates, source$assets)
  if (!length(asset)) {
    return(FALSE)
  }
  zip <- .remote_file_get_all_github_httr(
    source$repo,
    source$tag,
    asset[1L],
    workspace
  )
  .restore_build_unpack(zip, dest, snap$files, hashing)
  TRUE
}
