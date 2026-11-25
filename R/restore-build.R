#' Restore the files used by a checked-out build
#'
#' `projr_restore()` restores raw inputs from the GitHub release published by
#' `projr_build_patch(archive_github = TRUE)` and the other production builds.
#' Run it inside the checked-out project. It verifies file paths and MD5s before
#' installing the inputs. Files already matching the recorded snapshot are kept.
#'
#' @inheritParams projr_content_update
#' @param version Character. An exact production build version. By default projr
#'   reads the checkout's version. On the follow-up development commit it uses
#'   the most recent completed build in the checkout's `.projr/BUILDLOG.md`.
#' @param destination Character. A registered GitHub remote title or a concrete
#'   release tag in the checkout's repository. Defaults to `"archive"`. For a
#'   registered title, use the repository and release tag for the selected build.
#' @param clear Logical. Allow replacement of existing input files that differ
#'   from the snapshot, including extra files. The default fills missing files
#'   and stops if local changes would be lost.
#' @return Invisibly returns `TRUE` after restoring and verifying the selected
#'   directories, or `FALSE` if the checkout declares no raw directories.
#' @examples
#' \dontrun{
#' projr_restore()
#' projr_restore(label = "raw-measurements", version = "1.2.3")
#' }
#' @rdname projr_restore
#' @export
projr_restore <- function(
  label = NULL,
  version = NULL,
  destination = "archive",
  clear = FALSE
) {
  .assert_chr(label)
  .assert_string(destination, TRUE)
  .assert_flag(clear, TRUE)
  .project_state_upgrade(.path_get())
  directories <- .yml_get(NULL)[["directories"]]
  declared <- names(directories)
  if (is.null(label)) {
    label <- declared[.yml_dir_label_class_detect_raw(declared)]
  }
  if (!length(label)) {
    return(invisible(FALSE))
  }
  labels <- unique(label)
  .assert_in(labels, declared, TRUE)
  version <- .restore_build_version(version)
  missing_version <- !.version_exists()
  if (
    missing_version &&
      is.na(Sys.getenv("PROJR_BUILD_VERSION", unset = NA_character_))
  ) {
    # Resolve @version in directory paths before the recovered VERSION is installed.
    Sys.setenv(PROJR_BUILD_VERSION = version)
    on.exit(Sys.unsetenv("PROJR_BUILD_VERSION"), add = TRUE)
  }
  workspace <- tempfile("projr-restore-")
  fs::dir_create(workspace)
  on.exit(unlink(workspace, recursive = TRUE, expand = FALSE), add = TRUE)

  # The checkout's own manifest records what each label should contain.
  project <- list(
    type = "metadata",
    store = .path_get(".projr", "manifests"),
    csv = .path_get("manifest.csv")
  )
  snapshots <- lapply(labels, function(label) {
    .restore_build_snapshot(project$store, project$csv, label, version)
  })
  paths <- vapply(
    labels,
    projr_path_get_dir,
    character(1),
    safe = FALSE,
    format = "absolute",
    create = FALSE
  )
  .restore_build_check_paths(paths)
  .cli_info("Checking {paste(labels, collapse = ', ')} against build {version}")
  todo <- !vapply(
    seq_along(labels),
    function(i) {
      !is.null(snapshots[[i]]$files) &&
        dir.exists(paths[i]) &&
        identical(.restore_build_hashes(paths[i]), snapshots[[i]]$files)
    },
    logical(1)
  )
  # Recorded empty directories need no release access.
  release <- todo &
    vapply(
      snapshots,
      function(snapshot) {
        is.null(snapshot$files) || length(snapshot$files) > 0L
      },
      logical(1)
    )
  sources <- list(project)
  producer <- list()
  if (any(release)) {
    record <- .get_build_record(.buildlog_read(), version)
    registered <- destination %in%
      c(names(record$yml$remotes$github), names(.yml_get(NULL)$remotes$github))
    resolved <- .get_resolve(
      "github",
      if (!registered) .gh_repo_get(),
      if (registered) destination,
      if (!registered) destination,
      labels[release],
      version,
      workspace,
      metadata = project
    )
    sources <- resolved$sources
    producer <- resolved$producer
  }
  producer$hashing <- .yml_settings_hashing()
  if (any(todo)) {
    .get_install(
      sources,
      labels[todo],
      version,
      producer,
      workspace,
      clear,
      workspace,
      paths = paths[todo],
      option = "clear"
    )
  }
  # Record snapshots recovered from the release in the checkout's manifest.
  recovered <- list()
  for (i in which(release & vapply(snapshots, \(x) is.null(x$files), TRUE))) {
    archive <- sources[[length(sources)]]
    snapshot <- .restore_build_snapshot(
      archive$store,
      archive$csv,
      labels[i],
      version
    )
    rows <- snapshot$rows
    first <- snapshot$versions[1L]
    if (
      first != version &&
        is.null(.restore_build_snapshot(
          project$store,
          project$csv,
          labels[i],
          first
        ))
    ) {
      original <- rows
      original$version <- first
      rows <- rbind(original, rows)
    }
    recovered[[labels[i]]] <- rows
  }
  if (length(recovered)) {
    .manifest_store_write(do.call(rbind, recovered), project$store, project$csv)
  }
  if (missing_version) {
    .version_set_file(version)
  }
  if (!any(todo)) {
    .cli_success(
      "Verified {paste(labels, collapse = ', ')} for build {version}"
    )
  }
  invisible(TRUE)
}

.restore_build_version <- function(version) {
  if (is.null(version)) {
    version <- .version_completed_file() %||% .buildlog_last_version()
    if (is.null(version)) {
      stop(
        "No completed build recorded in this checkout. Supply version.",
        call. = FALSE
      )
    }
  }
  version <- .version_v_add(version)
  .version_check(version)
  version
}

.version_completed_file <- function() {
  if (.version_exists()) {
    components <- .version_format_list_get(NULL)$component
    if (
      !"dev" %in% components ||
        length(.version_current_vec_get()) != length(components)
    ) {
      return(.version_get_v())
    }
  }
  NULL
}

.buildlog_last_version <- function() {
  headers <- grep(
    "^#{2,4} v[0-9]+([.-][0-9]+)*:",
    .buildlog_read(),
    value = TRUE
  )
  if (length(headers)) sub("^#{2,4} (v[0-9.-]+):.*$", "\\1", headers[1L])
}

.restore_build_snapshot <- function(store, csv, label, version) {
  id <- NULL
  if (.manifest_store_ready(store)) {
    index <- .manifest_store_index(store, label)
    hit <- match(version, index$version)
    if (is.na(hit)) {
      return(NULL)
    }
    id <- index$snapshot[hit]
    versions <- index$version[index$snapshot == id]
    row <- index[hit, , drop = FALSE]
    if (!file.exists(file.path(store, .remote_manifest_snapshots(row)))) {
      return(list(id = id))
    }
    rows <- .manifest_store_expand(row, store)
  } else {
    history <- .manifest_read(csv)
    history <- history[history$label == label, , drop = FALSE]
    rows <- history[history$version == version, , drop = FALSE]
    if (!nrow(rows)) {
      return(NULL)
    }
    files <- .restore_build_files(rows)
    canonical <- tempfile()
    on.exit(unlink(canonical))
    .manifest_store_write_files(
      rows[nzchar(rows$fn), , drop = FALSE],
      canonical
    )
    id <- digest::digest(file = canonical, algo = "sha256")
    groups <- split(history, history$version)
    versions <- names(Filter(
      function(x) identical(.restore_build_files(x), files),
      groups
    ))
  }
  numbers <- numeric_version(sub("^v", "", versions))
  versions <- versions[order(numbers, versions, method = "radix")]
  versions <- versions[seq_len(match(version, versions))]
  list(
    files = .restore_build_files(rows),
    versions = versions,
    rows = rows,
    id = id
  )
}

# A later upload can repair an archive when its index identifies the exact
# snapshot independently recorded by the requested build.
.restore_matching_versions <- function(store, label, snapshot) {
  if (is.null(snapshot$id) || !.manifest_store_ready(store)) {
    return(character())
  }
  index <- .manifest_store_index(store, label)
  index$version[index$snapshot == snapshot$id]
}

.restore_build_files <- function(rows) {
  files <- rows$fn
  if (identical(files, "") && identical(rows$hash, "")) {
    return(character())
  }
  if (
    !is.character(files) ||
      !is.character(rows$hash) ||
      anyNA(files) ||
      any(!nzchar(files)) ||
      anyDuplicated(files) ||
      any(grepl("^/|^[A-Za-z]:|\\\\|(^|/)\\.{1,2}(/|$)|//|/$", files)) ||
      anyNA(rows$hash) ||
      any(!grepl("^[0-9a-f]{32}$", rows$hash))
  ) {
    stop("Invalid file list in build snapshot.", call. = FALSE)
  }
  hashes <- stats::setNames(rows$hash, files)
  hashes[order(files, method = "radix")]
}

.restore_build_hashes <- function(path, hashing = .yml_settings_hashing()) {
  if (!dir.exists(path)) {
    return(character())
  }
  if (length(fs::dir_ls(path, type = "symlink", recurse = TRUE, all = TRUE))) {
    stop("Cannot verify symbolic links in ", path, ".", call. = FALSE)
  }
  files <- .hash_dir_files(path, NULL)
  if (!length(files)) {
    return(character())
  }
  files <- sort(files, method = "radix")
  stats::setNames(
    unname(.hash_file(file.path(path, files), hashing$approach, hashing$cores)),
    files
  )
}

.restore_build_check_paths <- function(paths) {
  root <- as.character(fs::path_real(.path_get()))
  paths <- vapply(
    paths,
    function(path) {
      if (file.exists(path) && !dir.exists(path) || fs::link_exists(path)) {
        stop("Restore destination is a file or link: ", path, call. = FALSE)
      }
      parent <- path
      while (!dir.exists(parent)) {
        if (identical(parent, dirname(parent))) {
          stop("No accessible parent for ", path, ".", call. = FALSE)
        }
        parent <- dirname(parent)
      }
      as.character(fs::path_abs(
        fs::path_rel(path, parent),
        start = fs::path_real(parent)
      ))
    },
    character(1)
  )
  for (i in seq_along(paths)) {
    if (
      fs::path_has_parent(root, paths[i]) ||
        fs::path_has_parent(paths[i], file.path(root, ".git")) ||
        fs::path_has_parent(paths[i], file.path(root, ".projr")) ||
        fs::path_has_parent(paths[i], file.path(root, "_projr")) ||
        any(fs::path_has_parent(paths[-i], paths[i]))
    ) {
      stop(
        "Overlapping or protected restore directory: ",
        paths[i],
        call. = FALSE
      )
    }
  }
}

.restore_build_unpack <- function(
  zip,
  directory,
  expected,
  hashing = .yml_settings_hashing()
) {
  if (length(zip) != 1L || !file.exists(zip)) {
    stop("Missing archive download.", call. = FALSE)
  }
  entries <- utils::unzip(zip, list = TRUE)$Name
  files <- entries[!endsWith(entries, "/")]
  if (anyDuplicated(files) || !setequal(files, names(expected))) {
    stop(
      "Archive file list does not match the selected build snapshot.",
      call. = FALSE
    )
  }
  # Extract only the exact, validated paths recorded in the manifest.
  withCallingHandlers(
    utils::unzip(zip, files = files, exdir = directory),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  if (!identical(.restore_build_hashes(directory, hashing), expected)) {
    stop(
      "Archive file hash does not match the selected build snapshot.",
      call. = FALSE
    )
  }
}
