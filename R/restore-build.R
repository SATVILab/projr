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

  project_store <- .path_get(".projr", "manifests")
  project_csv <- .path_get("manifest.csv")
  snapshots <- lapply(labels, function(label) {
    .restore_build_snapshot(project_store, project_csv, label, version)
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
  current <- lapply(paths, .restore_build_hashes)
  complete <- vapply(
    seq_along(labels),
    function(i) {
      !is.null(snapshots[[i]]$files) &&
        dir.exists(paths[i]) &&
        identical(current[[i]], snapshots[[i]]$files)
    },
    logical(1)
  )
  store <- file.path(workspace, "manifests")
  needs_release <- !complete &
    vapply(
      snapshots,
      function(snapshot) {
        is.null(snapshot$files) || length(snapshot$files) > 0L
      },
      logical(1)
    )
  if (any(needs_release)) {
    record <- .get_build_record(.buildlog_read(), version)
    yml <- record$yml
    if (!destination %in% names(yml$remotes$github)) {
      yml <- .yml_get(NULL)
    }
    settings <- yml$remotes$github[[destination]]
    if (destination %in% names(yml$remotes$github)) {
      destination <- .get_remote_target(
        "github",
        destination,
        settings,
        record,
        version,
        .template_checkout_head()
      )
    }
    repo <- settings[["repo"]] %||% .gh_repo_get()
    .cli_debug(
      "Looking up snapshots for {version} in GitHub release {repo}/{destination}"
    )
    assets <- .remote_ls_final_github_httr(repo, destination)
    if ("manifest.zip" %in% assets) {
      .remote_manifest_download(
        repo,
        destination,
        store,
        label = labels[needs_release],
        version = version
      )
    } else if ("manifest.csv" %in% assets) {
      .remote_file_get_all_github_httr(repo, destination, "manifest.csv", store)
    }
  }
  tmp <- backup <- rep("", length(labels))
  installed <- rep(FALSE, length(labels))
  success <- FALSE
  on.exit(
    {
      for (i in which(nzchar(tmp))) {
        if (!success && installed[i]) {
          unlink(paths[i], recursive = TRUE, expand = FALSE)
        }
        if (dir.exists(backup[i])) {
          if (success) {
            unlink(backup[i], recursive = TRUE, expand = FALSE)
          } else if (!file.rename(backup[i], paths[i])) {
            warning("Original files retained in ", backup[i], call. = FALSE)
          }
        }
        unlink(tmp[i], recursive = TRUE, expand = FALSE)
      }
    },
    add = TRUE
  )
  recovered <- list()
  for (i in which(!complete)) {
    label <- labels[i]
    remote <- .restore_build_snapshot(
      store,
      file.path(store, "manifest.csv"),
      label,
      version
    )
    local <- snapshots[[i]]
    if (
      !is.null(local) && !is.null(remote) && !identical(local$id, remote$id)
    ) {
      stop(
        "Project and release disagree about ",
        label,
        " at ",
        version,
        ".",
        call. = FALSE
      )
    }
    snapshot <- if (!is.null(local$files)) local else remote
    if (is.null(snapshot$files)) {
      stop("No snapshot for ", label, " at ", version, ".", call. = FALSE)
    }
    if (is.null(local$files)) {
      recovered[[label]] <- snapshot$rows
      first <- snapshot$versions[1L]
      if (
        first != version &&
          is.null(.restore_build_snapshot(
            project_store,
            project_csv,
            label,
            first
          ))
      ) {
        original <- snapshot$rows
        original$version <- first
        recovered[[label]] <- rbind(original, snapshot$rows)
      }
    }
    if (dir.exists(paths[i]) && identical(current[[i]], snapshot$files)) {
      next
    }
    if (
      !clear &&
        length(current[[i]]) &&
        !identical(
          unname(current[[i]]),
          unname(snapshot$files[names(current[[i]])])
        )
    ) {
      stop(
        "Local files in ",
        paths[i],
        " differ from ",
        version,
        ". Use clear = TRUE to replace them.",
        call. = FALSE
      )
    }

    fs::dir_create(dirname(paths[i]))
    tmp[i] <- tempfile(".projr-restore-", tmpdir = dirname(paths[i]))
    backup[i] <- tempfile(".projr-original-", tmpdir = dirname(paths[i]))
    fs::dir_create(tmp[i])
    if (length(snapshot$files)) {
      versions <- unique(c(
        version,
        snapshot$versions,
        remote$versions,
        .restore_matching_versions(store, label, snapshot)
      ))
      candidates <- vapply(
        versions,
        function(v) {
          .remote_get_path_rel_github(
            settings[["path"]],
            settings[["path-append-label"]] %||% TRUE,
            label,
            settings[["structure"]] %||% "archive",
            v,
            FALSE
          )
        },
        character(1)
      )
      asset <- intersect(candidates, assets)
      if (!length(asset)) {
        stop(
          "No matching archive for ",
          label,
          " at ",
          version,
          " in release ",
          destination,
          ".",
          call. = FALSE
        )
      }
      .cli_info("Downloading {asset[1L]} from GitHub release {destination}")
      zip <- .remote_file_get_all_github_httr(
        repo,
        destination,
        asset[1L],
        workspace
      )
      .restore_build_unpack(zip, tmp[i], snapshot$files)
    }
  }
  # All selected inputs have been checked. Rename within each destination's filesystem.
  for (i in which(nzchar(tmp))) {
    if (dir.exists(paths[i]) && !file.rename(paths[i], backup[i])) {
      stop("Could not move existing inputs from ", paths[i], ".", call. = FALSE)
    }
    if (!file.rename(tmp[i], paths[i])) {
      stop("Could not install inputs in ", paths[i], ".", call. = FALSE)
    }
    installed[i] <- TRUE
  }
  if (length(recovered)) {
    .manifest_store_write(do.call(rbind, recovered), project_store, project_csv)
  }
  if (missing_version) {
    .version_set_file(version)
  }
  success <- TRUE
  .cli_success("Verified {paste(labels, collapse = ', ')} for build {version}")
  invisible(TRUE)
}

.restore_build_version <- function(version) {
  if (!is.null(version)) {
    version <- .version_v_add(version)
  } else if (.version_exists()) {
    components <- .version_format_list_get(NULL)$component
    if (
      !"dev" %in% components ||
        length(.version_current_vec_get()) != length(components)
    ) {
      return(.version_get_v())
    }
  }
  if (is.null(version)) {
    headers <- grep(
      "^#{2,4} v[0-9]+([.-][0-9]+)*:",
      .buildlog_read(),
      value = TRUE
    )
    if (!length(headers)) {
      stop(
        "No completed build recorded in this checkout. Supply version.",
        call. = FALSE
      )
    }
    version <- sub("^#{2,4} (v[0-9.-]+):.*$", "\\1", headers[1L])
  }
  .version_check(version)
  version
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
