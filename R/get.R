#' Download directories from another projr analysis
#'
#' @description Copy whole directories from a completed build into
#'   `out/<label>/`. projr verifies each file against the build manifest
#'   before installing any of the selected directories.
#' @details To inspect an archive's records, open `manifests/<label>/versions.csv`
#'   at a local destination. On GitHub, download the release's `manifest.zip`
#'   and open `<label>/versions.csv` inside it. The `snapshot` column
#'   identifies the matching `files-<snapshot>.csv`, which
#'   lists the expected paths and hashes. Several builds can share a file list.
#'   To query build history in a checkout, use [projr_manifest_changes()] or
#'   [projr_manifest_file_history()].
#' @param type A source type, `"local"` or `"github"`.
#' @param ... Arguments for `projr_get_local()` or `projr_get_github()`.
#' @param path Path to the local analysis that produced the files.
#' @param repo A producing GitHub project in `owner/repo` or
#'   `owner/repo@branch` form. Without a branch, use its default branch.
#' @param remote Title of a remote in the current project's configuration.
#'   Supply exactly one of this title and the direct source (`path` or `repo`).
#' @param tag Concrete GitHub release tag. With a direct repository, a tag
#'   identifies the release without looking up the producer's configuration.
#' @param label Directory labels to copy. By default, copy the labels recorded
#'   for the selected build at the source.
#' @param version Exact build version, with or without a leading `v`. Defaults
#'   to the producer's completed build or the release's recorded project version.
#' @param out Directory to receive a subdirectory for each label.
#' @param overwrite Allow replacement of changed or extra files in the selected
#'   output directories. Matching files can be reused with either setting.
#' @return A named character vector of absolute paths to the verified directories.
#' @examples
#' \dontrun{
#' projr_get_local(path = "../analysis", label = "output", out = "downloads")
#' projr_get_github(repo = "owner/analysis@main", label = "raw-data")
#' projr_get("github", remote = "reports", version = "1.2.0", out = "downloads")
#' }
#' @export
projr_get <- function(type, ...) {
  .assert_in_single(type, c("local", "github"), TRUE)
  switch(type, local = projr_get_local(...), github = projr_get_github(...))
}

#' @rdname projr_get
#' @export
projr_get_local <- function(
  path = NULL,
  remote = NULL,
  label = NULL,
  version = NULL,
  out = ".",
  overwrite = FALSE
) {
  .get_content("local", path, remote, NULL, label, version, out, overwrite)
}

#' @rdname projr_get
#' @export
projr_get_github <- function(
  repo = NULL,
  remote = NULL,
  tag = NULL,
  label = NULL,
  version = NULL,
  out = ".",
  overwrite = FALSE
) {
  .get_content("github", repo, remote, tag, label, version, out, overwrite)
}

.get_content <- function(
  type,
  direct,
  remote,
  tag,
  label,
  version,
  out,
  overwrite
) {
  .assert_string(direct)
  .assert_string(remote)
  if (is.null(direct) == is.null(remote)) {
    stop(
      "Supply exactly one of ",
      if (type == "local") "path" else "repo",
      " and remote.",
      call. = FALSE
    )
  }
  .assert_string(tag)
  .assert_directory_labels(label)
  .assert_string(version)
  if (!is.null(version)) {
    .assert_detect_single(version, "^v?[0-9]+([.-][0-9]+)*$", TRUE)
    version <- .version_v_add(version)
  }
  .assert_string(out, TRUE)
  .assert_flag(overwrite, TRUE)
  out <- .path_resolve_links(fs::path_abs(out))
  workspace <- tempfile("projr-get-")
  fs::dir_create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)

  if (!is.null(remote) || type == "local") {
    root <- if (is.null(remote)) fs::path_real(direct) else .path_get()
    producer <- with_dir(root, {
      if (!identical(fs::path_real(.path_get()), fs::path_real(root))) {
        stop(
          "path must be the root of the producing projr analysis.",
          call. = FALSE
        )
      }
      record <- .get_build_record(.buildlog_read(), version)
      list(
        root = root,
        yml = .yml_get(NULL),
        record = record,
        version = record$version %||%
          tryCatch(.restore_build_version(version), error = function(e) NULL),
        head = .template_checkout_head(),
        store = .manifest_project_dir(),
        hashing = .yml_settings_hashing()
      )
    })
  } else {
    if (
      !grepl(
        "^[a-zA-Z0-9][a-zA-Z0-9-]*/[a-zA-Z0-9_.-]+(@[^[:space:]]+)?$",
        direct
      ) ||
        basename(sub("@.*$", "", direct)) %in% c(".", "..")
    ) {
      stop("repo must be owner/repo or owner/repo@branch.", call. = FALSE)
    }
    repo <- sub("@.*$", "", direct)
    if (is.null(tag)) {
      branch <- if (repo != direct) substring(direct, nchar(repo) + 2L)
      producer <- .get_github_project(repo, branch, version, label, workspace)
    } else {
      producer <- list(repo = repo)
    }
  }

  sources <- list()
  record <- producer$record
  if (is.null(remote) && is.null(tag)) {
    version <- version %||% producer$version
  }
  if (type == "local" && is.null(remote)) {
    sources <- list(list(
      type = "project",
      store = producer$store,
      csv = file.path(root, "manifest.csv"),
      root = root,
      dirs = (record$yml %||% producer$yml)$directories
    ))
  }
  if (!is.null(producer$metadata)) {
    sources <- list(producer$metadata)
  }
  yml <- if (is.null(remote)) record$yml %||% producer$yml else producer$yml
  titles <- if (!is.null(remote)) {
    remote
  } else if (!is.null(tag)) {
    tag
  } else {
    unique(c(
      names(yml$remotes[[type]]),
      names(yml$build[[type]]),
      names(producer$yml$remotes[[type]]),
      "archive"
    ))
  }
  for (key in titles) {
    def <- .yml_remote_definition(type, key, yml = yml) %||%
      .yml_remote_definition(type, key, yml = producer$yml)
    if (!is.null(remote) && !is.null(def) && !is.null(version)) {
      def <- .yml_remote_definition(type, key, record$yml) %||% def
    }
    if (is.null(def) && !is.null(remote)) {
      stop("Unknown ", type, " remote: ", remote, call. = FALSE)
    }
    if (is.null(remote) && is.null(tag) && identical(def$source, FALSE)) {
      next
    }
    target <- .get_remote_target(
      type,
      key,
      def,
      record,
      version %||% producer$version,
      producer$head,
      tag,
      optional = is.null(remote) && is.null(tag)
    )
    if (is.null(target)) {
      next
    }
    source <- list(
      type = type,
      key = key,
      structure = def$structure %||% "archive",
      append = def[["path-append-label"]] %||% TRUE,
      prefix = def$path,
      preferred = def$source
    )
    if (type == "local") {
      source$root <- as.character(fs::path_abs(
        fs::path_expand(target),
        start = root
      ))
      source$store <- file.path(source$root, "manifests")
      source$csv <- file.path(source$root, "manifest.csv")
    } else {
      source$repo <- def$repo %||% producer$repo %||% .gh_repo_get()
      .assert_string(source$repo, TRUE)
      if (
        !grepl("^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$", source$repo) ||
          basename(source$repo) %in% c(".", "..")
      ) {
        stop("Invalid GitHub repository: ", source$repo, call. = FALSE)
      }
      source$tag <- target
      source$direct_tag <- !is.null(tag) && is.null(remote)
      if (!.remote_check_exists_github_httr(source$repo, target)) {
        next
      }
      source$assets <- .remote_ls_final_github_httr(source$repo, target)
      metadata <- tempfile(tmpdir = workspace)
      fs::dir_create(metadata)
      source$store <- file.path(metadata, "manifests")
      source$csv <- file.path(source$store, "manifest.csv")
      if ("manifest.zip" %in% source$assets) {
        .remote_manifest_download(
          source$repo,
          target,
          source$store,
          label,
          version
        )
      } else if ("manifest.csv" %in% source$assets) {
        .remote_file_get_all_github_httr(
          source$repo,
          target,
          "manifest.csv",
          source$store
        )
      }
    }
    if (.manifest_store_ready(source$store) || file.exists(source$csv)) {
      sources[[length(sources) + 1L]] <- source
    }
  }
  if (!length(sources)) {
    stop("No published manifest found for the selected source.", call. = FALSE)
  }
  version <- version %||% .get_archive_version(sources[[1L]], workspace)
  if (is.null(version)) {
    stop(
      "No completed build recorded at this source. Supply version.",
      call. = FALSE
    )
  }
  if (is.null(label)) {
    indexes <- lapply(sources, function(source) {
      if (.manifest_store_ready(source$store)) {
        .manifest_store_index(source$store)
      } else {
        .manifest_read(source$csv)
      }
    })
    actual <- indexes[vapply(
      sources,
      function(s) s$type != "metadata",
      logical(1)
    )]
    label <- unique(unlist(
      lapply(actual, function(index) index$label[index$version == version]),
      use.names = FALSE
    ))
    if (!is.null(producer$metadata)) {
      project <- indexes[[1L]]
      for (key in setdiff(
        unique(project$label[project$version == version]),
        label
      )) {
        snapshot <- .restore_build_snapshot(
          producer$metadata$store,
          producer$metadata$csv,
          key,
          version
        )
        present <- vapply(
          sources,
          function(source) {
            source$type != "metadata" &&
              length(.restore_matching_versions(source$store, key, snapshot)) >
                0L
          },
          logical(1)
        )
        if (!any(present)) {
          present <- vapply(
            actual,
            function(index) {
              any(index$label == key & index$version %in% snapshot$versions)
            },
            logical(1)
          )
        }
        if (any(present)) label <- c(label, key)
      }
    }
  }
  if (!length(label)) {
    stop("No published labels for ", version, ".", call. = FALSE)
  }
  .assert_directory_labels(label)
  .get_install(
    sources,
    unique(label),
    version,
    producer,
    out,
    overwrite,
    workspace
  )
}

.get_archive_version <- function(source, workspace) {
  if (source$type == "github") {
    if (!"VERSION.zip" %in% source$assets) {
      return(NULL)
    }
    zip <- .remote_file_get_all_github_httr(
      source$repo,
      source$tag,
      "VERSION.zip",
      workspace,
      asset = source$version_asset
    )
    if (sum(utils::unzip(zip, list = TRUE)$Name == "VERSION") != 1L) {
      return(NULL)
    }
    con <- unz(zip, "VERSION")
    on.exit(close(con))
    text <- readLines(con, warn = FALSE)
  } else {
    text <- .remote_get_version_file_read(file.path(source$root, "VERSION"))
  }
  # Older VERSION files can contain just the version instead of labelled lines.
  text <- trimws(text)
  if (length(text) != 1L || grepl(":", text, fixed = TRUE)) {
    text <- sub(
      "^Project:[[:space:]]*",
      "",
      grep("^Project:", text, value = TRUE)
    )
  }
  value <- sub("\\*$", "", text)
  if (
    length(value) == 1L &&
      grepl("^v?[0-9]+([.-][0-9]+)*$", value) &&
      !is.na(numeric_version(sub("^v", "", value), strict = FALSE))
  ) {
    .version_v_add(value)
  }
}
