.reproduce_output_labels <- function(labels) {
  labels[
    .yml_dir_label_class_detect_output(labels) |
      .yml_dir_label_class_detect_docs(labels)
  ]
}

.reproduce_baseline <- function(version, destination = "archive") {
  version <- .version_v_add(version)
  record <- .get_build_record(.buildlog_read(), version)
  yml <- record$yml %||% .yml_get(NULL)
  store <- .manifest_project_dir()
  csv <- .path_get("manifest.csv")
  labels <- names(yml$directories)
  if (is.null(record$yml$directories)) {
    # Older build logs can lack the merged configuration. Use any labels
    # actually recorded for this version as well as the checkout's declarations.
    index <- if (.manifest_store_ready(store)) {
      .manifest_store_index(store)
    } else {
      .manifest_read(csv)
    }
    labels <- union(labels, index$label[index$version == version])
  }
  labels <- as.character(.reproduce_output_labels(labels))
  snapshots <- lapply(stats::setNames(labels, labels), function(label) {
    .restore_build_snapshot(store, csv, label, version)
  })
  needed <- labels[vapply(snapshots, function(x) is.null(x$files), logical(1L))]
  if (length(needed)) {
    stage <- tempfile("projr-reproduce-manifest-")
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    archive <- .reproduce_archive_metadata(
      destination,
      record,
      yml,
      needed,
      stage
    )
    for (label in needed) {
      if (is.null(archive)) {
        break
      }
      original <- snapshots[[label]]
      remote <- .restore_build_snapshot(
        archive$store,
        archive$csv,
        label,
        version
      )
      if (!is.null(original$id)) {
        if (!is.null(remote$id) && original$id != remote$id) {
          stop(
            "Project and archive disagree about ",
            label,
            " at ",
            version,
            ".",
            call. = FALSE
          )
        }
        if (is.null(remote$files)) {
          matching <- .restore_matching_versions(archive$store, label, original)
          if (length(matching)) {
            remote <- .restore_build_snapshot(
              archive$store,
              archive$csv,
              label,
              matching[1L]
            )
          }
        }
      }
      snapshots[label] <- list(remote)
    }
  }
  lapply(snapshots, function(x) x$files)
}

.reproduce_archive_metadata <- function(
  destination,
  record,
  yml,
  labels,
  stage
) {
  head <- .template_head()
  github <- .yml_remote_definition("github", destination, yml)
  repo <- github$repo %||% getOption("projr.github.repo")
  if (is.null(repo)) {
    url <- .gh_git_remote_url(c("-C", "."), "origin")
    if (
      grepl(
        "^(https?://|ssh://git@|git@)github\\.com[/:]",
        url,
        ignore.case = TRUE
      )
    ) {
      repo <- .gh_owner_repo_from_url(url)
    }
  }
  if (is.null(repo)) {
    local <- .yml_remote_definition("local", destination, yml)
    if (!is.null(github) || (is.null(local) && destination != "archive")) {
      return(NULL)
    }
    path <- .get_remote_target(
      "local",
      destination,
      local,
      record,
      record$version,
      head,
      optional = TRUE
    )
    if (is.null(path)) {
      return(NULL)
    }
    root <- .path_resolve_root(path)
    return(list(
      store = file.path(root, "manifests"),
      csv = file.path(root, "manifest.csv")
    ))
  }
  tag <- .get_remote_target(
    "github",
    destination,
    github,
    record,
    record$version,
    head,
    optional = TRUE
  )
  if (is.null(tag) || !.remote_check_exists_github_httr(repo, tag)) {
    return(NULL)
  }
  assets <- .remote_ls_final_github_httr(repo, tag)
  store <- file.path(stage, "manifests")
  csv <- file.path(store, "manifest.csv")
  fs::dir_create(store)
  if ("manifest.zip" %in% assets) {
    .remote_manifest_download(repo, tag, store, label = labels)
  } else if ("manifest.csv" %in% assets) {
    .remote_file_get_all_github_httr(repo, tag, "manifest.csv", store)
  }
  list(store = store, csv = csv)
}

.reproduce_compare <- function(baseline) {
  current <- .reproduce_output_labels(names(.yml_dir_get(NULL)))
  labels <- as.character(union(names(baseline), current))
  missing_count <- rep(NA_integer_, length(labels))
  dirs <- data.frame(
    label = labels,
    exact = rep(FALSE, length(labels)),
    status = rep("unrecorded", length(labels)),
    identical = missing_count,
    changed = missing_count,
    missing = missing_count,
    extra = missing_count
  )
  details <- list(data.frame(
    label = character(),
    path = character(),
    status = character()
  ))
  for (i in seq_along(labels)) {
    label <- labels[i]
    expected <- baseline[[label]]
    if (label %in% names(baseline) && is.null(expected)) {
      next
    }
    path <- if (label %in% current) {
      projr_path_get_dir(
        label,
        safe = FALSE,
        create = FALSE,
        format = "absolute"
      )
    }
    exists <- !is.null(path) && dir.exists(path)
    actual <- if (exists) .restore_build_hashes(path) else character()
    paths <- sort(
      as.character(union(names(expected), names(actual))),
      method = "radix"
    )
    status <- rep("identical", length(paths))
    common <- paths %in% names(actual) & paths %in% names(expected)
    status[common] <- ifelse(
      expected[paths[common]] == actual[paths[common]],
      "identical",
      "changed"
    )
    status[!paths %in% names(expected)] <- "extra"
    status[!paths %in% names(actual)] <- "missing"
    details[[i + 1L]] <- data.frame(
      label = rep(label, length(paths)),
      path = paths,
      status = status
    )
    dirs$status[i] <- if (!label %in% names(baseline)) {
      "extra_directory"
    } else if (!exists) {
      "missing_directory"
    } else {
      "compared"
    }
    dirs$exact[i] <- dirs$status[i] == "compared" && all(status == "identical")
    for (kind in c("identical", "changed", "missing", "extra")) {
      dirs[[kind]][i] <- sum(status == kind)
    }
  }
  list(
    exact = nrow(dirs) > 0L && all(dirs$exact),
    directories = dirs,
    files = do.call(rbind, details)
  )
}
