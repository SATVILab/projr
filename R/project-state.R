# Older checkouts can be inspected without moving their tracked history.
.manifest_project_dir <- function() {
  root <- .path_get()
  current <- file.path(root, ".projr", "manifests")
  old <- file.path(root, "_projr", "manifests")
  if (!dir.exists(current) && file.exists(file.path(old, "FORMAT"))) {
    old
  } else {
    current
  }
}

# A build or restore can adopt files owned by previous versions of projr.
.project_state_upgrade <- function(root) {
  old <- file.path(root, "_projr")
  old_log <- file.path(root, "BUILDLOG.md")
  has_log <- utils::file_test("-f", old_log) || fs::link_exists(old_log)
  if (!dir.exists(old) && !has_log) {
    return(invisible(FALSE))
  }

  configs <- list.files(
    file.path(old, "config"),
    pattern = "^_projr(-[[:alnum:]_-]+)?\\.yml$"
  )
  configs <- configs[utils::file_test("-f", file.path(old, "config", configs))]
  paths <- if (length(configs)) file.path("config", configs)
  if (file.exists(file.path(old, "manifests", "FORMAT"))) {
    paths <- c(paths, "manifests")
  }
  sources <- file.path(old, paths)
  old_dirs <- if (length(paths)) c(file.path(old, "config"), old)
  if (has_log) {
    sources <- c(sources, old_log)
    paths <- c(paths, "BUILDLOG.md")
  }
  if (!length(paths)) {
    return(invisible(FALSE))
  }
  targets <- file.path(root, ".projr", paths)
  links <- c(
    if (length(old_dirs)) old,
    file.path(root, ".projr"),
    dirname(sources),
    dirname(targets),
    sources,
    targets
  )
  if (any(fs::link_exists(links))) {
    stop(
      "Project bookkeeping includes a symbolic link. Move the linked state to .projr/ explicitly.",
      call. = FALSE
    )
  }
  conflict <- which(file.exists(sources) & file.exists(targets))
  if (length(conflict)) {
    i <- conflict[1L]
    stop(
      "Both ",
      sources[i],
      " and ",
      targets[i],
      " exist. Reconcile them before building or restoring.",
      call. = FALSE
    )
  }
  fs::dir_create(unique(dirname(targets)))
  for (i in seq_along(paths)) {
    if (
      !file.rename(sources[i], targets[i]) &&
        (file.exists(sources[i]) || !file.exists(targets[i]))
    ) {
      stop(
        "Could not move ",
        sources[i],
        " to ",
        targets[i],
        ".",
        call. = FALSE
      )
    }
  }
  for (directory in old_dirs) {
    if (
      dir.exists(directory) &&
        !fs::path_has_parent(getwd(), directory) &&
        !length(list.files(directory, all.files = TRUE, no.. = TRUE))
    ) {
      suppressWarnings(file.remove(directory))
    }
  }
  invisible(TRUE)
}
