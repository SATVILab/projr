# Resolve includes in dependency order so each included output is complete.
.yml_dir_include_plan <- function(directories, profile, legacy = TRUE) {
  has_include <- vapply(directories, function(x) "include" %in% names(x), logical(1))
  if (!any(has_include)) {
    return(list())
  }

  labels <- names(directories)
  outputs <- labels[.yml_dir_label_class_detect_output(labels)]
  eligible <- c(outputs, labels[.yml_dir_label_class_detect_raw(labels) |
    .yml_dir_label_class_detect_docs(labels)])
  plan <- list()
  for (target in labels[has_include]) {
    sources <- directories[[target]][["include"]]
    if (!target %in% outputs) {
      stop("Only output directories can specify include: ", target, call. = FALSE)
    }
    if (identical(sources, list()) || identical(sources, character())) next
    if (!is.character(sources) || anyNA(sources) || !is.null(names(sources)) ||
      any(!sources %in% eligible)) {
      stop("Include for ", target, " must list declared raw, docs or output labels.", call. = FALSE)
    }
    plan[[target]] <- unique(sources)
  }
  if (!length(plan)) {
    return(list())
  }

  # Existing source-side requests participate in the same ordering when enabled.
  if (legacy) {
    for (source in labels) {
      output <- directories[[source]][["output"]]
      if (is.null(output)) next
      targets <- .yml_dir_complete_output(output, source, profile)
      for (target in targets) plan[[target]] <- union(plan[[target]], source)
    }
  }

  for (target in names(plan)) {
    sources <- plan[[target]]
    if (any(grepl("[/\\\\]", c(target, sources))) || any(sources %in% c(".", "..")) ||
      anyDuplicated(tolower(sources))) {
      stop("Include labels must be distinct, single directory names in ", target, ".", call. = FALSE)
    }
  }

  ordered <- list()
  while (length(plan)) {
    ready <- !vapply(plan, function(sources) any(sources %in% names(plan)), logical(1))
    if (!any(ready)) {
      stop("Cycle in output directory includes involving ", paste(names(plan), collapse = ", "), ".", call. = FALSE)
    }
    ordered <- c(ordered, plan[ready])
    plan <- plan[!ready]
  }
  ordered
}

# Resolve existing symlink ancestors even when the configured directory is new.
.build_include_real_path <- function(path) {
  if (file.exists(path)) {
    return(as.character(fs::path_real(path)))
  }
  if (fs::link_exists(path)) stop("Broken link in directory include: ", path, call. = FALSE)
  file.path(.build_include_real_path(dirname(path)), basename(path))
}

.build_include_check_paths <- function(directories, roots) {
  for (target in names(directories)) {
    sources <- directories[[target]][["include"]]
    if (!length(sources)) next
    receiver <- roots[[target]]
    if (is.null(receiver)) next
    others <- unlist(roots[setdiff(names(roots), target)], use.names = FALSE)
    for (source in sources) {
      from <- roots[[source]]
      if (is.null(from)) next
      dest <- file.path(receiver, source)
      if (file.exists(from) && !dir.exists(from)) {
        stop("Included path is not a directory: ", from, call. = FALSE)
      }
      if (fs::link_exists(dest) || (file.exists(dest) && !dir.exists(dest))) {
        stop("A file or link conflicts with directory include: ", dest, call. = FALSE)
      }
      # Copying an ancestor into itself would recurse. Clearing an output must
      # never erase a configured input, or a directory aliased by the include.
      if (fs::path_has_parent(dest, from) || any(fs::path_has_parent(others, dest)) ||
        any(others == receiver)) {
        stop("Overlapping directory paths for ", target, " including ", source, ".", call. = FALSE)
      }
    }
    inputs <- unlist(roots[.yml_dir_label_class_detect_raw(names(roots))], use.names = FALSE)
    if (length(inputs) && any(fs::path_has_parent(inputs, receiver))) {
      stop("Output directory ", target, " contains a raw directory.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.build_include_check_config <- function() {
  directories <- .yml_get(NULL)[["directories"]]
  if (!any(lengths(lapply(directories, `[[`, "include")))) {
    return(invisible(TRUE))
  }
  roots <- lapply(directories, function(directory) {
    path <- directory[["path"]]
    if (!is.null(path)) .build_include_real_path(.path_resolve_root(.template_expand(path)))
  })
  .build_include_check_paths(directories, roots)
}

.build_copy_includes <- function(plan, directories, output_run) {
  labels <- unique(c(names(plan), unlist(plan, use.names = FALSE)))
  roots <- lapply(stats::setNames(labels, labels), function(label) {
    path <- projr_path_get_dir(label, safe = !output_run, format = "absolute", create = FALSE)
    .build_include_real_path(path)
  })
  .build_include_check_paths(directories, roots)
  for (target in names(plan)) {
    for (source in plan[[target]]) {
      from <- roots[[source]]
      dest <- file.path(roots[[target]], source)
      .cli_step("Including directory {source} in {target}")
      if (!source %in% directories[[target]][["include"]]) {
        fs::dir_create(c(from, dest))
        .dir_copy_exact(from, dest, dir_exc = .build_label_get_dir_exc(source))
        next
      }
      # Finish reading the source before removing the previous included copy.
      fs::dir_create(roots[[target]])
      tmp <- tempfile(".include-", tmpdir = roots[[target]])
      tryCatch(
        {
          if (dir.exists(from)) fs::dir_copy(from, tmp) else fs::dir_create(tmp)
          if (dir.exists(dest)) fs::dir_delete(dest)
          fs::file_move(tmp, dest)
        },
        finally = if (dir.exists(tmp)) fs::dir_delete(tmp)
      )
    }
  }
  invisible(TRUE)
}
