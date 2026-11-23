.build_engines <- function(engines, version_run_on_list, args_engine) {
  collected <- list()
  for (index in seq_along(engines)) {
    engine <- names(engines)[[index]]
    standalone <- engine %in% c("quarto_document", "rmd")
    outputs <- .build_engine(
      scripts = engines[[index]],
      version_run_on_list = version_run_on_list,
      args_engine = args_engine,
      engine = engine,
      collect_paths = standalone
    )
    if (standalone) {
      # Re-render each occurrence, then copy a shared output or asset only once.
      script <- engines[[index]]
      engines[[index]] <- setdiff(outputs, collected[[script]])
      collected[[script]] <- union(collected[[script]], outputs)
      names(engines)[[index]] <- "document_output"
    }
  }
  invisible(engines)
}

.build_document_sources <- function(engines) {
  documents <- as.character(unlist(
    engines[names(engines) != "r"],
    use.names = FALSE
  ))
  if (
    "quarto_project" %in% names(engines) && is.null(engines[["quarto_project"]])
  ) {
    root <- .path_get()
    directories <- list.dirs(root, full.names = TRUE, recursive = FALSE)
    directories <- directories[!grepl("^[._]|^renv$", basename(directories))]
    documents <- c(
      documents,
      list.files(
        directories,
        pattern = "\\.(qmd|rmd)$",
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = TRUE
      )
    )
  }
  if ("bookdown" %in% names(engines) && is.null(engines[["bookdown"]])) {
    documents <- c(
      documents,
      as.character(unlist(.yml_bd_get_nm("rmd_files"), use.names = FALSE))
    )
  }
  documents
}

.build_engines_backup_project <- function(engines) {
  if ("quarto_project" %in% names(engines)) {
    path <- .path_get("_quarto.yml")
  } else if ("bookdown" %in% names(engines)) {
    path <- .path_get("_bookdown.yml")
  } else {
    return(function() invisible(NULL))
  }

  originals <- list()
  restore <- function(save = NULL) {
    if (is.null(save)) {
      for (path in names(originals)) {
        writeBin(originals[[path]], path)
      }
      originals <<- list()
    } else if (is.null(originals[[save]])) {
      originals[[save]] <<- readBin(save, "raw", n = file.size(save))
    }
    invisible(NULL)
  }
  restore(save = as.character(fs::path_real(path)))
  restore
}

.build_engines_override_project <- function(
  engines,
  restore = .build_engines_backup_project(engines),
  quarto_profile = NULL
) {
  project <- intersect(names(engines), c("quarto_project", "bookdown"))
  if (!length(project)) {
    return(engines)
  }

  excluded <- attr(engines, "excluded_documents", exact = TRUE)
  if (length(excluded)) {
    excluded <- as.character(fs::path_rel(excluded, start = .path_get()))
    render <- if (project == "quarto_project") {
      .build_quarto_exclude(excluded, restore, quarto_profile)
    } else {
      .build_bookdown_exclude(excluded)
    }
    if (!render) {
      return(engines[names(engines) != project])
    }
  }
  .dir_set_docs_safe(.dir_get_label_safe_path("docs"), "docs")
  if (!is.null(engines[["quarto_project"]])) {
    scripts <- as.list(engines[["quarto_project"]])
    config <- .yml_quarto_get()
    config[["project"]][["render"]] <- scripts
    if (identical(config[["project"]][["type"]], "book")) {
      config[["book"]][["chapters"]] <- scripts
      config[["book"]][["appendices"]] <- NULL
    }
    .yml_quarto_set(config)
  } else if (!is.null(engines[["bookdown"]])) {
    config <- .yml_bd_get()
    config[["rmd_files"]] <- as.list(engines[["bookdown"]])
    config[["rmd_subdir"]] <- FALSE
    .yml_bd_set(config)
  }
  invisible(engines)
}

.build_quarto_exclude <- function(excluded, restore, profile) {
  # Ask Quarto which files the project would render, including nested inputs and globs.
  root <- .path_get()
  .dep_install("quarto")
  inspection <- .build_user_call(
    function(root, profile) {
      quarto::quarto_inspect(root, profile = profile)
    },
    list(root, profile)
  )
  inputs <- inspection$files$input
  inputs <- as.character(fs::path_rel(
    as.character(unlist(inputs, use.names = FALSE)),
    start = root
  ))
  if (!any(inputs %in% excluded)) {
    return(TRUE)
  }
  inputs <- setdiff(inputs, excluded)
  if (!length(inputs)) {
    return(FALSE)
  }

  book <- identical(inspection$config$project$type, "book")
  if (book && !any(startsWith(inputs, "index."))) {
    stop(
      "Quarto books require an index page when other chapters are selected.",
      call. = FALSE
    )
  }
  main <- as.character(fs::path_real(.path_get("_quarto.yml")))
  # Quarto merges render lists and book chapters across the active YAML files.
  # An excluded chapter in an include or profile can override the root selection.
  files <- inspection$files$config
  for (path in unique(as.character(fs::path_real(files[file.exists(files)])))) {
    original <- yaml::read_yaml(path, handlers = list(seq = identity))
    config <- original
    if (path == main) {
      config$project$render <- as.list(inputs)
    } else if (length(config$project$render)) {
      patterns <- unlist(config$project$render, use.names = FALSE)
      if (
        !all(startsWith(patterns, "!") | fs::path_norm(patterns) %in% inputs)
      ) {
        config$project$render <- NULL
      }
    }
    if (book) {
      for (field in c("chapters", "appendices")) {
        if (!is.null(config$book[[field]])) {
          config$book[[field]] <- .build_quarto_filter_chapters(
            config$book[[field]],
            excluded
          )
        }
      }
      references <- config$book$references
      if (
        !is.null(references) &&
          !length(.build_quarto_filter_chapters(list(references), excluded))
      ) {
        config$book$references <- NULL
      }
    }
    if (!identical(config, original)) {
      if (!fs::path_has_parent(path, fs::path_real(root))) {
        stop(
          "Excluding project documents would change Quarto configuration outside the checkout: ",
          path,
          call. = FALSE
        )
      }
      restore(save = path)
      yaml::write_yaml(config, path)
    }
  }
  TRUE
}

.build_quarto_filter_chapters <- function(chapters, excluded) {
  result <- list()
  for (chapter in chapters) {
    if (is.list(chapter)) {
      if (!is.null(chapter$chapters)) {
        chapter$chapters <- .build_quarto_filter_chapters(
          chapter$chapters,
          excluded
        )
        if (!length(chapter$chapters)) {
          # A part with its own file can still render after all its children are excluded.
          if (
            !is.null(chapter$part) &&
              file.exists(chapter$part) &&
              !fs::path_norm(chapter$part) %in% excluded
          ) {
            result <- c(result, list(chapter$part))
          }
          next
        }
      }
      # A part may have its own introductory document. Keep its remaining children.
      if (!is.null(chapter$part) && fs::path_norm(chapter$part) %in% excluded) {
        result <- c(result, chapter$chapters)
        next
      }
      if (!is.null(chapter$href) && fs::path_norm(chapter$href) %in% excluded) {
        next
      }
    } else if (fs::path_norm(chapter) %in% excluded) {
      next
    }
    result <- c(result, list(chapter))
  }
  result
}

.build_bookdown_exclude <- function(excluded) {
  config <- .yml_bd_get()
  pattern <- if (isTRUE(config$include_md)) "[.]R?md$" else "[.]Rmd$"
  chapters <- config$rmd_files %||%
    list.files(pattern = pattern, ignore.case = TRUE)
  subdir <- config$rmd_subdir
  # Bookdown adds discovered subdirectory files even when rmd_files is set.
  extra <- if (isTRUE(subdir) || is.character(subdir)) {
    dirs <- if (isTRUE(subdir)) "." else subdir
    files <- fs::path_norm(list.files(
      dirs,
      pattern,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    ))
    files[dirname(files) != "."]
  }
  groups <- if (is.null(names(chapters))) list(chapters) else chapters
  groups <- lapply(groups, function(group) {
    files <- as.character(fs::path_norm(unique(c(
      unlist(group, use.names = FALSE),
      extra
    ))))
    files <- files[!startsWith(basename(files), "_")]
    setdiff(files, paste0(config$book_filename %||% "_main", c(".md", ".Rmd")))
  })
  if (!any(unlist(groups, use.names = FALSE) %in% excluded)) {
    return(TRUE)
  }
  groups <- lapply(groups, function(group) as.list(setdiff(group, excluded)))
  if (!any(lengths(groups))) {
    return(FALSE)
  }
  config$rmd_files <- if (is.null(names(groups))) groups[[1L]] else groups
  config$rmd_subdir <- FALSE
  .yml_bd_set(config)
  TRUE
}
