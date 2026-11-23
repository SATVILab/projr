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
  documents <- as.character(unlist(engines[names(engines) != "r"], use.names = FALSE))
  if ("quarto_project" %in% names(engines) && is.null(engines[["quarto_project"]])) {
    root <- .path_get()
    directories <- list.dirs(root, full.names = TRUE, recursive = FALSE)
    directories <- directories[!grepl("^[._]|^renv$", basename(directories))]
    documents <- c(documents, list.files(
      directories,
      pattern = "\\.(qmd|rmd)$", ignore.case = TRUE,
      full.names = TRUE, recursive = TRUE
    ))
  }
  if ("bookdown" %in% names(engines) && is.null(engines[["bookdown"]])) {
    documents <- c(documents, as.character(unlist(.yml_bd_get_nm("rmd_files"), use.names = FALSE)))
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

  original <- readBin(path, "raw", n = file.size(path))
  function() {
    if (!is.null(original)) {
      writeBin(original, path)
      original <<- NULL
    }
    invisible(NULL)
  }
}

.build_engines_override_project <- function(engines) {
  if (any(names(engines) %in% c("quarto_project", "bookdown"))) {
    .dir_set_docs_safe(.dir_get_label_safe_path("docs"), "docs")
  }
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
  invisible(NULL)
}
