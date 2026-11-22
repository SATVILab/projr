.build_engines <- function(engines, version_run_on_list, args_engine) {
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
      engines[[index]] <- outputs
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

.build_engines_override_project <- function(engines) {
  project <- intersect(c("quarto_project", "bookdown"), names(engines))
  if (length(project) == 0L || is.null(engines[[project]])) {
    return(function() invisible(NULL))
  }

  scripts <- as.list(engines[[project]])
  config_file <- if (project == "quarto_project") "_quarto.yml" else "_bookdown.yml"
  path <- .path_get(config_file)
  original <- readBin(path, "raw", n = file.size(path))
  restore <- function() {
    if (!is.null(original)) {
      writeBin(original, path)
      original <<- NULL
    }
    invisible(NULL)
  }

  tryCatch(
    {
      if (project == "quarto_project") {
        config <- .yml_quarto_get()
        config[["project"]][["render"]] <- scripts
        if (identical(config[["project"]][["type"]], "book")) {
          config[["book"]][["chapters"]] <- scripts
          config[["book"]][["appendices"]] <- NULL
        }
        .yml_quarto_set(config)
      } else {
        config <- .yml_bd_get()
        config[["rmd_files"]] <- scripts
        config[["rmd_subdir"]] <- FALSE
        .yml_bd_set(config)
      }
    },
    error = function(error) {
      restore()
      stop(error)
    }
  )
  restore
}
