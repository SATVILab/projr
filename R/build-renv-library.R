# Adding a package DESCRIPTION can move renv's default library outside the
# checkout. Keep the parent and child R processes on the same project library.
.build_renv_library_start <- function() {
  noop <- function() invisible(NULL)
  project <- as.character(.path_get())
  active <- renv::project()
  if (is.null(active) && !file.exists(file.path(project, "renv", "activate.R")) &&
      !file.exists(file.path(project, "renv.lock"))) return(noop)
  if (!is.null(active) && fs::path_real(active) != fs::path_real(project)) return(noop)
  if (!file.exists(file.path(project, "DESCRIPTION")) ||
      any(nzchar(Sys.getenv(c("RENV_PATHS_LIBRARY", "RENV_PATHS_LIBRARY_ROOT"))))) return(noop)

  analysis_root <- file.path(fs::path_abs(Sys.getenv("RENV_PATHS_RENV", "renv"), start = project), "library")
  if (!dir.exists(analysis_root) || !fs::path_has_parent(analysis_root, project)) return(noop)
  canonical <- renv::paths$library(project = project)
  previous <- xfun::set_envvar(c(RENV_PATHS_LIBRARY = analysis_root))
  analysis <- tryCatch(renv::paths$library(project = project),
                       finally = xfun::set_envvar(previous))
  if (identical(canonical, analysis) || !dir.exists(analysis)) return(noop)
  populated <- function(library) {
    entries <- setdiff(list.files(library), "renv")
    length(entries) > 0L && any(file.exists(file.path(library, entries, "DESCRIPTION")))
  }
  if (!populated(analysis)) return(noop)
  use_analysis <- !populated(canonical)
  chosen <- if (use_analysis) analysis else canonical
  libs <- .libPaths()
  # In an unactivated caller, global site packages can be absent from the
  # renderer's renv library. Honor renv's explicit external and user libraries.
  others <- if (is.null(active)) {
    c(renv::settings$external.libraries(project = project),
      renv::config$external.libraries(project),
      if (renv::config$user.library()) {
        strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1L]]
      })
  } else {
    setdiff(libs, c(canonical, analysis))
  }
  paths <- unique(c(chosen, others, .Library))
  env <- xfun::set_envvar(c(
    RENV_PATHS_LIBRARY = if (use_analysis) analysis_root else NA_character_,
    R_LIBS_USER = paste(paths, collapse = .Platform$path.sep)
  ))
  .libPaths(paths, include.site = FALSE)
  function() {
    xfun::set_envvar(env)
    .libPaths(libs, include.site = FALSE)
    invisible(NULL)
  }
}

.build_renv_missing_renderers <- function(engines, output_run = TRUE, inspect = TRUE) {
  quarto <- engines[names(engines) %in% c("quarto_project", "quarto_document")]
  readme <- .build_readme_rmd_render_check(output_run)
  rmd <- readme || any(names(engines) %in% c("rmd", "bookdown"))
  if (!length(quarto) && !rmd) return(character())
  project <- as.character(.path_get())
  active <- renv::project()
  if (is.null(active) && !file.exists(file.path(project, "renv", "activate.R")) &&
      !file.exists(file.path(project, "renv.lock"))) return(character())
  if (!is.null(active) && fs::path_real(active) != fs::path_real(project)) return(character())

  required <- c(if (length(quarto)) "quarto", if ("bookdown" %in% names(engines)) "bookdown",
                if (readme && .build_readme_rmd_render_detect_pkg_use()) "pkgbuild")
  missing <- function(packages) {
    setdiff(packages, basename(find.package(packages, lib.loc = .libPaths(), quiet = TRUE)))
  }
  absent <- missing(required)
  runtime <- missing(c("rmarkdown", "knitr"))
  if (length(runtime) && !rmd && length(quarto) && !"quarto" %in% absent) {
    # Let Quarto identify the engine, including project metadata and markdown-
    # only or Jupyter documents that can render without an R runtime.
    if (!requireNamespace("quarto", quietly = TRUE)) {
      # Some versions of the R wrapper import rmarkdown themselves.
      absent <- c(absent, runtime)
    } else {
      inputs <- as.character(fs::path_abs(unique(as.character(unlist(quarto, use.names = FALSE))), start = project))
      full_project <- "quarto_project" %in% names(quarto) && is.null(quarto[["quarto_project"]])
      if (inspect) {
        # Inspect an explicit member in its native project so its metadata is
        # inherited and excluded chapters cannot make R a requirement.
        if (full_project) {
          excluded <- attr(engines, "excluded_documents", exact = TRUE)
          members <- if (length(excluded)) {
            metadata <- quarto::quarto_inspect(project, quiet = TRUE)
            setdiff(as.character(metadata$files$input), excluded)
          } else project
          inputs <- c(inputs, members)
        }
        rmd <- any(vapply(inputs, function(input) {
          "knitr" %in% quarto::quarto_inspect(input, quiet = TRUE)$engines
        }, logical(1)))
      } else {
        # The public package query must leave Quarto's project cache alone.
        if (full_project) {
          config <- .yml_quarto_get()
          render <- unlist(config$project$render %||% config$book$chapters, use.names = FALSE)
          inputs <- if (length(render)) {
            setdiff(.scripts_expand_patterns(render[!startsWith(render, "!")]),
                    .scripts_get_exc(render))
          } else {
            c(list.files(project, pattern = "[.]qmd$", ignore.case = TRUE, full.names = TRUE),
              .build_document_sources(quarto))
          }
          rmd <- identical(config$engine, "knitr") || !is.null(config$knitr)
        }
        inputs <- setdiff(inputs[file.exists(inputs)], attr(engines, "excluded_documents"))
        if (!rmd && length(inputs)) {
          rmd <- "rmarkdown" %in% renv::dependencies(inputs, root = project, progress = FALSE,
                                                    errors = "ignored")$Package
        }
      }
    }
  }
  if (rmd) absent <- unique(c(absent, runtime))
  absent
}

.build_renv_render_status <- function(missing) {
  project <- as.character(.path_get())
  library <- renv::paths$library(project = project)
  pkgs <- paste(encodeString(missing, quote = '"'), collapse = ", ")
  install <- paste0("renv::install(c(", pkgs, "), project = ", encodeString(project, quote = '"'),
                    ", library = ", encodeString(library, quote = '"'), ")")
  list(available = FALSE, missing = missing, install_cmds = install,
       message = paste0("Missing renv render packages: ", paste(missing, collapse = ", "),
                        ".\nProject: ", project, "\nLibrary: ", library,
                        "\nInstall them before building with ", install))
}
