.engines_get <- function(scripts = NULL) {
  if (identical(scripts, character(0))) {
    return(list())
  }

  project_configs <- c(quarto_project = "_quarto.yml", bookdown = "_bookdown.yml")
  project <- names(project_configs)[file.exists(.path_get(unname(project_configs)))]
  if (length(project) > 1L) {
    stop("Both Quarto and Bookdown projects are present.", call. = FALSE)
  }

  automatic <- is.null(scripts) || all(startsWith(scripts, "!"))
  automatic_project <- automatic && length(project) > 0L
  if (automatic) {
    excluded <- if (length(scripts)) .scripts_get_exc(scripts) else character()
    scripts <- if (automatic_project) .scripts_get_inc_all_r() else .scripts_get_inc_all()
    scripts <- setdiff(scripts[!dir.exists(scripts)], excluded)
  }
  scripts <- scripts[!scripts %in% c("README.Rmd", as.character(.path_get("README.Rmd")))]

  engine_by_extension <- c(qmd = "quarto_document", rmd = "rmd", r = "r")
  extensions <- tolower(tools::file_ext(scripts))
  engines <- unname(engine_by_extension[extensions])
  if (anyNA(engines)) {
    stop("Unsupported build script: ", scripts[is.na(engines)][[1L]], call. = FALSE)
  }
  steps <- stats::setNames(as.list(scripts), engines)

  if (automatic_project) {
    steps <- c(stats::setNames(list(NULL), project), steps)
    types <- if (project == "bookdown") c("rmd", "md") else c("qmd", "rmd", "md", "ipynb")
    documents <- excluded[tolower(tools::file_ext(excluded)) %in% types]
    if (length(documents)) attr(steps, "excluded_documents") <- documents
    return(steps)
  }
  if (length(project) > 0L) {
    extension <- if (project == "quarto_project") "qmd" else "rmd"
    chapters <- which(extensions == extension)
    if (length(chapters) > 0L) {
      project_scripts <- as.character(fs::path_rel(scripts[chapters], start = .path_get()))
      repeated <- unique(project_scripts[duplicated(project_scripts)])
      if (length(repeated)) {
        renderer <- if (project == "quarto_project") "Quarto" else "Bookdown"
        stop(renderer, " projects do not support repeated renders of individual documents: ",
          paste(repeated, collapse = ", "),
          call. = FALSE
        )
      }
      if (any(startsWith(project_scripts, "../"))) {
        stop("Project documents must be inside the project directory.", call. = FALSE)
      }
      # Render the project once, where its last selected document appeared.
      last_chapter <- utils::tail(chapters, 1L)
      steps[[last_chapter]] <- project_scripts
      names(steps)[last_chapter] <- project
      steps <- steps[setdiff(seq_along(steps), utils::head(chapters, -1L))]
    }
  }
  steps
}
