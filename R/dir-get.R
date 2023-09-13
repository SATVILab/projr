# Helper functions - getting paths
# ===========================================================================

.projr_dir_get <- function(label, ..., output_safe) {
  dir_label <- .projr_dir_get_label(label, output_safe)
  .projr_dir_get_full(dir_label, ...)
}

.projr_dir_get_full <- function(dir_label, ...) {
  path_append <- list(...) |> unlist()
  do.call(
    "file.path",
    args = list(dir_label) |> append(path_append)
  ) |>
    as.character()
}

.projr_dir_get_label <- function(label, output_safe) {
  if (missing(label)) stop("label must be specified")
  if (length(label) != 1L) stop("label must be length 1")
  label_strip <- .projr_dir_label_strip(label)
  label_vec_valid <- c(
    .projr_dir_label_strip(names(projr_yml_get()[["directories"]])),
    "docs", "project"
  )
  label_recognised <- label_strip %in% label_vec_valid
  if (!label_recognised) {
    stop(paste0("label `", label, "` not recognised."))
  }
  switch(as.character(output_safe),
    "TRUE" = .projr_dir_get_label_safe(label),
    "FALSE" = .projr_dir_get_label_unsafe(label)
  )
}

.projr_dir_get_label_safe <- function(label) {
  # only output, archive and docs directories
  # need to be "safe"
  label_strip <- .projr_dir_label_strip(label)
  use_cache_subdir <- grepl("^output|^archive|^docs$", label_strip)
  if (!use_cache_subdir) {
    return(.projr_dir_get_label_unsafe(label))
  }

  dir_cache_safe <- .projr_dir_get_cache_auto()

  # get final directory
  dir_label_safe <- file.path(
    dir_cache_safe,
    "projr",
    paste0("v", projr_version_get()),
    switch(label,
      "docs" = .projr_dir_get_docs_unsafe(),
      label
    )
  )

  # set _bookdown.yml and _quarto.yml
  if (label_strip == "docs") {
    .projr_dir_set_docs_safe(dir_label_safe)
  }

  # no need to append version if archiving, as already done

  dir_label_safe
}

.projr_dir_get_label_unsafe <- function(label) {
  dir_active <- projr_yml_get_unchecked()[["directories"]]
  label_strip <- .projr_dir_label_strip(label)
  dir_label_unsafe <- switch(label_strip,
    "docs" = .projr_dir_get_docs_unsafe(),
    "project" = ".",
    dir_active[[label]][["path"]]
  )
  # append version if archiving
  if (grepl("^archive", label_strip)) {
    dir_label_unsafe <- file.path(
      dir_label_unsafe, paste0("v", projr_version_get())
    )
  }
  dir_label_unsafe
}

# create
.projr_dir_create <- function(path_dir) {
  if (dir.exists(path_dir)) {
    return(invisible())
  }
  dir.create(path_dir, recursive = TRUE)
}

# docs
.projr_dir_get_docs_unsafe <- function() {
  # get the path
  engine <- .projr_engine_get()
  path <- switch(engine,
    "quarto_project" = .projr_dir_get_docs_quarto_project(),
    "quarto_document" = .projr_dir_get_docs_md(),
    "bookdown" = .projr_dir_get_docs_bookdown(),
    "rmd" = .projr_dir_get_docs_md()
  )
  # ensure it's set in _projr.yml, and quarto.yml/_bookdown.yml if need be
  switch(engine,
    "quarto_project" = .projr_dir_set_docs_quarto_project(path),
    "quarto_document" = .projr_yml_dir_set_docs(path),
    "bookdown" = .projr_dir_set_docs_bookdown(path),
    "rmd" = .projr_yml_dir_set_docs(path)
  )
  path
}

.projr_dir_set_docs_safe <- function(path) {
  # don't do anything for quarto and rmd projects,
  # as we only manipulate the _quarto.yml and _bookdown.yml
  # here (_projr.yml manipulated only for unsafe ones)
  switch(.projr_engine_get(),
    "quarto_project" = .projr_dir_set_docs_quarto_project_safe(path),
    "bookdown" = .projr_dir_set_docs_bookdown(path)
  )
  invisible(TRUE)
}

# all
.projr_yml_dir_set_docs <- function(path) {
  yml_projr <- projr_yml_get_unchecked()
  within_cache <- fs::path_has_parent(
    path,
    projr::projr_dir_get("cache", "projr", paste0("v", projr_version_get()))
  )
  if (within_cache) {
    path <- fs::path_rel(
      path,
      projr::projr_dir_get("cache", "projr", paste0("v", projr_version_get()))
    )
  }

  yml_projr[["directories"]][["docs"]][["path"]] <- path
  .projr_yml_set(yml_projr)
}

# quarto
.projr_dir_get_docs_quarto_project <- function() {
  # use docs$path if it is set
  yml_projr <- projr_yml_get_unchecked()
  path <- yml_projr[["directories"]][["docs"]][["path"]]
  if (!is.null(path)) {
    return(path)
  }

  # use `_quarto.yml` if specified, otherwise defaults
  yml_quarto <- .projr_yml_quarto_get()
  switch(as.character(is.null(yml_quarto[["output-dir"]])),
    "FALSE" = yml_quarto[["output-dir"]],
    "TRUE" = {
      switch(yml_quarto[["project"]][["type"]],
        "book" = "_book",
        "site" = "_site",
        stop("Quarto project type not recognised.")
      )
    }
  )
}

.projr_dir_set_docs_quarto_project <- function(path) {
  .projr_dir_set_docs_quarto_project_safe(path)
  .projr_yml_dir_set_docs(path)
  return(invisible(TRUE))
}

.projr_dir_set_docs_quarto_project_safe <- function(path) {
  yml_quarto <- .projr_yml_quarto_get()
  yml_quarto[["project"]][["output-dir"]] <- path
  .projr_yml_quarto_set(yml_quarto)
}

# bookdown
.projr_dir_get_docs_bookdown <- function() {
  yml_projr <- projr_yml_get_unchecked()
  # use what's in `_projr.yml` if specified
  path <- yml_projr[["directories"]][["docs"]][["path"]]
  if (!is.null(path)) {
    return(path)
  }
  # use what's in `_bookdown.yml` if specified
  yml_bd <- .projr_yml_bd_get()
  path <- yml_bd[["output_dir"]]
  if (!is.null(path)) {
    return(path)
  }
  # use default if nothing pre-specified
  "_book"
}

.projr_dir_set_docs_bookdown <- function(path) {
  .projr_dir_set_docs_bookdown_safe(path)
  .projr_yml_dir_set_docs(path)
}

.projr_dir_set_docs_bookdown_safe <- function(path) {
  yml_bd <- .projr_yml_bd_get()
  yml_bd[["project"]][["output_dir"]] <- path
  .projr_yml_bd_set(yml_bd)
}

# Rmd/qmd (no other yml file of concern)
.projr_dir_get_docs_md <- function() {
  yml_projr <- projr_yml_get_unchecked()
  dir_docs_yml <- yml_projr[["directories"]][["docs"]][["path"]]
  if (!is.null(dir_docs_yml)) {
    return(dir_docs_yml)
  }
  "docs"
}

# get cache directory to save to
.projr_dir_get_cache_auto <- function() {
  # find cache directory to save to
  dir_active <- projr_yml_get_unchecked()[["directories"]]
  dir_active_vec_names_strip <- .projr_dir_label_strip(names(dir_active))
  cache_ind <- which(dir_active_vec_names_strip == "cache")[1]
  if (length(cache_ind) == 0) {
    cache_ind <- which(grepl("^cache", dir_active_vec_names_strip))[1]
  }
  stopifnot(
    "No cache directory specified, but required for projr builds." =
      length(cache_ind) > 0
  )
  dir_active[[cache_ind]]$path
}
