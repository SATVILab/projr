# Helper functions - getting paths
# ===========================================================================

.projr_dir_get <- function(label, ..., safe) {
  switch(label,
    # useful for uploading source code to github,
    # as code is the label but we don't actually
    # want to upload anything in the code directory
    "code" = .projr_dir_get_code(),
    .projr_dir_get_label(label, safe)
  ) |>
    .path_get_full(...)
}

.projr_dir_get_code <- function() {
  dir_out <- .projr_dir_get_tmp_random("code")
  .dir_rm(dir_out)
  dir_out
}

.projr_dir_get_tmp_random <- function(...) {
  file.path(tempdir(), "projr", signif(stats::rnorm(1))) |>
    .path_get_full(...)
}

.projr_dir_get_label <- function(label, safe) {
  switch(as.character(safe),
    "TRUE" = .projr_dir_get_label_safe(label),
    "FALSE" = .projr_dir_get_label_unsafe(label)
  )
}

.projr_dir_get_label_safe <- function(label) {
  # ouput, data and docs directories
  # need to be "safe", so
  # if it's not one of them use the unsafe function
  if (!.projr_dir_get_label_safe_check_unsafe(label)) {
    return(.projr_dir_get_label_unsafe(label))
  }

  path_dir <- .projr_dir_get_label_safe_path(label)

  # set _bookdown.yml and _quarto.yml
  .projr_dir_set_docs_safe(path_dir, label)

  path_dir
}

.projr_dir_get_label_safe_check_unsafe <- function(label) {
  .projr_dir_label_strip(label) |>
    grepl("^output|^docs$|^data$", x = _)
}

.projr_dir_get_label_safe_path <- function(label) {
  .projr_dir_get_cache_auto_version(profile = NULL) |>
    file.path(
      .projr_dir_get_label_safe_path_get_label(label)
    )
}

.projr_dir_get_label_safe_path_get_label <- function(label) {
  switch(label,
    "docs" = .projr_dir_get_docs_unsafe(),
    label
  )
}

.projr_dir_get_label_unsafe <- function(label) {
  switch(.projr_dir_label_strip(label),
    "docs" = .projr_dir_get_docs_unsafe(),
    "project" = ".",
    "data" = "data",
    .projr_yml_dir_get_path(label, NULL)
  )
}

# docs
.projr_dir_get_docs_unsafe <- function() {
  # get the path
  .projr_dir_get_docs_unsafe_path() |>
    .projr_dir_set_docs_unsafe_path()
}

.projr_dir_get_docs_unsafe_path <- function() {
  switch(.projr_engine_get(),
    "quarto_project" = .projr_dir_get_docs_quarto_project(),
    "quarto_document" = .projr_dir_get_docs_md(),
    "bookdown" = .projr_dir_get_docs_bookdown(),
    "rmd" = .projr_dir_get_docs_md()
  )
}

.projr_dir_set_docs_unsafe_path <- function(path) {
  # ensure it's set in _projr.yml, and quarto.yml/_bookdown.yml if need be
  switch(.projr_engine_get(),
    "quarto_project" = .projr_dir_set_docs_quarto_project(path),
    "quarto_document" = .projr_yml_dir_set_docs(path, NULL),
    "bookdown" = .projr_dir_set_docs_bookdown(path),
    "rmd" = .projr_yml_dir_set_docs(path)
  )
  invisible(path)
}

.projr_dir_set_docs_safe <- function(path, label) {
  # don't do anything for quarto and rmd projects,
  # as we only manipulate the _quarto.yml and _bookdown.yml
  # here (_projr.yml manipulated only for unsafe ones)
  if (!.projr_dir_set_docs_safe_check(label)) {
    return(invisible(FALSE))
  }
  switch(.projr_engine_get(),
    "quarto_project" =
      .projr_dir_set_docs_quarto_project(path),
    "bookdown" = .projr_dir_set_docs_bookdown(path)
  )
  invisible(TRUE)
}

.projr_dir_set_docs_safe_check <- function(label) {
  .projr_dir_label_strip(label) |>
    grepl("^docs$", x = _)
}


# quarto
.projr_dir_get_docs_quarto_project <- function() {
  # use docs$path if it is set
  path <- .projr_yml_dir_get_path("docs", NULL)
  if (!is.null(path)) {
    return(path)
  }
  .projr_dir_get_docs_quarto_project_unset()
}

.projr_dir_get_docs_quarto_project_unset <- function() {
  # use `_quarto.yml` if specified, otherwise defaults
  switch(as.character(is.null(.projr_yml_quarto_get_output_dir())),
    "FALSE" = .projr_yml_quarto_get_output_dir(),
    "TRUE" = .projr_dir_get_docs_quarto_project_unset_default()
  )
}

.projr_dir_get_docs_quarto_project_unset_default <- function() {
  switch(.projr_yml_quarto_get_project_type(),
    "book" = "_book",
    "website" = ,
    "site" = "_site",
    stop("Quarto project type not recognised.")
  )
}

.projr_dir_set_docs_quarto_project <- function(path) {
  .projr_yml_quarto_set_output_dir(path)
  .projr_yml_dir_set_docs(path, NULL)
  return(invisible(TRUE))
}

# bookdown
.projr_dir_get_docs_bookdown <- function() {
  # use what's in `_projr.yml` if specified
  path <- .projr_yml_dir_get_path("docs", NULL)
  if (!is.null(path)) {
    return(path)
  }
  # use what's in `_bookdown.yml` if specified
  path <- .projr_yml_bd_get_output_dir()
  if (!is.null(path)) {
    return(path)
  }
  # use default if nothing pre-specified
  "_book"
}

.projr_dir_set_docs_bookdown <- function(path) {
  .projr_yml_bd_set_output_dir(path)
  .projr_yml_dir_set_docs(path, NULL)
}

# Rmd/qmd (no other yml file of concern)
.projr_dir_get_docs_md <- function() {
  yml_projr <- .projr_yml_get(NULL)
  dir_docs_yml <- .projr_yml_dir_get_path("docs", NULL)
  if (!is.null(dir_docs_yml)) {
    return(dir_docs_yml)
  }
  "docs"
}

# get cache directory to save to
.projr_dir_get_cache_auto <- function(..., create = FALSE, profile) {
  .projr_dir_get_cache_auto_check(profile = profile)
  .projr_dir_get_cache_auto_path(profile) |>
    .path_get_full(...) |>
    .projr_dir_get_create(create)
}

.projr_path_get_cache_auto <- function(..., create = FALSE, profile) {
  .projr_dir_get_cache_auto_check(profile = profile)
  path_dir <- .projr_dir_get_cache_auto_path(profile) |>
    .path_get_full(...)
  .projr_dir_get_create(dirname(path_dir), create)
  path_dir
}

.projr_dir_get_cache_auto_version <- function(..., create = FALSE, profile) {
  .projr_dir_get_cache_auto_check(profile = profile)
  .projr_dir_get_cache_auto_path(profile) |>
    file.path("projr") |>
    .projr_version_append() |>
    .path_get_full(...) |>
    .projr_dir_get_create(create)
}

.projr_dir_get_cache_auto_path <- function(profile) {
  .projr_yml_dir_get(profile)[[
    .projr_dir_get_cache_auto_ind(profile)
  ]][["path"]]
}

.projr_dir_get_cache_auto_check <- function(profile) {
  # find cache directory to save to
  stopifnot(
    "No cache directory specified, but required for projr builds." =
      length(.projr_dir_get_cache_auto_ind(profile)) > 0
  )
}

.projr_dir_get_cache_auto_ind <- function(profile) {
  which(
    grepl("^cache", .projr_dir_label_strip(names(.projr_yml_dir_get(profile))))
  )[1]
}

.projr_dir_get_cache_auto_version_old <- function(..., create = TRUE, profile) {
  .projr_dir_get_cache_auto_check(profile = profile)
  .projr_dir_get_cache_auto_path() |>
    file.path("projr") |>
    .projr_version_append() |>
    file.path("old", ...) |>
    .projr_dir_get_create(create)
}
