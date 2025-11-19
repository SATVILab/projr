# Helper functions - getting paths
# ===========================================================================

.dir_get <- function(label, ..., safe) {
  switch(label,
    # useful for uploading source code to github,
    # as code is the label but we don't actually
    # want to upload anything in the code directory
    "code" = .dir_get_code(),
    .dir_get_label(label, safe)
  ) |>
    .path_get_full(...)
}

.dir_get_code <- function() {
  dir_out <- .dir_get_tmp_random("code")
  .dir_rm(dir_out)
  dir_out
}

.dir_get_tmp_random <- function(...) {
  file.path(tempdir(), "projr", signif(stats::rnorm(1))) |>
    .path_get_full(...)
}

.dir_get_label <- function(label, safe) {
  switch(as.character(safe),
    "TRUE" = .dir_get_label_safe(label),
    "FALSE" = .dir_get_label_unsafe(label)
  ) |>
    .dir_get_label_check_not_root(label = label)
}

.dir_get_label_check_not_root <- function(path, label) {
  if (label %in% c("code", "project", "docs")) {
    return(path)
  }
  path_proj <- .path_get() |>
    fs::path_abs() |>
    as.character()
  if (path_proj == path) {
    stop(
      paste0(
        "The path for label ", label, " is the root directory of the project."
      ),
      call. = FALSE
    )
  }
  path
}

.dir_get_label_safe <- function(label) {
  # ouput, data and docs directories
  # need to be "safe", so
  # if it's not one of them use the unsafe function
  if (!.dir_get_label_safe_check_unsafe(label)) {
    return(.dir_get_label_unsafe(label))
  }

  path_dir <- .dir_get_label_safe_path(label)

  # set _bookdown.yml and _quarto.yml
  .dir_set_docs_safe(path_dir, label)

  path_dir
}

.dir_get_label_safe_check_unsafe <- function(label) {
  .dir_label_strip(label) |>
    (\(x) grepl("^output|^docs$|^data$", x))()
}

.dir_get_label_safe_path <- function(label) {
  .dir_get_cache_auto_version(profile = NULL) |>
    file.path(
      .dir_get_label_safe_path_get_label(label)
    )
}

.dir_get_label_safe_path_get_label <- function(label) {
  switch(label,
    "docs" = .dir_get_docs_unsafe(),
    label
  )
}

.dir_get_label_unsafe <- function(label) {
  switch(.dir_label_strip(label),
    "docs" = .dir_get_docs_unsafe(),
    "project" = ".",
    "data" = "data",
    .yml_dir_get_path(label, NULL)
  )
}

# docs
.dir_get_docs_unsafe <- function() {
  # get the path
  .dir_get_docs_unsafe_path() |>
    .dir_set_docs_unsafe_path()
}

.dir_get_docs_unsafe_path <- function() {
  switch(.engine_get(),
    "quarto_project" = .dir_get_docs_quarto_project(),
    "quarto_document" = .dir_get_docs_md(),
    "bookdown" = .dir_get_docs_bookdown(),
    "rmd" = .dir_get_docs_md(),
    .dir_get_docs_md()
  )
}

.dir_set_docs_unsafe_path <- function(path) {
  # ensure it's set in _projr.yml, and quarto.yml/_bookdown.yml if need be
  if (is.null(.engine_get())) {
    return(invisible(NULL))
  }
  switch(.engine_get(),
    "quarto_project" = .dir_set_docs_quarto_project(path),
    "quarto_document" = .yml_dir_set_docs(path, NULL),
    "bookdown" = .dir_set_docs_bookdown(path),
    "rmd" = .yml_dir_set_docs(path)
  )
  invisible(path)
}

.dir_set_docs_safe <- function(path, label) {
  # don't do anything for quarto and bookdown projects,
  # as we only manipulate the _quarto.yml and _bookdown.yml
  # here (_projr.yml manipulated only for unsafe ones)
  if (!.dir_set_docs_safe_check(label)) {
    return(invisible(FALSE))
  }
  switch(.engine_get(),
    "quarto_project" =
      .dir_set_docs_quarto_project(path),
    "bookdown" = .dir_set_docs_bookdown(path)
  )
  invisible(TRUE)
}

.dir_set_docs_safe_check <- function(label) {
  .dir_label_strip(label) |>
    (\(x) grepl("^docs$", x))()
}


# quarto
.dir_get_docs_quarto_project <- function() {
  # use docs$path if it is set
  path <- .yml_dir_get_path("docs", NULL)
  if (!is.null(path)) {
    return(path)
  }
  .dir_get_docs_quarto_project_unset()
}

.dir_get_docs_quarto_project_unset <- function() {
  # use `_quarto.yml` if specified, otherwise defaults
  switch(as.character(is.null(.yml_quarto_get_output_dir())),
    "FALSE" = .yml_quarto_get_output_dir(),
    "TRUE" = .dir_get_docs_quarto_project_unset_default()
  )
}

.dir_get_docs_quarto_project_unset_default <- function() {
  project_type <- .yml_quarto_get_project_type()

  # Handle case where _quarto.yml exists but has no project type specified
  # Default to "website" behavior (outputs to _site)
  if (is.null(project_type)) {
    return("_site")
  }

  switch(project_type,
    "book" = "_book",
    "website" = ,
    "site" = "_site",
    stop("Quarto project type not recognised.")
  )
}

.dir_set_docs_quarto_project <- function(path) {
  .yml_quarto_set_output_dir(path)
  .yml_dir_set_docs(path, NULL)
  return(invisible(TRUE))
}

# bookdown
.dir_get_docs_bookdown <- function() {
  # use what's in `_projr.yml` if specified
  path <- .yml_dir_get_path("docs", NULL)
  if (!is.null(path)) {
    return(path)
  }
  # use what's in `_bookdown.yml` if specified
  path <- .yml_bd_get_output_dir()
  if (!is.null(path)) {
    return(path)
  }
  # use default if nothing pre-specified
  "_book"
}

.dir_set_docs_bookdown <- function(path) {
  .yml_bd_set_output_dir(path)
  .yml_dir_set_docs(path, NULL)
}

# Rmd/qmd (no other yml file of concern)
.dir_get_docs_md <- function() {
  yml_projr <- .yml_get(NULL)
  dir_docs_yml <- .yml_dir_get_path("docs", NULL)
  if (!is.null(dir_docs_yml)) {
    return(dir_docs_yml)
  }
  "docs"
}

# get cache directory to save to
.path_get_cache_auto_dir <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_check(profile = profile)
  .dir_get_cache_auto_path(profile) |>
    .path_get_full(...) |>
    .dir_get_create(create)
}

.path_get_cache_auto <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_check(profile = profile)
  path_dir <- .dir_get_cache_auto_path(profile) |>
    .path_get_full(...)
  .dir_get_create(dirname(path_dir), create)
  path_dir
}

.dir_get_cache_auto_version <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_check(profile = profile)
  .dir_get_cache_auto_path(profile) |>
    file.path("projr") |>
    .version_append() |>
    .path_get_full(...) |>
    .dir_get_create(create)
}

.path_get_cache_auto_version <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_check(profile = profile)
  path_dir <- .dir_get_cache_auto_path(profile) |>
    file.path("projr") |>
    .version_append() |>
    .path_get_full(...)
  .dir_get_create(dirname(path_dir), create)
  path_dir
}

#' @title Get `projr` build cache directory
#' @rdname projr_path_get_cache_build
#'
#' @description Get the cache directory for `projr` builds.
#' It is a sub-directory of the cache directory.
#' For development builds (.build_dev`), this is the final
#' directory for `output` and `docs` items.
#' For output builds (.build_output`), this is the staging
#' directory. After the documents are rendered, they are copied
#' to their final directories.
#'
#' .path_get_cache_build` assumes the path is to a file,
#' whereas .path_get_cache_build_dir` assumes the path
#' is to a directory.
#' This distinctiion is only relevant when `create = TRUE`, as it
#' determines what directory is attempted to be created.
#'
#' @param ... comma-separated strings specified initially the
#' label (e.g. `"docs"` or `"output"`) as well as, optionally, sub-directories
#' (e.g. `"img", "`). For example, .path_get_cache_build("docs", "img")`
#' returns the path to the `img` directory in the `docs` sub-directory
#' of the build cache directory.
#' @param create logical.
#' If \code{TRUE}, then the directory
#' is created if it does not exist.
#' @param profile character.
#' The name of the `projr` profile to use.
#' Default is \code{NULL}, which uses the current `projr` profile.
#' @return character.
#' Path to the cache (sub-)directory for `projr` builds.
#' @seealso.path_get.path_get_dir
#' @export
projr_path_get_cache_build_dir <- .dir_get_cache_auto_version

#' @rdname projr_path_get_cache_build
#' @export
projr_path_get_cache_build <- .path_get_cache_auto_version


.dir_get_cache_auto_path <- function(profile) {
  .yml_dir_get(profile)[[
    .dir_get_cache_auto_ind(profile)
  ]][["path"]]
}

.dir_get_cache_auto_check <- function(profile) {
  # find cache directory to save to
  stopifnot(
    "No cache directory specified, but required for projr builds." =
      length(.dir_get_cache_auto_ind(profile)) > 0
  )
}

.dir_get_cache_auto_ind <- function(profile) {
  which(
    grepl("^cache", .dir_label_strip(names(.yml_dir_get(profile))))
  )[1]
}

.dir_get_cache_auto_version_old <- function(..., create = TRUE, profile) {
  .dir_get_cache_auto_check(profile = profile)
  .dir_get_cache_auto_path() |>
    file.path("projr") |>
    .version_append() |>
    file.path("old", ...) |>
    .dir_get_create(create)
}
