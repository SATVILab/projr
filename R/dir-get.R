# Helper functions - getting paths
# ===========================================================================

.dir_get <- function(label, ..., safe) {
  .dir_get_label(label, safe) |> .path_get_full(...)
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
  if (as.character(.path_resolve_root(path) |> fs::path_norm()) == path_proj) {
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
  if (label != "docs") return(label)
  docs <- .path_resolve_root(.yml_dir_get_path("docs", NULL))
  root <- .path_get()
  if (fs::path_has_parent(docs, root) && docs != root) {
    return(as.character(fs::path_rel(docs, root)))
  }
  "docs"
}

.dir_get_label_unsafe <- function(label) {
  .yml_dir_get_path(label, NULL)
}

.dir_set_docs_safe <- function(path, label) {
  if (!.dir_set_docs_safe_check(label)) {
    return(invisible(FALSE))
  }
  switch(.engine_get_auto(),
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
.dir_set_docs_quarto_project <- function(path) {
  .yml_quarto_set_output_dir(path)
  return(invisible(TRUE))
}

# bookdown
.dir_set_docs_bookdown <- function(path) {
  .yml_bd_set_output_dir(path)
}

# get cache directory to save to
.path_get_cache_auto_dir <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_path(profile) |>
    .path_get_full(...) |>
    .dir_get_create(create)
}

.path_get_cache_auto <- function(..., create = FALSE, profile) {
  path_dir <- .dir_get_cache_auto_path(profile) |>
    .path_get_full(...)
  .dir_get_create(dirname(path_dir), create)
  path_dir
}

.dir_get_cache_auto_version <- function(..., create = FALSE, profile) {
  .dir_get_cache_auto_path(profile) |>
    file.path("projr") |>
    .version_append() |>
    .path_get_full(...) |>
    .dir_get_create(create)
}

.path_get_cache_auto_version <- function(..., create = FALSE, profile) {
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
projr_path_get_cache_build_dir <- function(..., create = FALSE, profile = NULL) {
  .cache_path_for_caller(.dir_get_cache_auto_version(..., create = create, profile = profile))
}

#' @rdname projr_path_get_cache_build
#' @export
projr_path_get_cache_build <- function(..., create = FALSE, profile = NULL) {
  .cache_path_for_caller(.path_get_cache_auto_version(..., create = create, profile = profile))
}

.cache_path_for_caller <- function(path) {
  if (fs::is_absolute_path(path)) return(path)
  .path_force_rel(.path_resolve_root(path), getwd())
}


.dir_get_cache_auto_path <- function(profile) {
  directories <- .yml_get_null(profile)[["directories"]]
  labels <- names(directories)
  cache <- labels[startsWith(.dir_label_strip(labels), "cache")]
  if (!length(cache)) stop("No cache directory specified, but required for projr builds.", call. = FALSE)
  path <- directories[[cache[[1L]]]][["path"]]
  .assert_string(path, TRUE, nm = paste0("directories.", cache[[1L]], ".path"))
  .template_expand(path)
}
