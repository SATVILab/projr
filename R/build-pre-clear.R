# clear
# -----------------

.projr_build_clear_pre <- function(output_run, cache = TRUE) {
  # output directories
  .projr_build_clear_pre_output(cache)
  # docs
  .projr_build_clear_pre_docs(output_run, cache)
  # entire projrect cache, except for `old` directory
  # which is what the above are saving to if they're
  # caching
  .projr_dir_clear_pre_cache_version()
  invisible(TRUE)
}

.projr_dir_clear_pre_cache_version <- function() {
  .projr_dir_get_cache_auto_version(profile = NULL) |>
    .projr_dir_clear_dir(recursive = FALSE, dir_exc = "old")
}

.projr_build_clear_pre_output <- function(cache) {
  # purpose:
  # we want to empty the safe output directories
  # (safe data, output and docs),
  # and caching docs as well if requested (see below; the default),
  # before the run so that we know they were produced by the run.
  # notes:
  # 1. clear output director(ies)
  # does not depend on output_run as
  # this is always saved to the same location
  # (and only copied across after builds on output runs,
  # nothing about non-safe dirs pre-run)
  # 2. cache only docs directory
  # only cache docs as the other two output types
  # are written to cache directories by default anyway
  # (as long as people use `.projr_use_data`)
  # we clear safe output directories before copying afterwards
  for (x in .projr_yml_dir_get_label_out(NULL)) {
    cache_label <- cache && x %in% .projr_yml_dir_get_label_docs(NULL)
    .projr_build_clear_pre_output_label(x, cache_label)
  }
}

.projr_build_clear_pre_output_label <- function(label, cache) {
  if (cache) {
    .projr_build_clear_pre_output_label_cache(label)
  } else {
    .projr_build_clear_pre_output_label_no_cache(label)
  }
}

.projr_build_clear_pre_output_label_cache <- function(label) {
  .dir_copy_exact(
    projr_path_get_dir(label, safe = TRUE),
    .projr_dir_get_cache_auto_version(label, profile = NULL)
  )
}

.projr_build_clear_pre_output_label_no_cache <- function(label) {
  projr_path_get_dir(label, safe = TRUE) |>
    .dir_clear()
}

.projr_build_clear_pre_docs <- function(output_run, cache) {
  # clear docs folder
  if (!.projr_build_clear_pre_docs_check(output_run)) {
    return(invisible(FALSE))
  }
  if (cache) {
    .projr_build_clear_pre_docs_cache(output_run)
  } else {
    .projr_build_clear_pre_docs_no_cache(output_run)
  }
  invisible(TRUE)
}

.projr_build_clear_pre_docs_check <- function(output_run) {
  path_docs <- projr_path_get_dir("docs", safe = !output_run) |>
    normalizePath(winslash = "/")
  path_proj_root <- .dir_proj_get() |> normalizePath(winslash = "/")
  identical(path_docs, path_proj_root)
}

.projr_build_clear_pre_docs_cache <- function(output_run) {
  .dir_copy_exact(
    projr_path_get_dir("docs", safe = !output_run),
    .projr_dir_get_cache_auto("projr", "cleared_docs")
  )
}

.projr_build_clear_pre_docs_no_cache <- function(output_run) {
  projr_path_get_dir("docs", safe = !output_run) |>
    .dir_clear()
}
