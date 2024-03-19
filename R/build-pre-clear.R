# clear
# -----------------

.projr_build_clear_pre <- function(output_run) {
  # output directories
  .projr_build_clear_pre_output()
  # docs
  .projr_build_clear_pre_docs(output_run)
  # entire projrect cache, except for `old` directory
  # which is what the above are saving to if they're
  # caching
  .projr_dir_clear_pre_cache_version()
  invisible(TRUE)
}

.projr_dir_clear_pre_cache_version <- function() {
  .projr_dir_get_cache_auto_version(profile = NULL) |>
    .dir_clear_dir(dir_exc = "old")
}

.projr_build_clear_pre_output <- function() {
  # purpose:
  # we want to empty the safe output directories
  # (safe data and output, docs handled separately),
  # before the run so that we know they were produced by the run.
  # notes:
  # 1. clear output director(ies)
  # does not depend on output_run as
  # this is always saved to the same location
  # (and only copied across after builds on output runs,
  # nothing about non-safe dirs pre-run)
  label_vec <- .projr_yml_dir_get_label_out(NULL) |>
    setdiff(.projr_yml_dir_get_label_docs(NULL))
  for (i in seq_along(label_vec)) {
    .projr_build_clear_pre_output_label(label_vec[[i]])
  }
}

.projr_build_clear_pre_output_label <- function(label) {
  projr_path_get_dir(label, safe = TRUE) |>
    .dir_clear()
}

.projr_build_clear_pre_docs <- function(output_run) {
  # clear docs folder
  if (!.projr_build_clear_pre_docs_check(output_run)) {
    return(invisible(FALSE))
  }
  if (output_run) {
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
    .projr_path_get_cache_auto_dir("projr", "cleared_docs")
  )
}

.projr_build_clear_pre_docs_no_cache <- function(output_run) {
  projr_path_get_dir("docs", safe = !output_run) |>
    .dir_clear()
}
