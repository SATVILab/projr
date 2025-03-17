# clear
# -----------------

.build_clear_pre <- function(output_run, clear_output) {
  # output directories
  .build_clear_pre_output(clear_output)
  # docs
  .build_clear_pre_docs(output_run, clear_output)
  # entire projrect cache, except for `old` directory
  # which is what the above are saving to if they're
  # caching
  .dir_clear_pre_cache_version()
  invisible(TRUE)
}

.dir_clear_pre_cache_version <- function() {
  .dir_get_cache_auto_version(profile = NULL) |>
    .dir_clear_dir(dir_exc = "old")
}

.build_clear_pre_output <- function(clear_output) {
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
  if (clear_output %in% c("post", "never")) {
    return(invisible(FALSE))
  }
  label_vec <- .yml_dir_get_label_out(NULL) |>
    setdiff(.yml_dir_get_label_docs(NULL))
  for (i in seq_along(label_vec)) {
    .build_clear_pre_output_label(label_vec[[i]])
  }
  invisible(TRUE)
}

.build_clear_pre_output_label <- function(label) {
  projr_path_get_dir(label, safe = TRUE) |>
    .dir_clear()
}

.build_clear_pre_docs <- function(output_run, clear_output) {
  if (clear_output %in% c("post", "never")) {
    return(invisible(FALSE))
  }
  # clear docs folder
  if (!.build_clear_pre_docs_check(output_run)) {
    return(invisible(FALSE))
  }
  if (output_run) {
    .build_clear_pre_docs_cache(output_run)
  } else {
    .build_clear_pre_docs_no_cache(output_run)
  }
  invisible(TRUE)
}

.build_clear_pre_docs_check <- function(output_run) {
  path_docs <- projr_path_get_dir("docs", safe = !output_run) |>
    normalizePath(winslash = "/")
  path_proj_root <- .path_get() |> normalizePath(winslash = "/")
  identical(path_docs, path_proj_root)
}

.build_clear_pre_docs_cache <- function(output_run) {
  .dir_copy_exact(
    projr_path_get_dir("docs", safe = !output_run),
    .path_get_cache_auto_dir("projr", "cleared_docs")
  )
}

.build_clear_pre_docs_no_cache <- function(output_run) {
  projr_path_get_dir("docs", safe = !output_run) |>
    .dir_clear()
}
