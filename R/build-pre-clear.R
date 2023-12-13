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
  .projr_dir_get_cache_auto_version() |>
    .projr_dir_clear_dir(recursive = FALSE, dir_exc = "old")
}

.projr_build_clear_pre_output <- function(cache) {
  # clear output director(ies)
  # does not depend on output_run as
  # this is always saved to the same location
  # (and only copied across after builds on output runs,
  # nothing about non-safe dirs pre-run)
  # we clear safe output directories before copying afterwards
  for (x in .projr_build_clear_pre_output_get_label()) {
    .projr_build_clear_pre_output_label(x, cache)
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
  .projr_dir_mimick(
    .projr_dir_get(label, safe = TRUE),
    .projr_dir_get_cache_auto_version_old(label)
  )
}

.projr_build_clear_pre_output_label_no_cache <- function(label) {
  .projr_dir_get(label, safe = TRUE) |>
    .projr_dir_clear()
}

.projr_build_clear_pre_output_get_label <- function() {
  label_vec <- names(.projr_yml_dir_get())
  label_vec[grepl("^output", .projr_dir_label_strip(label_vec))] |>
    c("data") |>
    unique()
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
  path_docs <- projr_dir_get("docs", safe = !output_run) |>
    normalizePath(winslash = "/")
  path_proj_root <- .projr_dir_proj_get() |> normalizePath(winslash = "/")
  identical(path_docs, path_proj_root)
}

.projr_build_clear_pre_docs_cache <- function(output_run) {
  .projr_dir_mimick(
    .projr_dir_get("docs", safe = !output_run),
    .projr_dir_get_cache_auto("projr", "cleared_docs")
  )
}

.projr_build_clear_pre_docs_no_cache <- function(output_run) {
  .projr_dir_get("docs", safe = !output_run) |>
    .projr_dir_clear()
}
