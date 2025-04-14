# clear
# -----------------

.build_clear_pre <- function(output_run, clear_output) {
  # output directories
  .build_clear_pre_output(clear_output)
  # never need to clear `docs` directories,
  # as we control where they're saved to.
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
  # clear all the output directories
  # to which the user may be saving.
  # if clearing beforehand, then this is 
  # the temporary and final output directories.
  # if clearing afterwards, then this is just
  # the final output directories (as 
  # we assume the user knows the final output
  # directories will be cleared before
  # copying stuff across).
  if (clear_output == "never") {
    return(invisible(FALSE))
  }
  # docs handled separately
  label_vec <- .yml_dir_get_label_out(NULL) |>
    setdiff(.yml_dir_get_label_docs(NULL))
  for (i in seq_along(label_vec)) {
    .build_clear_pre_output_label(label_vec[[i]], clear_output)
  }
  invisible(TRUE)
}

.build_clear_pre_output_label <- function(label, clear_output) {
  projr_path_get_dir(label, safe = TRUE, create = FALSE) |>
    .dir_clear()
  if (clear_output == "pre") {
    # clear unsafe directories, as if we are clearing
    # beforehand we are allowing user to save there
    # directly.
    # but we also clear the safe directories
    # because the user might be using them
    projr_path_get_dir(label, safe = FALSE, create = FALSE) |>
      .dir_clear()
  }
  invisible(TRUE)
}

.build_clear_pre_docs <- function(clear_output) {
  if (clear_output %in% "never") {
    return(invisible(FALSE))
  }
  # clear docs folder
  if (!.build_clear_pre_docs_check(clear_output)) {
    return(invisible(FALSE))
  }
  switch(clear_output,
    # if we are clearing before building,
    # then we always save docs directly to
    # the final docs folder
    "pre" = .build_clear_pre_docs_unsafe(),
    # if we are clearing after building,
    # then we initially save to a temporary
    # location, and then copy
    # to the final location
    "post" = .build_clear_pre_docs_safe()
  )
  invisible(TRUE)
}

.build_clear_pre_docs_check <- function(clear_output) {
  # use the temporary directory if clearing afterwards,
  # and the final directory if clearing before
  path_docs <- projr_path_get_dir("docs", safe = clear_output == "post") |>
    normalizePath(winslash = "/")
  path_proj_root <- .path_get() |> normalizePath(winslash = "/")
  identical(path_docs, path_proj_root)
}

.build_clear_pre_docs_safe <- function() {
  projr_path_get_dir("docs", safe = TRUE, create = FALSE) |> .dir_clear()
}

.build_clear_pre_docs_unsafe <- function() {
  projr_path_get_dir("docs", safe = FALSE, create = FALSE) |> .dir_clear()
}
