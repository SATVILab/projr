.build_clear_post_safe <- function(output_run, clear_output) {
  .cli_debug("Starting .build_clear_post_safe()")
  .cli_debug("  output_run: {output_run}")
  .cli_debug("  clear_output: {clear_output}")

  # clear safe directories before moving new docs and outputs into them
  # move rmd and qmd's (not bookdown or quarto projects)
  # to the final docs directories
  .cli_debug("Calling .build_clear_post_safe_docs()")
  .build_clear_post_safe_docs(output_run)
  .cli_debug("Completed .build_clear_post_safe_docs()")

  # clear final directories, in preparation
  # for the next build
  if (!.build_clear_post_safe_check(clear_output)) {
    .cli_debug("Skipping post-clear: check failed (clear_output != 'post')")
    return(invisible(FALSE))
  }

  .cli_debug("Proceeding with post-clear of output directories")

  # clear the output folders (output and data),
  # as these would still contain output from
  # previous runs as they're not cleared pre-run
  label_vec <- c(.yml_dir_get_label_output(NULL), "data") |>
    unique()
  .cli_debug("  Labels to clear: {paste(label_vec, collapse = ', ')}")

  for (x in label_vec) {
    path_to_clear <- projr_path_get_dir(x, safe = FALSE, create = FALSE)
    .cli_debug("  Clearing {x} at: {path_to_clear}")
    .dir_clear(path_to_clear)
    .cli_debug("  Completed clearing {x}")
  }

  .cli_debug("Finished .build_clear_post_safe()")
  invisible(TRUE)
}

.build_clear_post_safe_docs <- function(output_run) {
  .cli_debug("Starting .build_clear_post_safe_docs()")
  .cli_debug("  output_run: {output_run}")

  # always clear docs for output directories
  # if not quarto projects or bookdown projects.
  # don't clear quarto projects or bookdown projects
  # as we save directly there.
  if (output_run && .build_clear_post_safe_check_docs()) {
    engine <- .engine_get()
    .cli_debug("  engine: {engine}")
    .cli_debug("  Clearing docs directory (engine allows it)")

    path_to_clear <- projr_path_get_dir("docs", safe = FALSE, create = FALSE)
    .cli_debug("  Docs path to clear: {path_to_clear}")

    .dir_clear(path_to_clear)
    .cli_debug("  Completed clearing docs directory")
    .cli_debug("Finished .build_clear_post_safe_docs() - cleared")
  } else {
    .cli_debug("  Skipping docs clear: output_run={output_run}, check_docs={.build_clear_post_safe_check_docs()}")
    .cli_debug("Finished .build_clear_post_safe_docs() - skipped")
    invisible(FALSE)
  }
}

.build_clear_post_safe_check <- function(clear_output) {
  # only clear final directories if
  # we are clearing after build (i.e. we
  # cleared "conservatively") in an output_run (i.e. if we
  # are going to copy across )
  clear_output == "post"
}

.build_clear_post_safe_check_docs <- function() {
  # allow the exception that the quarto project
  # and bookdown folders are not cleared
  .engine_get() %in% c("quarto_document", "rmd")
}

# clear old temporary output directories
# ---------------------------------------
.build_clear_old <- function(output_run, old_dev_remove) {
  if (!.build_clear_old_check(old_dev_remove)) {
    return(invisible(FALSE))
  }
  switch(as.character(output_run),
    "FALSE" = .build_clear_old_dev(),
    "TRUE" = .build_clear_old_output()
  )
}

.build_clear_old_check <- function(old_dev_remove) {
  invisible(old_dev_remove)
}

.build_clear_old_dev <- function() {
  path_dir <- .dir_get_cache_auto_version(profile = NULL) |>
    dirname()
  if (!dir.exists(path_dir) || !nzchar(path_dir)) {
    return(invisible(FALSE))
  }
  path_dir_vec <- path_dir |>
    .dir_ls(recursive = FALSE) |>
    setdiff(.version_get_v())
  if (.is_len_0(path_dir_vec)) {
    return(invisible(FALSE))
  }
  # Convert relative paths to full paths
  path_dir_vec_full <- file.path(path_dir, path_dir_vec)
  .dir_rm(path_dir_vec_full)
  invisible(TRUE)
}

.build_clear_old_output <- function() {
  path_dir <- .dir_get_cache_auto_version(profile = NULL) |>
    dirname()
  if (!dir.exists(path_dir) || !nzchar(path_dir)) {
    return(invisible(FALSE))
  }
  dir_vec <- path_dir |>
    .dir_ls(recursive = FALSE) |>
    setdiff("log")
  for (i in seq_along(dir_vec)) {
    path_dir <- projr_path_get_dir(
      "cache", "projr", dir_vec[[i]],
      create = FALSE
    )
    if (!file.exists(path_dir)) {
      next
    }
    unlink(
      path_dir,
      recursive = TRUE, force = TRUE
    )
  }
  invisible(TRUE)
}
