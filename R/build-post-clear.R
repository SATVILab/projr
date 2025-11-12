.build_clear_post <- function(output_run, clear_output) {
  .build_clear_post_docs(output_run)
  # clear final directories, in preparation
  # for the next build
  if (!.build_clear_post_check(clear_output)) {
    return(invisible(FALSE))
  }
  # clear the output folders (output and data),
  # as these would still contain output from
  # previous runs as they're not cleared pre-run
  label_vec <- c(.yml_dir_get_label_output(NULL), "data") |>
    unique()
  for (x in label_vec) {
    projr_path_get_dir(x, safe = FALSE, create = FALSE) |> .dir_clear()
  }
  invisible(TRUE)
}

.build_clear_post_docs <- function(output_run) {
  # always clear docs for output directories
  # if not quarto projects or bookdown projects.
  # don't clear quarto projects or bookdown projects
  # as we save directly there.
  if (output_run && .build_clear_post_check_docs()) {
    projr_path_get_dir("docs", safe = FALSE, create = FALSE) |> .dir_clear()
  } else {
    invisible(FALSE)
  }
}

.build_clear_post_check <- function(clear_output) {
  # only clear final directories if
  # we are clearing after build (i.e. we
  # cleared "conservatively") in an output_run (i.e. if we
  # are going to copy across )
  clear_output == "post"
}

.build_clear_post_check_docs <- function() {
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
  if (!dir.exists(path_dir)) {
    return(invisible(FALSE))
  }
  path_dir_vec <- path_dir |>
    .dir_ls() |>
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
  .dir_get_cache_auto_version(profile = NULL) |>
    dirname() |>
    .dir_clear()
}
