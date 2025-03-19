.build_clear_post <- function(output_run, clear_output) {
  # clear final directories, in preparation
  # for the next build
  if (!.build_clear_post_check(output_run, clear_output)) {
    return(invisible(FALSE))
  }
  # clear the output folders (output and data),
  # as these would still contain output from
  # previous runs as they're not cleared pre-run
  label_vec <- c(.yml_dir_get_label_output(NULL), "docs", "data") |>
    unique()
  for (x in label_vec) {
    if (!.build_clear_post_check_label(x)) {
      next
    }
    projr_path_get_dir(x, safe = FALSE, create = FALSE) |> .dir_clear()
  }
  invisible(TRUE)
}

.build_clear_post_check <- function(output_run, clear_output) {
  # only clear final directories if 
  # we are clearing after build (i.e. we 
  # cleared "conservatively") in an output_run (i.e. if we
  # are going to copy across )
  invisible(output_run) && clear_output == "post"
}

.build_clear_post_check_label <- function(label) {
  # allow the exception that the quarto project
  # and bookdown folders are not cleared
  if (!label == "docs") {
    return(TRUE)
  }
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
  .dir_rm(path_dir_vec)
  invisible(TRUE)
}

.build_clear_old_output <- function() {
  .dir_get_cache_auto_version(profile = NULL) |>
    dirname() |>
    .dir_clear()
}
