.projr_build_clear_post <- function(output_run) {
  if (!.projr_build_clear_post_check(output_run)) {
    return(invisible(FALSE))
  }
  # clear the output folders (output and data),
  # as these would still contain output from
  # previous runs as they're not cleared pre-run
  for (x in c(.projr_yml_dir_get_label_output(NULL), "data")) {
    projr_path_get_dir(x, safe = FALSE) |> .dir_clear()
  }
  invisible(TRUE)
}

.projr_build_clear_post_check <- function(output_run) {
  invisible(output_run)
}

# clear old temporary output directories
# ---------------------------------------
.projr_build_clear_old <- function(output_run, old_dev_remove) {
  if (!.projr_build_clear_old_check(old_dev_remove)) {
    return(invisible(FALSE))
  }
  switch(as.character(output_run),
    "FALSE" = .projr_build_clear_old_dev(),
    "TRUE" = .projr_build_clear_old_output()
  )
}

.projr_build_clear_old_check <- function(old_dev_remove) {
  invisible(old_dev_remove)
}

.projr_build_clear_old_dev <- function() {
  path_dir <- .projr_dir_get_cache_auto_version(profile = NULL) |>
    dirname()
  if (!dir.exists(path_dir)) {
    return(invisible(FALSE))
  }
  path_dir_vec <- path_dir |>
    .dir_ls() |>
    setdiff(.projr_version_get_v())
  if (.is_len_0(path_dir_vec)) {
    return(invisible(FALSE))
  }
  .dir_rm(path_dir_vec)
  invisible(TRUE)
}

.projr_build_clear_old_output <- function() {
  .projr_dir_get_cache_auto_version(profile = NULL) |>
    dirname() |>
    .dir_clear()
}
