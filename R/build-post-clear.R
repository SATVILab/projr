.projr_build_clear_post <- function(output_run) {
  if (!.projr_build_clear_post_check(output_run)) {
    return(invisible(FALSE))
  }
  for (x in .projr_yml_dir_get_label_output()) {
    projr_path_get_dir(x, safe = FALSE) |> .projr_dir_clear()
  }
  invisible(TRUE)
}

.projr_build_clear_post_check <- function(output_run) {
  invisible(output_run)
}

.projr_build_clear_post_cache <- function(output_run) {
  # clear projr cache directories
  .projr_dir_get_cache_auto_version() |>
    .projr_dir_clear()
}

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
  .projr_dir_get_cache_auto_version() |>
    dirname() |>
    .projr_dir_ls() |>
    setdiff(projr_version_get()) |>
    vapply(.projr_dir_rm, logical(1))
}

.projr_build_clear_old_output <- function() {
  .projr_dir_get_cache_auto_version() |>
    dirname() |>
    .projr_dir_clear()
}
