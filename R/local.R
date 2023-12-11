.projr_local_send_dir <- function(path_dir_local,
                                  path_dir_local_dest,
                                  conflict) {
  # remove pre-existing files and error on overwrite
  fn_vec_rel <- list.files(path_dir_local, recursive = TRUE, full.names = TRUE)
  if (length(fn_vec_rel) == 0L) {
    return(invisible(FALSE))
  }
  .projr_local_send_file(
    fn_rel = fn_vec_rel,
    path_dir_local = path_dir_local,
    path_dir_local_dest = path_dir_local_dest,
    conflict = conflict
  )
}

.projr_local_send_file <- function(fn_rel,
                                   path_dir_local,
                                   path_dir_local_dest,
                                   conflict) {
  # remove pre-existing files and error on overwrite
  if (length(fn_rel) == 0L) {
    return(invisible(FALSE))
  }
  attach(
    .projr_local_conflict_manage(
      path_file_source = file.path(path_dir_local, fn_rel),
      path_file_dest = file.path(path_dir_local_dest, fn_rel),
      conflict = conflict
    ),
    warn.conflicts = FALSE
  )
  if (length(path_file_dest) == 0L) { # nolint
    return(invisible(FALSE))
  }
  .projr_local_dir_create(path_file_dest) # nolint

  # copy the files
  all(file.copy(path_file_source, path_file_dest, overwrite = TRUE)) # nolint
}

.projr_local_conflict_manage <- function(path_file_source,
                                         path_file_dest,
                                         conflict) {
  if (conflict == "overwrite") {
    return(list(
      "path_file_source" = path_file_source, "path_file_dest" = path_file_dest
    ))
  }
  skip_vec_ind <- file.exists(path_file_dest)
  if (sum(skip_vec_ind) > 0 && conflict == "error") {
    stop(paste0(
      "File already exists at destination: ",
      path_file_dest[skip_vec_ind], " and conflict = 'error'"
    ))
  }
  path_file_dest <- path_file_dest[!skip_vec_ind]
  path_file_source <- path_file_source[!skip_vec_ind]
  list(
    "path_file_source" = path_file_source, "path_file_dest" = path_file_dest
  )
}

.projr_local_dir_create <- function(fn) {
  dir_vec <- unique(dirname(fn))
  for (i in seq_along(dir_vec)) {
    .projr_dir_create(dir_vec[i])
  }
  invisible(TRUE)
}

.projr_local_rm_file <- function(fn_rel,
                                 path_dir) {
  all(file.remove(file.path(path_dir, fn_rel)))
}

.projr_get_dir_local <- function(label, bump_component) {
  projr_dir_get(
    label = label,
    safe = !.projr_run_output_check(bump_component = bump_component)
  )
}
