# clear
# ----------------------

.projr_dir_clear <- function(path_dir,
                             delete_directories = TRUE,
                             delete_hidden = TRUE) {
  if (!.projr_dir_clear_check(path_dir)) {
    return(invisible(FALSE))
  }

  .projr_dir_clear_file(path_dir, delete_hidden = delete_hidden, recursive = FALSE)
  .projr_dir_clear_dir(path_dir, recursive = FALSE)
  invisible(TRUE)
}

.projr_dir_clear_check <- function(path_dir) {
  path_dir <- path_dir |>
    fs::path_norm() |>
    as.character()
  # nothing to do if it doesn't exist
  if (!dir.exists(path_dir)) {
    return(invisible(FALSE))
  }
  # don't do if it it's the project directory
  !.projr_dir_check_identical_proj(path_dir)
}

.projr_dir_check_identical <- function(path_dir_one, path_dir_two) {
  identical(
    .projr_dir_ls(path_dir_one) |> sort(),
    .projr_dir_ls(path_dir_two) |> sort()
  )
}
.projr_dir_check_identical_proj <- function(path_dir) {
  .projr_dir_check_identical(path_dir, .projr_dir_proj_get())
}

.projr_dir_clear_file <- function(path, delete_hidden = TRUE, recursive = TRUE) {
  if (!.projr_dir_clear_file_check()) {
    return(invisible(FALSE))
  }
  .projr_dir_clear_file_ls(path, delete_hidden, recursive) |>
    .projr_file_exclude_essential() |>
    .projr_file_rm()
  invisible(TRUE)
}

.projr_dir_clear_file_ls <- function(path_dir, delete_hidden, recursive) {
  list.files(
    path = path_dir,
    recursive = recursive, full.names = TRUE, all.files = delete_hidden
  )
}

.projr_dir_clear_dir <- function(path, recursive = FALSE, dir_exc = NULL) {
  if (!.projr_dir_clear_check(path)) {
    return(invisible(FALSE))
  }
  path_vec_dir <- .projr_dir_clear_dir_ls(path, recursive) |>
    setdiff(dir_exc)
  for (i in seq_along(path_vec_dir)) {
    unlink(path_vec_dir[i], recursive = TRUE)
  }
  invisible(TRUE)
}

.projr_dir_clear_dir_ls <- function(path_dir, recursive) {
  list.dirs(
    path_dir,
    recursive = recursive, full.names = TRUE
  )
}


.projr_dir_clear_file_check <- function(path_dir, delete_hidden = TRUE) {
  if (!.projr_dir_clear_check(path_dir)) {
    return(invisible(FALSE))
  }
  .projr_dir_clear_file_ls(path_dir, delete_hidden, recursive = TRUE) |>
    .projr_state_len_nz()
}

# move
# ----------------------
.projr_dir_mimick <- function(path_dir_from,
                              path_dir_to) {
  .projr_dir_clear(path_dir_to)
  .projr_dir_copy(path_dir_from, path_dir_to)
}

.projr_dir_copy <- function(path_dir_from,
                            path_dir_to) {
  fn_vec <- .projr_dir_ls(path_dir_from)
  .projr_dir_copy_file(fn_vec, path_dir_from, path_dir_to)
  .projr_dir_copy_tree(path_dir_from = path_dir_from, path_dir_to = path_dir_to)
}

.projr_dir_mimick_file <- function(fn,
                                   path_dir_from,
                                   path_dir_to) {
  .projr_dir_clear(path_dir_to)
  .projr_dir_copy_file(fn, path_dir_from, path_dir_to)
}

.projr_dir_copy_file <- function(fn,
                                 path_dir_from,
                                 path_dir_to) {
  if (!.projr_dir_copy_check(fn, path_dir_from)) {
    return(invisible(FALSE))
  }
  .projr_dir_copy_tree(path_dir_from = path_dir_from, path_dir_to = path_dir_to)
  fs::file_copy(
    .projr_file_get_abs_exists(path_dir_from), .projr_dir_fn_get_abs(fn_vec_abs_dest),
    overwrite = TRUE
  )
  invisible(TRUE)
}

.projr_dir_copy_check <- function(fn, path_dir_from) {
  if (.projr_state_len_z(fn)) {
    return(invisible(FALSE))
  }
  fn_vec_source_exists <- .projr_file_get_abs_exists(fn, path_dir_from)
  length(fn_vec_source_exists) > 0L
}

.projr_dir_move <- function(path_dir_from,
                            path_dir_to) {
  .projr_dir_copy_tree(path_dir_from = path_dir_from, path_dir_to = path_dir_to)
  .projr_dir_move_file(
    fn = .projr_dir_ls(path_dir_from),
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to
  )
  .projr_dir_clear(path_dir_from)
}

.projr_dir_move_file <- function(fn,
                                 path_dir_from,
                                 path_dir_to) {
  if (!.projr_dir_copy_check(fn, path_dir_from)) {
    return(invisible(FALSE))
  }
  .projr_dir_copy_tree(path_dir_from = path_dir_from, path_dir_to = path_dir_to)
  fs::file_move(
    .projr_file_get_abs_exists(path_dir_from), .projr_dir_fn_get_abs(fn_vec_abs_dest),
    overwrite = TRUE
  )
  invisible(TRUE)
}

.projr_dir_rm <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(FALSE))
  }
  unlink(path, recursive = TRUE)
  invisible(TRUE)
}

.projr_dir_ls <- function(path_dir, recursive = TRUE, full.names = FALSE) {
  list.files(
    path_dir,
    recursive = recursive, full.names = full.names
  )
}
