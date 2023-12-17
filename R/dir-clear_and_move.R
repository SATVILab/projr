# clear
# ----------------------

.projr_dir_clear <- function(path_dir,
                             recursive_file = FALSE,
                             recursive_dir = FALSE,
                             delete_hidden = TRUE) {
  if (!.projr_dir_clear_check(path_dir)) {
    return(invisible(FALSE))
  }

  .projr_dir_clear_file(
    path_dir,
    delete_hidden = delete_hidden, recursive = recursive_file
  )
  .projr_dir_clear_dir(path_dir, recursive = recursive_dir)
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
  if (.projr_dir_check_identical_proj(path_dir)) {
    stop("Cannot clear the entire project", call. = FALSE)
  }
  invisible(TRUE)
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

.projr_dir_clear_file <- function(path,
                                  delete_hidden = TRUE,
                                  recursive = TRUE) {
  if (!.projr_dir_clear_file_check(path)) {
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
  ) |>
    fs::path_norm() |>
    as.character() |>
    .projr_file_filter_essential_non() |>
    .projr_file_filter_dir_non()
}

.projr_file_filter_essential_non <- function(fn, path_dir = NULL) {
  fn |> setdiff(.projr_file_filter_essential_non_get_essential(path_dir))
}

.projr_file_filter_essential_non_get_essential <- function(path_dir) {
  fn_vec_rel <- c(
    ".", "..", .projr_dir_proj_get(),
    dirname(.projr_dir_proj_get()), path_dir
  )
  fn_vec_abs <- fn_vec_rel |> .projr_file_get_abs()
  c(fn_vec_rel, fn_vec_abs) |>
    unique() |>
    fs::path_norm() |>
    as.character() |>
    unique()
}

.projr_file_filter_dir <- function(x) {
  x[fs::is_dir(x)]
}

.projr_file_filter_dir_non <- function(x) {
  x[!fs::is_dir(x)]
}

.projr_dir_clear_dir <- function(path, recursive = FALSE, dir_exc = NULL) {
  if (!.projr_dir_clear_check(path)) {
    return(invisible(FALSE))
  }
  path_vec_dir <- .projr_dir_clear_dir_ls(path, recursive) |>
    setdiff(dir_exc) |>
    .projr_file_filter_essential_non() |>
    .projr_file_filter_dir()
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
                              path_dir_to,
                              dir_exc = NULL) {
  .projr_dir_clear(path_dir_to)
  .projr_dir_copy(path_dir_from, path_dir_to, dir_exc = dir_exc)
}

.projr_dir_copy <- function(path_dir_from,
                            path_dir_to,
                            dir_exc = NULL) {
  .projr_dir_ls(path_dir_from) |>
    .projr_file_dir_exc(dir_exc) |>
    .projr_dir_copy_file(path_dir_from, path_dir_to)
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
    file.path(path_dir_from, fn) |> .projr_file_get_abs(),
    file.path(path_dir_to, fn) |> .projr_file_get_abs(),
    overwrite = TRUE
  )
  invisible(TRUE)
}

.projr_dir_copy_check <- function(fn, path_dir_from) {
  if (.projr_state_len_z(fn)) {
    return(invisible(FALSE))
  }
  .projr_file_get_abs_exists(fn, path_dir_from) |>
    .projr_state_len_nz()
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

.projr_dir_ls <- function(path_dir,
                          recursive = TRUE,
                          full.names = FALSE,
                          all.files = TRUE) {
  .projr_check_dir_exists(path_dir, "path_dir", required = TRUE)
  list.files(
    path_dir,
    recursive = recursive, full.names = full.names, all.files = all.files
  ) |>
    fs::path_norm() |>
    as.character() |>
    .projr_file_filter_essential_non(path_dir)
}

.projr_dir_ls_get_exc_auto <- function(path_dir = NULL) {
  c(
    ".", "..", .projr_dir_proj_get(),
    dirname(.projr_dir_proj_get()), path_dir
  ) |>
    fs::path_norm() |>
    as.character()
}
