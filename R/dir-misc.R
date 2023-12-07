.projr_dir_clear <- function(path_dir,
                             delete_directories = TRUE,
                             delete_hidden = TRUE) {
  if (!.projr_dir_clear_check(path_dir)) {
    return(invisible(FALSE))
  }

  fn_vec_dir <- list.files(
    path_dir,
    recursive = TRUE, all.files = TRUE,
    full.names = FALSE
  ) |>
    sort()
  fn_vec_proj <- list.files(
    .projr_dir_proj_get(),
    recursive = TRUE, all.files = TRUE,
    full.names = FALSE
  ) |>
    sort()
  if (identical(fn_vec_dir, fn_vec_proj)) {
    stop("Attempting to delete entire project directory")
  }
  # delete directories
  if (delete_directories) {
    path_vec_dir <- list.dirs(
      path_dir,
      recursive = FALSE, full.names = TRUE
    )
    for (i in seq_along(path_vec_dir)) {
      unlink(path_vec_dir[i], recursive = TRUE)
    }
    # delete files
    fn_vec <- list.files(
      path_dir,
      recursive = FALSE, full.names = TRUE, all.files = TRUE
    )
    fn_vec <- fn_vec |>
      fs::path_norm() |>
      as.character()
    exc_vec <- c(
      ".", "..", path_dir, dirname(path_dir)
    )
    fn_vec <- setdiff(fn_vec, exc_vec)
    invisible(file.remove(fn_vec))
  } else {
    fn_vec <- list.files(
      path = path_dir,
      recursive = TRUE, full.names = TRUE, all.files = TRUE
    )
    if (length(fn_vec) == 0) {
      return(invisible(TRUE))
    }
    fn_vec <- fn_vec |>
      fs::path_norm() |>
      as.character()
    wd <- normalizePath(projr_dir_get("project"), winslash = "/")
    exc_vec <- c(
      ".", "..", wd, dirname(wd), normalizePath(path_dir, winslash = "/")
    )
    fn_vec <- setdiff(fn_vec, exc_vec)
    if (length(fn_vec) > 0) {
      invisible(file.remove(fn_vec))
    }
  }
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

.projr_dir_ls <- function(path_dir, full_names = FALSE) {
  out <- list.files(path_dir, recursive = TRUE, all.files = TRUE, full.names = full_names)
  if (length(out) == 0L) character() else out
}

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

.projr_dir_create

.projr_dir_copy_check <- function(fn, path_dir_from) {
  if (length(fn) == 0L) {
    return(invisible(FALSE))
  }
  fn_vec_source_exists <- .projr_file_get_abs_exists(fn, path_dir_from)
  length(fn_vec_source_exists) > 0L
}

.projr_file_get_abs_exists <- function(fn, path_dir) {
  .projr_file_get_abs(fn, path_dir) |>
    .projr_file_filter_exists()
}

.projr_file_get_abs <- function(fn, path_dir = NULL) {
  .projr_file_get_abs_check(fn = fn, path_dir = path_dir)
  if (.projr_state_abs(fn)) {
    return(fn)
  }
  if (.projr_state_null(path_dir)) {
    path_dir <- .projr_dir_get_proj()
  }
  .projr_file_get_full(fn, path_dir) |>
    .projr_file_get_abs_single()
}

.projr_file_get_abs_single <- function(x) {
  .projr_check_chr_single(x, "x")
  fs::path_abs(x) |>
    as.character()
}

.projr_file_get_abs_check <- function(fn, path_dir) {
  .projr_check_chr_nz(fn, "fn", required = TRUE)
  .projr_check_chr_nz(path_dir, "path_dir")
  if (.projr_state_abs(fn) && !.projr_state_null(path_dir)) {
    stop(paste0(
      "fn is absolute, but path_dir is not NULL:\n",
      paste0("  - fn ", fn, "\n"),
      paste0("  - path_dir, ", path_dir, "\n")
    ))
  }
}

.projr_file_filter_exists <- function(fn) {
  fn[.projr_file_state_exists(fn)]
}

.projr_file_state_exists <- function(fn) {
  file.exists(fn)
}

.projr_file_get_full <- function(fn, path_dir) {
  file.path(path_dir, fn)
}

.projr_file_get_full_exists <- function(fn, path_dir) {
  .projr_file_get_full(fn, path_dir) |>
    .projr_file_state_exists()
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

.projr_dir_label_strip <- function(x) {
  gsub("_", "", gsub("-", "", x)) |>
    tolower()
}

.projr_file_rm <- function(fn) {
  if (file.exists(fn)) {
    file.remove(fn)
  }
  invisible(TRUE)
}
