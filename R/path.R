# ========================
# paths
# ========================

# ------------------
# filter
# ------------------

.path_filter_spec <- function(fn, exc = NULL) {
  # filter based on a character vector of
  # paths to exclude
  if (is.null(exc)) {
    return(fn)
  }
  .assert_chr(exc)
  pattern <- paste0(
    paste0("^", gsub("/+$|^\\^", "", exc), "/", collapse = "|"),
    "|",
    paste0("^", gsub("/+$|^\\^", "", exc), "$", collapse = "|")
  )
  fn[!grepl(pattern, fn)]
}

# -------------------
# transform
# -------------------

# relative
.path_force_rel <- function(path, path_dir = NULL) {
  path |>
    fs::path_rel(path_dir %||% .dir_proj_get()) |>
    as.character()
}

# absolute
.path_force_abs <- function(fn, path_dir = NULL) {
  .path_force_abs_check(fn = fn, path_dir = path_dir)
  if (all(fs::is_absolute_path(fn))) {
    return(fn)
  }
  if (is.null(path_dir)) {
    path_dir <- .dir_proj_get()
  }
  file.path(path_dir, fn) |>
    fs::path_abs() |>
    as.character()
}

.path_force_abs_check <- function(fn, path_dir) {
  .assert_chr(fn, TRUE)
  .assert_string(path_dir)
  if (all(fs::is_absolute_path(fn)) && !is.null(path_dir)) {
    stop(paste0(
      "fn is absolute, but path_dir is not NULL:\n",
      paste0("  - fn ", fn, "\n"),
      paste0("  - path_dir, ", path_dir, "\n")
    ))
  }
}

# full
.path_get_full <- function(path_dir, ...) {
  .assert_string(path_dir, TRUE)
  path_append <- list(...) |> unlist()
  do.call(
    "file.path",
    args = list(path_dir) |> append(path_append)
  ) |>
    as.character()
}

# ==============================
# file
# ==============================

# ---------------------
# filter
# ---------------------

.file_filter_dir <- function(x) {
  x[fs::is_dir(x)]
}

.file_filter_dir_non <- function(x) {
  # fs::is_dir returns FALSE for
  # files that do not exist, so ensuring that
  # the files exist makes this match file_filter_dir
  # better
  x[!fs::is_dir(x) & file.exists(x)]
}

# ---------------------
# list
# ---------------------

.file_ls <- function(path_dir,
                     recursive = TRUE,
                     full.names = FALSE,
                     all.files = TRUE) {
  # list all files, with useful defaults,
  # whilst excluding the project directory
  # and the directory itself
  .assert_string(path_dir, TRUE)
  .assert_dir_exists(path_dir, TRUE)
  .assert_flag(recursive, TRUE)
  .assert_flag(full.names, TRUE)
  .assert_flag(all.files, TRUE)
  path_vec <- list.files(
    path_dir,
    recursive = recursive, full.names = full.names, all.files = all.files
  )
  if (.is_len_0(path_vec)) {
    return(path_vec)
  }
  path_vec <- path_vec |> .file_ls_rm_dir(path_dir, full.names)

  if (.is_len_0(path_vec)) {
    return(path_vec)
  }
  .dir_filter_removable(path_vec)
}

.file_ls_rm_dir <- function(fn, path_dir, full.names) {
  if (!full.names) {
    fn_full <- file.path(path_dir, fn)
  } else {
    fn_full <- fn
  }
  fn <- fn[!fs::is_dir(fn) & file.exists(fn_full)]
  fn |>
    fs::path_expand() |>
    fs::path_norm() |>
    as.character()
}

# --------------------
# filter
# ---------------------

.file_filter_exists <- function(fn) {
  fn[file.exists(fn)]
}

# moving
# ---------------------

.file_rm <- function(fn) {
  .assert_chr_min(fn, TRUE)
  fn <- fn |> .file_filter_exists()
  if (length(fn) == 0) {
    return(invisible(character()))
  }
  vapply(fn, function(x) do.call(file.remove, list(x)), logical(1))
  invisible(fn)
}

.file_rm_single <- function(fn) {
  .assert_string(fn, TRUE)
  if (!file.exists(fn)) {
    return(invisible(character()))
  }
  invisible(file.remove(fn))
  invisible(fn)
}

# ==============================
# directories
# ==============================

# ---------------------
# list
# ---------------------

# list all paths in a directory
.dir_ls <- function(path_dir,
                    recursive = TRUE,
                    full.names = FALSE) {
  .assert_string(path_dir, TRUE)
  .assert_dir_exists(path_dir, TRUE)
  .assert_flag(recursive, TRUE)
  .assert_flag(full.names, TRUE)
  list.dirs(
    path_dir,
    recursive = recursive, full.names = full.names
  ) |>
    fs::path_norm() |>
    as.character() |>
    .dir_filter_removable(path_dir)
}

# list paths to exclude
.dir_ls_unremovable <- function(path_dir = NULL) {
  .assert_chr(path_dir)
  c(
    ".", "..", .dir_proj_get(),
    dirname(.dir_proj_get()), path_dir
  ) |>
    .file_filter_dir() |>
    .file_filter_exists() |>
    fs::path_real() |>
    as.character() |>
    unique()
}

# ---------------------
# get
# ---------------------

.dir_proj_get <- function(...) {
  if (!requireNamespace("rprojroot", quietly = TRUE)) {
    utils::install.packages("rprojroot")
  }
  tryCatch(
    rprojroot::is_r_package$find_file(),
    error = function(e) getwd()
  ) |>
    .path_get_full(...)
}

# ---------------------
# create
# ---------------------

# create
.dir_create <- function(path_dir) {
  .assert_chr_min(path_dir, TRUE)
  for (i in seq_along(path_dir)) {
    .assert_path_not_file(path_dir[[i]])
    if (!dir.exists(path_dir[[i]])) {
      dir.create(path_dir[[i]], recursive = TRUE)
    }
  }
  invisible(path_dir)
}

# random directory in temp
.dir_create_tmp_random <- function() {
  path_dir <- .dir_get_tmp_random_path()
  while (dir.exists(path_dir)) {
    path_dir <- .dir_get_tmp_random_path()
  }
  .dir_create(path_dir)
  path_dir
}

.dir_get_tmp_random_path <- function() {
  file.path(
    tempdir(), "randomnia", signif(stats::rnorm(1), 6)
  )
}

# ---------------------
# filter
# ---------------------

.dir_filter_exists <- function(path_dir) {
  .file_filter_dir(path_dir)
}

.dir_filter_exists_single <- function(path_dir) {
  .assert_string(path_dir, TRUE)
  path_dir[dir.exists(path_dir)]
}

.dir_filter_removable <- function(path, path_dir_base = NULL) {
  .assert_chr_min(path, TRUE)
  .assert_string(path_dir_base)
  path[
    !fs::path_abs(path) %in% .dir_ls_unremovable(path_dir_base)
  ]
}

# ---------------------
# removing
# ---------------------

# remove directory itself
.dir_rm <- function(path) {
  .assert_chr_min(path, TRUE)
  for (i in seq_along(path)) {
    .assert_path_not_file(path[[i]])
    if (!dir.exists(path[[i]])) {
      next
    }
    unlink(path[[i]], recursive = TRUE)
  }
  invisible(TRUE)
}

.dir_rm_single <- function(path) {
  .assert_string(path, TRUE)
  .dir_rm(path)
}

# ---------------------
# clearing
# ---------------------

# remove directory contents
.dir_clear <- function(path_dir,
                       recursive_file = FALSE,
                       recursive_dir = FALSE,
                       delete_hidden = TRUE,
                       dir_exc = NULL) {
  .assert_string(path_dir, TRUE)
  .assert_flag(recursive_file, TRUE)
  .assert_flag(recursive_dir, TRUE)
  .assert_flag(delete_hidden, TRUE)
  if (!.dir_clear_check(path_dir)) {
    return(invisible(FALSE))
  }

  .dir_clear_file(
    path_dir,
    delete_hidden = delete_hidden, recursive = recursive_file,
    dir_exc = dir_exc
  )
  .dir_clear_dir(
    path_dir,
    recursive = recursive_dir, dir_exc = dir_exc
  )
  invisible(TRUE)
}

.dir_clear_check <- function(path_dir) {
  if (!dir.exists(path_dir)) {
    return(invisible(FALSE))
  }
  # nothing to do if it doesn't exist
  # don't do if it it's the project directory
  if (.dir_check_identical(path_dir)) {
    stop("Cannot clear the entire project", call. = FALSE)
  }
  invisible(TRUE)
}

.dir_check_identical <- function(path_dir_one, path_dir_two = NULL) {
  .assert_string(path_dir_one, TRUE)
  .assert_dir_exists(path_dir_one, TRUE)
  if (is.null(path_dir_two)) {
    path_dir_two <- .dir_proj_get()
  }
  .assert_dir_exists(path_dir_two, TRUE)
  identical(
    .file_ls(path_dir_one) |> sort(),
    .file_ls(path_dir_two) |> sort()
  )
}

.dir_clear_file <- function(path,
                            delete_hidden = TRUE,
                            recursive = TRUE,
                            dir_exc = NULL) {
  # delete all files within a directory
  if (!.dir_clear_check(path)) {
    return(invisible(FALSE))
  }
  .file_ls(path, recursive, all.files = delete_hidden, full.names = TRUE) |>
    .file_filter_dir_non() |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec_add_back_file(path, dir_exc) |>
    .file_rm()
  invisible(TRUE)
}

.dir_clear_dir <- function(path, recursive = FALSE, dir_exc = NULL) {
  .assert_string(path)
  if (!.dir_clear_check(path)) {
    return(invisible(FALSE))
  }
  path_vec_dir <- .dir_ls(path, recursive = recursive, full.names = TRUE) |>
    .dir_filter_removable() |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec_add_back_file(path, dir_exc)
  for (i in seq_along(path_vec_dir)) {
    unlink(path_vec_dir[i], recursive = TRUE)
  }
  invisible(TRUE)
}

# ---------------------
# moving and copying
# ---------------------

# make second directory exactly like first,
# whilst not removing contents from first
.dir_copy_exact <- function(path_dir_from,
                            path_dir_to,
                            dir_exc = NULL) {
  .dir_clear(path_dir_to)
  .dir_copy(path_dir_from, path_dir_to, dir_exc = dir_exc)
}

# make second directory exactly like first,
# whilst removing contents from first
.dir_move_exact <- function(path_dir_from,
                            path_dir_to,
                            dir_exc = NULL) {
  .dir_clear(path_dir_to)
  .dir_move(path_dir_from, path_dir_to, dir_exc = dir_exc)
}

# copy across, retaining relative paths
.dir_copy <- function(path_dir_from,
                      path_dir_to,
                      dir_exc = NULL) {
  .assert_string(path_dir_from, TRUE)
  .assert_string(path_dir_to, TRUE)
  .assert_chr(dir_exc)
  .file_ls(path_dir_from) |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec_add_back_file(path_dir_from, dir_exc) |>
    .dir_copy_file(path_dir_from, path_dir_to)
  # .dir_copy_tree(
  #   path_dir_from = path_dir_from, path_dir_to = path_dir_to
  # )
  invisible(TRUE)
}

.dir_copy_exact_file <- function(fn,
                                 path_dir_from,
                                 path_dir_to) {
  # mimick files while preserving their
  # relative paths within a particular directory
  .dir_clear(path_dir_to)
  .dir_copy_file(fn, path_dir_from, path_dir_to)
}

.dir_copy_file <- function(fn,
                           path_dir_from,
                           path_dir_to) {
  # relative paths within a particular directory
  if (!.dir_copy_file_check(fn, path_dir_from)) {
    return(invisible(FALSE))
  }
  # ensure relevant directories are available
  .dir_copy_file_tree(fn, path_dir_to)
  fs::file_copy(
    file.path(path_dir_from, fn) |> .path_force_abs(),
    file.path(path_dir_to, fn) |> .path_force_abs(),
    overwrite = TRUE
  )
  invisible(TRUE)
}

.dir_copy_file_check <- function(fn, path_dir_from) {
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .path_force_abs(fn, path_dir_from) |>
    .file_filter_exists() |>
    .is_len_pos()
}

.dir_copy_file_tree <- function(fn,
                                path_dir_to) {
  # copy the required directory tree across
  dirname(fn) |>
    setdiff(c(".", "..")) |>
    unique() |>
    vapply(
      function(x) file.path(path_dir_to, x), character(1)
    ) |>
    stats::setNames(NULL) |>
    .dir_filter_removable(path_dir_to) |>
    .dir_create()
  invisible(TRUE)
}

.dir_copy_tree <- function(path_dir_from, path_dir_to) {
  path_dir_from_vec_tree <- list.dirs(
    path_dir_from,
    recursive = TRUE, full.names = FALSE
  ) |>
    setdiff("")
  path_dir_to_vec_tree <- file.path(path_dir_to, path_dir_from_vec_tree)
  .dir_create(path_dir_to_vec_tree)
  for (i in seq_along(path_dir_to_vec_tree)) {
    .dir_create(path_dir_to_vec_tree[[i]])
  }
  invisible(TRUE)
}

.dir_move <- function(path_dir_from,
                      path_dir_to,
                      dir_exc = NULL) {
  .dir_move_file(
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to,
    dir_exc = dir_exc
  )
  .dir_move_dir(
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to,
    dir_exc = dir_exc
  )
}

.dir_move_file <- function(fn = NULL,
                           path_dir_from,
                           path_dir_to,
                           dir_exc = dir_exc) {
  .assert_string(path_dir_from)
  if (is.null(fn)) {
    fn <- .file_ls(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  } else {
    .assert_chr_min(fn)
    fn <- file.path(path_dir_from, fn) |>
      .file_filter_exists() |>
      .file_filter_dir_non() |>
      .path_force_rel(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  }
  if (.is_len_0(fn)) {
    return(invisible(FALSE))
  }
  .assert_string(path_dir_to, TRUE)
  .dir_copy_file_tree(fn, path_dir_to)
  .assert_string(path_dir_to)
  file.rename(
    fn |> .path_force_abs(path_dir_from),
    fn |> .path_force_abs(path_dir_to)
  ) |>
    invisible()
  invisible(TRUE)
}

.path_filter_spec_add_back_file <- function(fn, path_dir, path_exc) {
  if (is.null(path_exc)) {
    return(fn)
  }
  fn |>
    c(
      file.path(path_dir, path_exc) |> .file_filter_dir_non()
    )
}

.dir_move_dir <- function(path_dir = NULL,
                          path_dir_from,
                          path_dir_to,
                          dir_exc) {
  .assert_string(path_dir_from)
  if (is.null(path_dir)) {
    path_dir <- .dir_ls(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  } else {
    .assert_chr_min(path_dir)
    path_dir <- file.path(path_dir_from, path_dir) |>
      .file_filter_exists() |>
      .file_filter_dir() |>
      .path_force_rel(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  }
  if (.is_len_0(path_dir)) {
    return(invisible(FALSE))
  }
  suppressWarnings(file.rename(
    path_dir |> .path_force_abs(path_dir_from),
    path_dir |> .path_force_abs(path_dir_to)
  ) |>
    invisible())

  .dir_rm(
    file.path(path_dir_from, path_dir)[
      dir.exists(file.path(path_dir_from, path_dir))
    ]
  )
  invisible(TRUE)
}
