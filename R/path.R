# ========================
# paths
# ========================

# ------------------
# filter
# ------------------

.path_filter_spec <- function(fn, exc = NULL) {
  # filter based on a character vector of
  # paths to exclude

  # Validate fn input - allow empty vectors
  .assert_chr_min(fn, required = TRUE)

  if (is.null(exc)) {
    return(fn)
  }
  .assert_chr(exc, required = TRUE)

  .cli_debug("        Filtering {length(fn)} paths with exclusions: {paste(exc, collapse = ', ')}")

  pattern <- paste0(
    paste0("^", gsub("/+$|^\\^", "", exc), "/", collapse = "|"),
    "|",
    paste0("^", gsub("/+$|^\\^", "", exc), "$", collapse = "|")
  )
  result <- fn[!grepl(pattern, fn)]

  .cli_debug("        After filtering: {length(result)} paths remain")
  result
}

# -------------------
# transform
# -------------------

# relative
.path_force_rel <- function(path, path_dir = NULL) {
  # Validate inputs - allow empty character vectors
  .assert_chr_min(path, required = TRUE)
  .assert_string(path_dir)

  # Handle empty path
  if (length(path) == 0) {
    return(character(0))
  }

  path |>
    fs::path_rel(path_dir %||% .path_get()) |>
    as.character()
}

# absolute
.path_force_abs <- function(fn, path_dir = NULL) {
  .path_force_abs_check(fn = fn, path_dir = path_dir)
  if (all(fs::is_absolute_path(fn))) {
    return(fn)
  }
  if (is.null(path_dir)) {
    path_dir <- .path_get()
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

  # Validate path_append if not empty
  if (length(path_append) > 0) {
    .assert_chr(path_append, required = TRUE)
  }

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
  # Validate input - allow empty vectors
  .assert_chr_min(x, required = TRUE)
  x[fs::is_dir(x)]
}

.filter_filter_non_na <- function(x) {
  # Validate input - can be any vector type
  if (length(x) == 0) {
    return(x)
  }
  x[!is.na(x)]
}

.file_filter_dir_non <- function(x) {
  # fs::is_dir returns FALSE for
  # files that do not exist, so ensuring that
  # the files exist makes this match file_filter_dir
  # better

  # Validate input - allow empty vectors
  .assert_chr_min(x, required = TRUE)

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

  .cli_debug("      Starting .file_ls() on {path_dir}")

  path_vec <- list.files(
    path_dir,
    recursive = recursive, full.names = full.names, all.files = all.files
  )

  .cli_debug("        Found {length(path_vec)} entries before filtering")

  if (.is_len_0(path_vec)) {
    .cli_debug("      Finished .file_ls() - no files")
    return(path_vec)
  }
  path_vec <- path_vec |> .file_ls_rm_dir(path_dir, full.names)

  if (.is_len_0(path_vec)) {
    .cli_debug("      Finished .file_ls() - no files after dir removal")
    return(path_vec)
  }

  result <- .dir_filter_removable(path_vec)
  .cli_debug("      Finished .file_ls() - {length(result)} files")
  result
}

.file_ls_rm_dir <- function(fn, path_dir, full.names) {
  # Validate inputs - allow empty fn vector
  .assert_chr_min(fn, required = TRUE)
  .assert_string(path_dir, required = TRUE)
  .assert_flag(full.names, required = TRUE)

  if (!full.names) {
    fn_full <- file.path(path_dir, fn)
  } else {
    fn_full <- fn
  }
  fn <- fn[!fs::is_dir(fn_full) & file.exists(fn_full)]
  fn |>
    fs::path_expand() |>
    fs::path_norm() |>
    as.character()
}

# --------------------
# filter
# ---------------------

.file_filter_exists <- function(fn) {
  # Validate input - allow empty vectors
  .assert_chr_min(fn, required = TRUE)
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

  .cli_debug("      Starting .dir_ls() on {path_dir}")

  result <- list.dirs(
    path_dir,
    recursive = recursive, full.names = full.names
  ) |>
    fs::path_norm() |>
    as.character() |>
    .dir_filter_removable(path_dir)

  .cli_debug("      Finished .dir_ls() - {length(result)} directories")
  result
}

# list paths to exclude
.dir_ls_unremovable <- function(path_dir = NULL) {
  # Validate path_dir (can be NULL or character)
  if (!is.null(path_dir)) {
    .assert_chr(path_dir, required = TRUE)
  }

  c(
    ".", "..", .path_get(),
    dirname(.path_get()), path_dir
  ) |>
    .file_filter_dir() |>
    .file_filter_exists() |>
    fs::path_real() |>
    fs::path_abs() |>
    as.character() |>
    unique()
}

# ---------------------
# get
# ---------------------

.path_get <- function(..., relative = FALSE) {
  # Validate relative flag
  .assert_flag(relative, required = TRUE)

  path_init <- tryCatch(
    rprojroot::find_root_file(
      ...,
      criterion = rprojroot::criteria$is_vcs_root ||
        rprojroot::has_file("VERSION") ||
        rprojroot::has_file("README.md") ||
        rprojroot::criteria$is_r_package ||
        rprojroot::criteria$is_rstudio_project ||
        rprojroot::criteria$is_renv_project ||
        rprojroot::criteria$is_quarto_project
    ),
    error = function(e) file.path(getwd(), ...)
  )
  if (!relative) {
    return(path_init)
  }
  fs::path_rel(path_init, .path_get())
}

# ---------------------
# create
# ---------------------

# create
.dir_create <- function(path_dir) {
  .assert_chr_min(path_dir, TRUE)
  path_dir <- unique(path_dir)
  if (.is_len_0(path_dir)) {
    return(invisible(character()))
  }
  path_dir <- .dir_filter_exists_non(path_dir)
  for (i in seq_along(path_dir)) {
    .assert_path_not_file(path_dir[[i]])
    dir.create(path_dir[[i]], recursive = TRUE)
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
    tempdir(), "randomnia", signif(abs(stats::rnorm(1)), 6)
  )
}

# ---------------------
# filter
# ---------------------

.dir_filter_exists <- function(path_dir) {
  # Validate input - allow empty vectors
  .assert_chr_min(path_dir, required = TRUE)
  .file_filter_dir(path_dir)
}

.dir_filter_exists_non <- function(path_dir) {
  # Validate input - allow empty vectors
  .assert_chr_min(path_dir, required = TRUE)
  path_dir[!dir.exists(path_dir)]
}


.dir_filter_exists_single <- function(path_dir) {
  .assert_string(path_dir, TRUE)
  path_dir[dir.exists(path_dir)]
}

.dir_filter_removable <- function(path, path_dir_base = NULL) {
  .assert_chr_min(path, TRUE)
  if (!is.null(path_dir_base)) {
    .assert_string(path_dir_base)
  }

  # Get unremovable directories (already normalized with path_real)
  unremovable <- .dir_ls_unremovable(path_dir_base)

  # For each path, check if it's removable
  # Use path_real for existing paths to match the normalization in .dir_ls_unremovable
  # Use path_abs for non-existing paths (can't use path_real on non-existing paths)
  is_removable <- vapply(path, function(p) {
    if (file.exists(p)) {
      # For existing paths, normalize the same way as .dir_ls_unremovable
      normalized <- as.character(fs::path_real(fs::path_abs(p)))
    } else {
      # For non-existing paths, just use path_abs
      normalized <- as.character(fs::path_abs(p))
    }
    !normalized %in% unremovable
  }, logical(1))

  path[is_removable]
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

  .cli_debug("  Starting .dir_clear()")
  .cli_debug("    path_dir: {path_dir}")

  if (!.dir_clear_check(path_dir)) {
    .cli_debug("    Directory check failed, returning FALSE")
    .cli_debug("  Finished .dir_clear()")
    return(invisible(FALSE))
  }

  .cli_debug("    Clearing files...")
  .dir_clear_file(
    path_dir,
    delete_hidden = delete_hidden, recursive = recursive_file,
    dir_exc = dir_exc
  )
  .cli_debug("    Clearing directories...")
  .dir_clear_dir(
    path_dir,
    recursive = recursive_dir, dir_exc = dir_exc
  )
  .cli_debug("  Finished .dir_clear()")
  invisible(TRUE)
}

.dir_clear_check <- function(path_dir) {
  # Validate input
  .assert_string(path_dir, required = TRUE)

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
    path_dir_two <- .path_get()
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

  # Validate inputs
  .assert_string(path, required = TRUE)
  .assert_flag(delete_hidden, required = TRUE)
  .assert_flag(recursive, required = TRUE)
  if (!is.null(dir_exc)) {
    .assert_chr(dir_exc, required = TRUE)
  }

  if (!.dir_clear_check(path)) {
    return(invisible(FALSE))
  }

  # Get files with full paths
  files_full <- .file_ls(path, recursive, all.files = delete_hidden, full.names = TRUE) |>
    .file_filter_dir_non()

  # Convert to relative paths for filtering (dir_exc expects relative paths)
  files_rel <- .path_force_rel(files_full, path)

  # Filter using relative paths
  files_rel_filtered <- files_rel |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec_add_back_file(path, dir_exc)

  # Convert back to full paths for removal
  files_full_filtered <- file.path(path, files_rel_filtered)

  # Remove files
  .file_rm(files_full_filtered)
  invisible(TRUE)
}

.dir_clear_dir <- function(path, recursive = FALSE, dir_exc = NULL) {
  # Validate inputs
  .assert_string(path, required = TRUE)
  .assert_flag(recursive, required = TRUE)
  if (!is.null(dir_exc)) {
    .assert_chr(dir_exc, required = TRUE)
  }

  if (!.dir_clear_check(path)) {
    return(invisible(FALSE))
  }

  # Get directories with full paths
  dirs_full <- .dir_ls(path, recursive = recursive, full.names = TRUE) |>
    .dir_filter_removable()

  # Convert to relative paths for filtering (dir_exc expects relative paths)
  dirs_rel <- .path_force_rel(dirs_full, path)

  # Filter using relative paths
  dirs_rel_filtered <- dirs_rel |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec_add_back_file(path, dir_exc)

  # Convert back to full paths for removal
  dirs_full_filtered <- file.path(path, dirs_rel_filtered)

  # Remove directories
  for (i in seq_along(dirs_full_filtered)) {
    unlink(dirs_full_filtered[i], recursive = TRUE)
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
                            dir_exc = NULL,
                            fn_exc = NULL) {
  .cli_debug("Starting .dir_move_exact()")
  .cli_debug("  path_dir_from: {path_dir_from}")
  .cli_debug("  path_dir_to: {path_dir_to}")
  .cli_debug("  dir_exc: {paste(dir_exc, collapse = ', ')}")
  .cli_debug("  fn_exc: {paste(fn_exc, collapse = ', ')}")

  # Clear destination directory first
  .cli_debug("  Clearing destination directory...")
  .dir_clear(path_dir_to)

  # Moving files and directories
  .dir_move(
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to,
    dir_exc = dir_exc,
    fn_exc = fn_exc
  )
}

# copy across, retaining relative paths
.dir_copy <- function(path_dir_from,
                      path_dir_to,
                      dir_exc = NULL,
                      fn_exc = NULL) {
  .assert_string(path_dir_from, TRUE)
  .assert_string(path_dir_to, TRUE)
  .assert_chr(dir_exc)
  .assert_chr(fn_exc)
  .file_ls(path_dir_from) |>
    .path_filter_spec(dir_exc) |>
    .path_filter_spec(fn_exc) |>
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

  # Validate inputs - allow empty fn vector
  .assert_chr_min(fn, required = TRUE)
  .assert_string(path_dir_from, required = TRUE)
  .assert_string(path_dir_to, required = TRUE)

  if (!.dir_copy_file_check(fn, path_dir_from)) {
    return(invisible(FALSE))
  }
  .dir_create(path_dir_to)
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
  # Validate inputs - allow empty fn vector
  .assert_chr_min(fn, required = TRUE)
  .assert_string(path_dir_from, required = TRUE)

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

  # Validate inputs - allow empty fn vector
  .assert_chr_min(fn, required = TRUE)
  .assert_string(path_dir_to, required = TRUE)

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
                      dir_exc = NULL,
                      fn_exc = NULL) {
  .cli_debug("  Starting .dir_move()")
  .cli_debug("    path_dir_from: {path_dir_from}")
  .cli_debug("    path_dir_to: {path_dir_to}")

  if (is.null(dir_exc) && is.null(fn_exc)) {
    .cli_debug("    No exclusions provided, using optimized move")
    res <- .dir_move_no_exc(
      path_dir_from = path_dir_from,
      path_dir_to = path_dir_to
    )
    if (isTRUE(res)) {
      .cli_debug("    Optimized move succeeded, finishing .dir_move()")
      return(invisible(TRUE))
    } else {
      .cli_debug("    Optimized move failed, falling back to standard move")
    }
  }
  
  .cli_debug("    Moving files...")
  .dir_move_file(
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to,
    dir_exc = dir_exc,
    fn_exc = fn_exc
  )
  .cli_debug("    Moving directories...")
  .dir_move_dir(
    path_dir_from = path_dir_from,
    path_dir_to = path_dir_to,
    dir_exc = dir_exc
  )
}

.dir_move_no_exc <- function(path_dir_from,
                             path_dir_to) {
  .cli_debug("  Starting .dir_move_no_exc()")
  .cli_debug("    path_dir_from: {path_dir_from}")
  .cli_debug("    path_dir_to: {path_dir_to}")

  # Ensure destination parent exists for the rename attempt
  

  if (.is_same_filesystem(path_dir_from, path_dir_to)) {
    .cli_debug("    Same filesystem detected, trying fs::file_move()")
    .dir_create(dirname(path_dir_to))
    res <- tryCatch({
      fs::file_move(path_dir_from, path_dir_to)
      invisible(TRUE)
      },
      error = function(e) {q
        .cli_debug("    fs::file_move() failed with error: {e$message}")
        invisible(FALSE)
      }
    )
    if (isTRUE(res)) {
      .cli_debug("    fs::file_move() succeeded")
      return(invisible(TRUE))
    }
    .cli_debug("    fs::file_move() failed; falling back")
  } else {
    .cli_debug("    Cross-filesystem or unknown volume; using fallback")
  }
  invisible(FALSE)
}

.dir_move_file <- function(fn = NULL,
                           path_dir_from,
                           path_dir_to,
                           dir_exc = NULL,
                           fn_exc = NULL) {
  .assert_string(path_dir_from)
  .cli_debug("    Starting .dir_move_file()")

  if (is.null(fn)) {
    .cli_debug("      Listing files from source directory...")
    fn <- .file_ls(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec(fn_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  } else {
    .assert_chr_min(fn)
    .cli_debug("      Filtering provided files...")
    fn <- file.path(path_dir_from, fn) |>
      .file_filter_exists() |>
      .file_filter_dir_non() |>
      .path_force_rel(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec(fn_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  }

  .cli_debug("      Found {length(fn)} files to move")

  if (.is_len_0(fn)) {
    .cli_debug("      No files to move")
    .cli_debug("    Finished .dir_move_file()")
    return(invisible(FALSE))
  }

  .assert_string(path_dir_to, TRUE)
  .cli_debug("      Creating directory tree...")
  .dir_copy_file_tree(fn, path_dir_to)
  .assert_string(path_dir_to)
  .cli_debug("      Renaming files...")
  fs::file_move(
    fn |> .path_force_abs(path_dir_from),
    fn |> .path_force_abs(path_dir_to)
  ) |>
    invisible()
  .cli_debug("    Finished .dir_move_file()")
  invisible(TRUE)
}

.path_filter_spec_add_back_file <- function(fn, path_dir, path_exc) {
  # Validate inputs - allow empty fn vector
  .assert_chr_min(fn, required = TRUE)
  .assert_string(path_dir, required = TRUE)

  if (is.null(path_exc)) {
    return(fn)
  }

  .assert_chr(path_exc, required = TRUE)

  .cli_debug("        Checking for excluded paths that are files...")

  # Check if the excluded path exists as a non-directory file
  exc_path <- file.path(path_dir, path_exc)
  if (length(.file_filter_dir_non(exc_path)) > 0) {
    # Return relative path, not absolute
    .cli_debug("        Adding back {length(path_exc)} excluded file(s)")
    fn <- c(fn, path_exc)
  }
  fn
}

.dir_move_dir <- function(path_dir = NULL,
                          path_dir_from,
                          path_dir_to,
                          dir_exc) {
  .assert_string(path_dir_from)
  .cli_debug("    Starting .dir_move_dir()")

  if (is.null(path_dir)) {
    .cli_debug("      Listing directories from source...")
    path_dir <- .dir_ls(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  } else {
    .assert_chr_min(path_dir)
    .cli_debug("      Filtering provided directories...")
    path_dir <- file.path(path_dir_from, path_dir) |>
      .file_filter_exists() |>
      .file_filter_dir() |>
      .path_force_rel(path_dir_from) |>
      .path_filter_spec(dir_exc) |>
      .path_filter_spec_add_back_file(path_dir_from, dir_exc)
  }

  .cli_debug("      Found {length(path_dir)} directories to move")

  if (.is_len_0(path_dir)) {
    .cli_debug("      No directories to move")
    .cli_debug("    Finished .dir_move_dir()")
    return(invisible(FALSE))
  }

  path_vec_to <- path_dir |> .path_force_abs(path_dir_to)
  path_vec_to_exists_non_ind <- !dir.exists(path_vec_to)

  if (sum(path_vec_to_exists_non_ind) == 0L) {
    .cli_debug("      All directories to copy already exist")
  } else {
    .cli_debug("      Creating missing directories in destination...")
    dir.create(
      path_vec_to[path_vec_to_exists_non_ind],
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  .cli_debug("      Removing remaining directories from source...")
  unlink(
    file.path(path_dir_from, path_dir),
    recursive = TRUE
  ) |>
    invisible()
}

.is_same_filesystem <- function(path1, path2, follow_symlinks = TRUE) {
  stopifnot(requireNamespace("fs", quietly = TRUE))

  # Prefer "follow = TRUE" if you care
  # about the target rather than the symlink itself
  i1 <- fs::file_info(path1, follow = follow_symlinks, fail = TRUE)
  i2 <- fs::file_info(path2, follow = follow_symlinks, fail = TRUE)

  # Same filesystem ⇔ same device id
  isTRUE(i1$device_id == i2$device_id)
}