.projr_file_get_abs_exists <- function(fn, path_dir = NULL) {
  .projr_file_get_abs(fn, path_dir) |>
    .projr_file_filter_exists()
}

.projr_file_get_abs <- function(fn, path_dir = NULL) {
  .projr_file_get_abs_check(fn = fn, path_dir = path_dir)
  if (all(.is_path_abs(fn))) {
    return(fn)
  }
  if (is.null(path_dir)) {
    path_dir <- .projr_dir_proj_get()
  }
  .projr_file_get_full(path_dir, fn) |>
    vapply(.projr_file_get_abs_single, character(1))
}

.projr_file_get_abs_single <- function(x) {
  .assert_string(x)
  fs::path_abs(x) |>
    as.character()
}

.projr_file_get_abs_check <- function(fn, path_dir) {
  .assert_chr(fn, TRUE)
  .assert_string(path_dir)
  if (all(.is_path_abs(fn)) && !is.null(path_dir)) {
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

.projr_file_get_full <- function(path_dir, fn) {
  file.path(path_dir, fn)
}

.projr_file_get_full_dots <- function(path_dir, ...) {
  path_append <- list(...) |> unlist()
  do.call(
    "file.path",
    args = list(path_dir) |> append(path_append)
  ) |>
    as.character()
}

.projr_file_get_full_exists <- function(path_dir, fn) {
  .projr_file_get_full(path_dir, fn) |>
    .projr_file_state_exists()
}

.projr_file_rm <- function(fn) {
  vapply(fn, .projr_file_rm_single, logical(1))
  invisible(fn)
}

.projr_file_rm_single <- function(fn) {
  if (!file.exists(fn)) {
    return(invisible(FALSE))
  }
  invisible(file.remove(fn))
}
.projr_file_get_rel <- function(path, path_dir = NULL) {
  path |>
    fs::path_rel(.projr_path_get_default(path_dir)) |>
    as.character()
}

.projr_path_get_default <- function(path) {
  path %||% .projr_dir_proj_get()
}

.projr_file_append_version <- function(path) {
  file.path(path, paste0("v", projr_version_get()))
}

.projr_file_exclude_essential <- function(fn_vec) {
  fn_vec |>
    fs::path_norm() |>
    as.character() |>
    setdiff(.projr_dir_ls_get_exc_auto())
}

.projr_file_dir_exc <- function(fn, exc) {
  if (is.null(exc)) {
    return(fn)
  }
  fn[
    !grepl(paste0("^", gsub("/+$", "", exc), "/", collapse = "|"), fn)
  ]
}
