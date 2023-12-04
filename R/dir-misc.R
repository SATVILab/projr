.projr_dir_clear <- function(path_dir,
                             delete_directories = TRUE,
                             delete_hidden = TRUE) {
  if (!file.exists(path_dir)) {
    return(invisible(TRUE))
  }
  if (FALSE) {
    if (dir.exists(file.path(path_dir, ".git"))) {
      stop("Attempting to remove a .git folder")
    }
    warning("path_dir")
    warning(path_dir)
  }
  path_dir <- path_dir |>
    fs::path_norm() |>
    as.character()
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

.projr_dir_label_strip <- function(x) {
  gsub("_", "", gsub("-", "", x)) |>
    tolower()
}
