.hash_dir <- function(path_dir, version = NULL, dir_exc = NULL) {
  .assert_string(path_dir, TRUE)
  .assert_dir_exists(path_dir, TRUE)
  files <- .hash_dir_files(path_dir, dir_exc)
  if (!length(files)) {
    return(.zero_tbl_get_hash())
  }
  out_tbl <- data.frame(
    fn = files,
    version = .version_v_add(version %||% .version_get()),
    hash = .hash_file(file.path(path_dir, files))
  )
  rownames(out_tbl) <- NULL
  out_tbl
}

.hash_dir_files <- function(path_dir, dir_exc) {
  if (is.null(dir_exc)) {
    files <- list.files(path_dir, recursive = TRUE, all.files = TRUE)
  } else {
    # Skip excluded top-level trees (especially the build cache) before descending.
    entries <- list.files(path_dir, all.files = TRUE, no.. = TRUE)
    entries <- .path_filter_spec(entries, dir_exc)
    is_dir <- dir.exists(file.path(path_dir, entries))
    nested <- lapply(entries[is_dir], function(entry) {
      files <- list.files(file.path(path_dir, entry), recursive = TRUE, all.files = TRUE)
      if (length(files)) file.path(entry, files)
    })
    files <- sort(c(entries[!is_dir], unlist(nested, use.names = FALSE)))
    # Exclusions can also refer to a path beneath one of the retained directories.
    if (length(files)) files <- .path_filter_spec(files, dir_exc)
  }

  # Recursive list.files omits directories but can include dangling symlinks.
  files[file.exists(file.path(path_dir, files))]
}

.hash_file <- function(fn) {
  if (is.null(fn)) return(character())
  hashes <- tools::md5sum(fn)
  if (anyNA(hashes)) {
    stop("Could not hash file: ", paste(fn[is.na(hashes)], collapse = ", "), call. = FALSE)
  }
  names(hashes) <- names(fn) %||% fn
  hashes
}

.zero_tbl_get_hash <- function() {
  out_df <- data.frame(
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}
