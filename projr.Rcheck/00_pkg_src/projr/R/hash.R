.hash_dir <- function(path_dir, version = NULL, dir_exc = NULL) {
  .assert_given(path_dir)
  fn_vec <- .file_ls(path_dir)
  if (.is_len_0(fn_vec)) {
    return(.zero_tbl_get_hash())
  }
  # Filter before converting to absolute paths
  fn_vec <- .path_filter_spec(fn_vec, dir_exc)
  fn_vec <- file.path(path_dir, fn_vec)
  out_tbl <- data.frame(
    fn = fn_vec |> .path_force_rel(path_dir),
    version = (version %||% .version_get()) |>
      .version_v_add(),
    hash = .hash_file(fn_vec)
  )
  rownames(out_tbl) <- NULL
  out_tbl
}


.hash_file <- function(fn) {
  vapply(fn, .hash_file_single, character(1))
}

.hash_file_single <- function(fn) {
  digest::digest(fn, serialize = FALSE, file = TRUE)
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
