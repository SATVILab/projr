.projr_hash_dir <- function(path_dir, version = NULL, dir_exc = NULL) {
  .assert_given(path_dir)
  fn_vec <- .file_ls(path_dir)
  if (.is_len_0(fn_vec)) {
    return(.projr_zero_tbl_get_hash())
  }
  fn_vec <- file.path(path_dir, fn_vec) |>
    .path_filter_spec(dir_exc)
  out_tbl <- data.frame(
    fn = fn_vec |> .path_force_rel(path_dir),
    version = (version %||% projr_version_get()) |>
      .projr_version_v_add(),
    hash = .projr_hash_file(fn_vec)
  )
  rownames(out_tbl) <- NULL
  out_tbl
}


.projr_hash_file <- function(fn) {
  vapply(fn, .projr_hash_file_single, character(1))
}

.projr_hash_file_single <- function(fn) {
  digest::digest(fn, serialize = FALSE, file = TRUE)
}

.projr_zero_tbl_get_hash <- function() {
  out_df <- data.frame(
    fn = character(0),
    version = character(0),
    hash = character(0)
  )
  rownames(out_df) <- NULL
  out_df
}
