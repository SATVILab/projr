.projr_hash_dir <- function(path_dir, version = NULL, dir_exc = NULL) {
  .assert_given(path_dir, "path_dir")
  fn_vec <- .projr_dir_ls(path_dir)
  if (.is_len_0(fn_vec)) {
    return(.projr_zero_tbl_get_hash())
  }
  fn_vec <- fn_vec |>
    .projr_file_dir_exc(dir_exc) |>
    .projr_file_get_full(path_dir = path_dir)
  out_tbl <- data.frame(
    fn = .projr_file_get_rel(fn_vec, path_dir),
    version = paste0("v", version %||% projr_version_get()),
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
