path_filter_essential_non_get_essential <- function(path_dir) {
  fn_vec_rel <- c(
    ".", "..", .path_get(),
    dirname(.path_get()), path_dir
  )
  fn_vec_abs <- fn_vec_rel |> .path_force_abs()
  c(fn_vec_rel, fn_vec_abs) |>
    unique() |>
    fs::path_norm() |>
    as.character() |>
    unique()
}

.projr_dir_ls <- function(path_dir,
                          recursive = TRUE,
                          full.names = FALSE,
                          all.files = TRUE) {
  .assert_dir_exists(path_dir, TRUE)
  list.files(
    path_dir,
    recursive = recursive, full.names = full.names, all.files = all.files
  ) |>
    fs::path_norm() |>
    as.character() |>
    .projr_file_filter_essential_non(path_dir)
}

.projr_dir_clear_dir_ls <- function(path_dir, recursive) {
  list.dirs(
    path_dir,
    recursive = recursive, full.names = TRUE
  )
}
