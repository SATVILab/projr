
.save_to_dir_output <- function(dir_output, dir_archive, dir_book) {

  # save book to output and copy to archive
  path_zip <- file.path(dir_output, dir_book)

  if (file.exists(path_zip)) unlink(path_zip, recursive = TRUE)
  fn_vec_output <- list.files(
    dir_book,
    recursive = TRUE,
    full.names = TRUE
  )
  zip(
    path_zip,
    files = list.files(
      dir_output,
      recursive = TRUE,
      full.names = TRUE
    )
  )

  # save built package to output and copy to archive
  if (yml_projr$build_pkg && bump_component != "dev") {
    devtools::build(
      pkg = rprojroot::is_r_package$find_file(),
      path = dir_output,
      binary = FALSE
    )
    file.copy(
      file.path(dir_output, paste0(basename(dir_book), ".tar.gz")),
      to = file.path(dir_archive, paste0(basename(dir_book), ".tar.gz"))
    )
  }

  invisible(TRUE)
}
