# Document this function:
#' @title Test renv restore
#'
#' @description
#' Tests renv restore without using the current project library.
#' Useful for testing renv restore in a clean environment
#' without using the cache.
#'
#' @param file character.
#' Additional files to copy across to the new directory
#' before restoring.
#' May be useful, for example, to copy across an `.Renviron` file
#' to ensure that certain environment variables (such
#' as authentication tokens) are set.
#'
#' @return
#' `TRUE`` for successful restores and `FALSE`` for failed restores.
#'
#' @details
#' Copies the renv.lock file and renv directory to a temporary directory,
#' along with .Rprofile and any files specified by the user.
#' Then runs renv::restore() on the temporary directory, without
#' using the first library used by the project to which
#' renv has been installing packages.
projr_test_renv <- function(file = NULL) {
  # set up project (to be deleted afterwards as well)
  path_dir_test <- .projr_test_renv_dir_setup(file)
  on.exit(
    unlink(path_dir_test, recursive = TRUE),
    add = TRUE, after = TRUE
  )
  # get renv command text
  cmd_txt <- .projr_test_renv_cmd_get(path_dir_test)
  # set up logging files
  path_vec_log <- .projr_test_renv_file_log_get()
  out <- suppressWarnings(system2(
    .projr_path_rscript_get(),
    args = cmd_txt, stdout = path_vec_log[1], stderr = path_vec_log[2]
  ))
  # notify user of success or failure
  if (x == 0) {
    print("renv restore successful")
    return(invisible(TRUE))
  }
  stop("renv restore failed")
  return(invisible(FALSE))
}

.projr_test_renv_dir_setup <- function(file) {
  # may need to add in stuff about setting up environment
  # variables automatically
  fn_vec <- c(
    "renv.lock",
    file,
    file.path("renv", list.files(.dir_proj_get("renv"), recursive = TRUE)),
    ".Rprofile"
  ) |>
    .file_filter_exists()
  dir_test <- file.path(
    tempdir(), "test_renv", "renv", signif(abs(stats::rnorm(1)), 5)
  )
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  dir.create(file.path(dir_test, "renv", "staging"), recursive = TRUE)
  for (x in fn_vec) {
    file.copy(from = .dir_proj_get(x), to = file.path(dir_test, x))
  }
  dir_test
}
.projr_test_renv_cmd_get <- function(path_dir_test) {
  lib_paths <- c(file.path(path_dir_test, "renv_lib_check"), .libPaths()[-1])
  .dir_create(lib_paths)

  # Convert the library paths to a string that can be used in the Rscript command
  lib_paths_str <- paste(sapply(lib_paths, function(x) paste0("\"", x, "\"")), collapse = ", ")

  # Create the Rscript command
  cmd_txt <- sprintf("-e 'renv::restore(rebuild = TRUE, library = c(%s))'", lib_paths_str)

  return(cmd_txt)
}

.projr_test_renv_file_log_get <- function() {
  path_vec <- .projr_test_renv_file_log_get_path()
  .projr_test_renv_file_log_prepare_path(path_vec)
  path_vec
}

.projr_test_renv_file_log_get_path <- function() {
  tryCatch(
    c(
      projr_path_get_dir("cache", "projr", "log-renv_restore-output.txt"),
      projr_path_get_dir("cache", "projr", "log-renv_restore-error.txt")
    ),
    error = function(e) {
      c(
        .dir_proj_get("_tmp", "projr", "log-renv_restore-output.txt"),
        .dir_proj_get("_tmp", "projr", "log-renv_restore-error.txt")
      )
    }
  )
}

.projr_test_renv_file_log_prepare_path <- function(path) {
  for (i in seq_along(path)) {
    .dir_create(dirname(path[[i]]))
    if (file.exists(path[[i]])) {
      file.remove(path[[i]])
    }
  }
  invisible(TRUE)
}
