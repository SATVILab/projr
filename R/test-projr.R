#' @title Test renv restore
#'
#' @description
#' Tests renv restore without using the current project library.
#' Useful for testing renv restore in a clean environment
#' without using the cache.
#'
#' @param file character vector.
#' Additional files to copy across to the new directory
#' before restoring.
#' May be useful, for example, to copy across an `.Renviron` file
#' to ensure that certain environment variables (such
#' as authentication tokens) are set.
#'
#' @return
#' `TRUE` for successful restores and `FALSE` for failed restores.
#'
#' @details
#' Copies the renv.lock file and renv directory to a temporary directory,
#' along with .Rprofile and any files specified by the user.
#' Then runs renv::restore() on the temporary directory, without
#' using the first library used by the project to which
#' renv has been installing packages.
#'
#' @export
projr_test_renv <- function(file = NULL) {
  if (!is.null(file) && !is.character(file)) {
    stop("`file` must be a character vector or NULL.")
  }
  
  .projr_dep_install_only("renv")
  
  renv_lock_path <- .dir_proj_get("renv.lock")
  if (!file.exists(renv_lock_path)) {
    stop("No renv configuration detected in project.")
  }
  
  # Set up project (to be deleted afterwards as well)
  path_dir_test <- .projr_test_renv_dir_setup(file)
  on.exit(
    unlink(path_dir_test, recursive = TRUE),
    add = TRUE
  )
  
  # Get renv command text
  cmd_txt <- .projr_test_renv_cmd_get(path_dir_test)
  
  # Set up logging files
  path_vec_log <- .projr_test_renv_file_log_get()
  
  out <- system2(
    .projr_path_rscript_get(),
    args = c("-e", shQuote(cmd_txt)),
    stdout = path_vec_log[1],
    stderr = path_vec_log[2]
  )
  
  # Notify user of success or failure
  if (out == 0) {
    message("renv restore successful")
    message(paste0("For details, see log file at ", path_vec_log[1]))
    return(TRUE)
  } else {
    message(
      paste0(
        "renv restore failed. For details, see log file at ", path_vec_log[2]
      )
    )
    return(FALSE)
  }
}

.projr_test_renv_dir_setup <- function(file) {
  # Collect files to copy
  fn_vec <- c(
    "renv.lock",
    file,
    file.path("renv", list.files(.dir_proj_get("renv"), recursive = TRUE)),
    ".Rprofile"
  ) |> 
    .file_filter_exists()
  
  # Generate a unique temporary directory
  dir_test <- tempfile(pattern = "test_renv_", tmpdir = tempdir())
  dir.create(file.path(dir_test, "renv", "staging"), recursive = TRUE)
  
  # Copy files
  for (x in fn_vec) {
    source_path <- .dir_proj_get(x)
    dest_path <- file.path(dir_test, x)
    
    if (dir.exists(source_path)) {
      dir.create(dest_path, recursive = TRUE)
      success <- file.copy(from = source_path, to = dest_path, recursive = TRUE)
    } else {
      success <- file.copy(from = source_path, to = dest_path)
    }
    
    if (!success) {
      stop(paste("Failed to copy file:", x))
    }
  }
  
  dir_test
}

.projr_test_renv_cmd_get <- function(path_dir_test) {
  lib_paths <- c(file.path(path_dir_test, "renv_lib_check"), .libPaths()[-1])
  .dir_create(lib_paths)
  
  # Quote library paths
  lib_vec_quoted <- sapply(lib_paths, shQuote)
  
  # Collapse into a single string with commas
  paths_str <- paste(lib_vec_quoted, collapse = ", ")
  
  # Construct the renv::restore command
  cmd <- paste0("renv::restore(rebuild = TRUE, library = c(", paths_str, "))")
  return(cmd)
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

.projr_test_renv_file_log_prepare_path <- function(paths) {
  for (path in paths) {
    .dir_create(dirname(path))
    if (file.exists(path)) {
      file.remove(path)
    }
  }
  invisible(TRUE)
}
