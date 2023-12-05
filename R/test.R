projr_test_renv <- function() {
  # set up project (to be deleted afterwards as well)
  path_dir_test <- .projr_test_renv_dir_setup()
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
  } else {
    stop("renv restore failed")
  }
  invisible(TRUE)
}

.projr_test_renv_dir_setup <- function() {
  # may need to add in stuff about setting up environment
  # variables automatically
  fn_vec <- c(
    "renv.lock",
    file.path("renv", list.files(.projr_dir_proj_get("renv"), recursive = TRUE)),
    ".Rprofile"
  )
  dir_test <- file.path(
    tempdir(), "test_renv", "renv", signif(abs(rnorm(1)), 5)
  )
  if (dir.exists(dir_test)) {
    unlink(dir_test, recursive = TRUE)
  }
  dir.create(file.path(dir_test, "renv", "staging"), recursive = TRUE)
  for (x in fn_vec) {
    file.copy(from = .projr_dir_proj_get(x), to = file.path(dir_test, x))
  }
  dir_test
}
.projr_test_renv_cmd_get <- function(path_dir_test) {
  lib_paths <- c(file.path(path_dir_test, "renv_lib_check"), .libPaths()[-1])
  for (x in lib_paths) {
    if (!dir.exists(x)) {
      dir.create(x, recursive = TRUE)
    }
  }

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
      projr_dir_get("cache", "projr", "log-renv_restore-output.txt"),
      projr_dir_get("cache", "projr", "log-renv_restore-error.txt")
    ),
    error = function(e) {
      c(
        .projr_dir_proj_get("_tmp", "projr", "log-renv_restore-output.txt"),
        .projr_dir_proj_get("_tmp", "projr", "log-renv_restore-error.txt")
      )
    }
  )
}

.projr_test_renv_file_log_prepare_path <- function(path) {
  for (i in seq_along(path)) {
    if (!dir.exists(dirname(path[[i]]))) {
      dir.create(dirname(path[[i]]), recursive = TRUE)
    }
    if (file.exists(path[[i]])) {
      file.remove(path[[i]])
    }
  }
  invisible(TRUE)
}
