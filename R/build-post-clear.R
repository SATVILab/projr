.build_clear_post_safe <- function(output_run, clear_output, engines = .engine_get_auto(), rendered = character()) {
  # clear safe directories before moving new docs and outputs into them
  # move rmd and qmd's (not bookdown or quarto projects)
  # to the final docs directories
  .build_clear_post_safe_docs(output_run, engines, rendered)

  # clear final directories, in preparation
  # for the next build
  if (!.build_clear_post_safe_check(clear_output)) {
    return(invisible(FALSE))
  }

  label_vec <- .yml_dir_get_label_output(NULL)

  for (x in label_vec) {
    path_to_clear <- projr_path_get_dir(x, safe = FALSE, create = FALSE)
    .cli_debug("Clearing {path_to_clear}")
    .dir_clear(path_to_clear)
  }

  invisible(TRUE)
}

.build_clear_post_safe_docs <- function(output_run, engines = .engine_get_auto(), rendered = character()) {
  # always clear docs for output directories
  # if not quarto projects or bookdown projects.
  # don't clear quarto projects or bookdown projects
  # as we save directly there.
  if (output_run && "docs" %in% names(.yml_dir_get(NULL)) && .build_clear_post_safe_check_docs(engines)) {
    path_to_clear <- projr_path_get_dir("docs", safe = FALSE, create = FALSE)
    # A native renderer can write directly into the final docs directory.
    if (any(fs::path_has_parent(rendered[file.exists(rendered)], path_to_clear))) {
      return(invisible(FALSE))
    }

    .cli_debug("Clearing {path_to_clear}")
    .dir_clear(path_to_clear)
  } else {
    invisible(FALSE)
  }
}

.build_clear_post_safe_check <- function(clear_output) {
  # only clear final directories if
  # we are clearing after build (i.e. we
  # cleared "conservatively") in an output_run (i.e. if we
  # are going to copy across )
  clear_output == "post"
}

.build_clear_post_safe_check_docs <- function(engines = .engine_get_auto()) {
  # allow the exception that the quarto project
  # and bookdown folders are not cleared
  any(engines %in% c("quarto_document", "rmd", "document_output")) &&
    !any(engines %in% c("quarto_project", "bookdown"))
}

# clear old temporary output directories
# ---------------------------------------
.build_clear_old <- function(output_run, old_dev_remove) {
  if (!.build_clear_old_check(old_dev_remove)) {
    return(invisible(FALSE))
  }
  switch(as.character(output_run),
    "FALSE" = .build_clear_old_dev(),
    "TRUE" = .build_clear_old_output()
  )
}

.build_clear_old_check <- function(old_dev_remove) {
  invisible(old_dev_remove)
}

.build_clear_old_dev <- function() {
  current <- .dir_get_cache_auto_version(profile = NULL)
  path_dir <- dirname(current)
  if (!dir.exists(path_dir) || !nzchar(path_dir)) {
    return(invisible(FALSE))
  }
  path_dir_vec <- path_dir |>
    .dir_ls(recursive = FALSE) |>
    setdiff(c(basename(current), "log"))
  if (.is_len_0(path_dir_vec)) {
    return(invisible(FALSE))
  }
  # Convert relative paths to full paths
  path_dir_vec_full <- file.path(path_dir, path_dir_vec)
  .cli_debug("Removing {length(path_dir_vec_full)} previous development build directories")
  .dir_rm(path_dir_vec_full)
  invisible(TRUE)
}

.build_clear_old_output <- function() {
  path_dir <- .dir_get_cache_auto_version(profile = NULL) |>
    dirname()
  if (!dir.exists(path_dir) || !nzchar(path_dir)) {
    return(invisible(FALSE))
  }
  dir_vec <- path_dir |>
    .dir_ls(recursive = FALSE) |>
    setdiff("log")
  if (!length(dir_vec)) return(invisible(TRUE))
  .cli_debug("Removing {length(dir_vec)} build cache directories")
  unlink(file.path(path_dir, dir_vec), recursive = TRUE, force = TRUE, expand = FALSE)
  invisible(TRUE)
}
