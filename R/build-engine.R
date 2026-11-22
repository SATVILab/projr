.build_engine <- function(scripts,
                          version_run_on_list,
                          args_engine,
                          engine = .engine_get(scripts),
                          collect_paths = FALSE) {
  .cli_info("Using rendering engine: {engine}")

  # Handle case where no documents are found
  if (identical(engine, character(1L))) {
    build_error <- paste0(
      "No Quarto or RMarkdown documents found in the project directory. ",
      "Please create either a .qmd or .Rmd file, or specify the file path explicitly."
    )
  } else {
    build_error <- switch(engine,
      "bookdown" = .build_engine_bookdown(args_engine),
      "quarto_project" = .build_engine_quarto_project(args_engine),
      "quarto_document" = .build_engine_qmd(scripts, args_engine),
      "rmd" = .build_engine_rmd(scripts, args_engine, collect_paths),
      "r" = tryCatch(.script_run(scripts), error = conditionMessage)
    )
  }

  if (is.list(build_error)) {
    return(invisible(build_error[["paths"]]))
  }
  .build_engine_error(build_error, version_run_on_list)
  if (collect_paths && identical(engine, "quarto_document")) {
    return(invisible(.build_copy_docs_quarto_paths_get(
      .build_frontmatter_get(scripts), scripts, args_engine
    )))
  }
  invisible(TRUE)
}

.build_engine_bookdown <- function(args_engine) {
  .dep_install("bookdown")
  .cli_debug("Rendering bookdown project")
  x_return <- try(do.call(bookdown::render_book, args_engine))
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    return(NULL)
  }
  paste0("Error rendering bookdown project ", err_msg)
}

.build_engine_quarto_project <- function(args_engine) {
  .dep_install("quarto")
  .cli_debug("Rendering Quarto project")
  x_return <- try(do.call(quarto::quarto_render, args_engine))
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    return(NULL)
  }
  paste0("Error rendering Quarto project ", err_msg)
}

.build_engine_qmd <- function(scripts, args_engine) {
  .dep_install("quarto")
  fn_vec <- .build_engine_doc_fn_get(scripts = scripts, type = "qmd")
  .cli_debug("Rendering {length(fn_vec)} Quarto document(s)")
  for (x in fn_vec) {
    .cli_debug("Rendering: {x}")
    x_return <- try(
      do.call(quarto::quarto_render, list(input = x) |> append(args_engine))
    )
    if (.is_try_error(x_return)) {
      break
    }
  }
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    return(NULL)
  }
  paste0("Error rendering Quarto document ", x, ": ", err_msg)
}

.build_engine_rmd <- function(scripts, args_engine, collect_paths = FALSE) {
  .dep_install("rmarkdown")
  fn_vec <- .build_engine_doc_fn_get(scripts = scripts, type = "rmd")
  .cli_debug("Rendering {length(fn_vec)} RMarkdown document(s)")
  outputs <- character()
  for (x in fn_vec) {
    .cli_debug("Rendering: {x}")
    x_return <- try(
      do.call(rmarkdown::render, list(input = x) |> append(args_engine))
    )
    if (.is_try_error(x_return)) {
      break
    }
    outputs <- c(outputs, x_return)
  }
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    if (collect_paths) {
      assets <- paste0(
        tools::file_path_sans_ext(sub("\\.nb\\.html$", ".html", outputs)),
        getOption("rmarkdown.files.suffix", "_files"),
        recycle0 = TRUE
      )
      return(list(paths = unique(c(outputs, assets))))
    }
    return(NULL)
  }
  paste0("Error rendering RMarkdown document ", x, ": ", err_msg)
}

.build_engine_error <- function(build_error, version_run_on_list) {
  if (!is.null(build_error)) {
    .build_version_set_post(
      version_run_on_list = version_run_on_list,
      success = FALSE
    )
    stop(build_error)
  }
  invisible(TRUE)
}

.build_engine_doc_fn_get <- function(scripts,
                                     type) {
  detect_str <- switch(tolower(type),
    "qmd" = "\\.qmd$",
    "rmd" = "\\.rmd$",
    stop(paste0("Unknown document type: ", type), call. = FALSE)
  )

  if (is.null(scripts)) {
    fn_vec <- list.files(.path_get(), pattern = detect_str, ignore.case = TRUE)
    missing_files <- NULL
  } else {
    # Filter files by pattern and check existence
    file_matching <- scripts[grepl(detect_str, scripts, ignore.case = TRUE)]
    fn_vec <- .file_filter_exists(file_matching)
    # Track which files are missing
    missing_files <- setdiff(file_matching, fn_vec)
  }

  fn_vec <- setdiff(fn_vec, c("README.Rmd", as.character(.path_get("README.Rmd"))))
  .build_engine_doc_fn_get_error(fn_vec, type, scripts, missing_files)
  fn_vec
}

.build_engine_doc_fn_get_error <- function(fn, type, scripts, missing_files = NULL) {
  # Check if there are missing files when files were specified
  if (!is.null(missing_files) && length(missing_files) > 0) {
    document_type <- switch(tolower(type),
      "qmd" = "Quarto",
      "rmd" = "RMarkdown"
    )
    error_msg <- paste0(
      "The following ", document_type, " document(s) could not be found: ",
      paste0(missing_files, collapse = ", "), ". ",
      "Please check that the file(s) exist and have the correct extension (", type, ")."
    )
    stop(error_msg, call. = FALSE)
  }

  # Check if no files were found at all
  if (.is_given_mid(fn) && .is_len_pos(fn)) {
    return(invisible(TRUE))
  }

  document_type <- switch(tolower(type),
    "qmd" = "Quarto",
    "rmd" = "RMarkdown"
  )

  # Create appropriate error message based on whether files were specified
  if (is.null(scripts)) {
    error_msg <- paste0(
      "No ", document_type, " documents found in the project directory. ",
      "Please create a ", tolower(type), " file or specify the file path explicitly."
    )
  } else {
    # Fallback for when no files match the pattern
    error_msg <- paste0(
      "The following ", document_type, " document(s) could not be found: ",
      paste0(scripts, collapse = ", "), ". ",
      "Please check that the file(s) exist and have the correct extension (", type, ")."
    )
  }

  stop(error_msg, call. = FALSE)
}
