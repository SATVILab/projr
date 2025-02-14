.build_engine <- function(file,
                                version_run_on_list,
                                args_engine) {
  build_error <- switch(.engine_get(),
    "bookdown" = .build_engine_bookdown(args_engine),
    "quarto_project" = .build_engine_quarto_project(args_engine),
    "quarto_document" = .build_engine_qmd(file, args_engine),
    "rmd" = .build_engine_rmd(file, args_engine)
  )
  .build_engine_error(build_error, version_run_on_list)
}

.build_engine_bookdown <- function(args_engine) {
  .dep_install("bookdown")
  x_return <- try(do.call(bookdown::render_book, args_engine))
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    return(NULL)
  }
  paste0("Error rendering bookdown project ", err_msg)
}

.build_engine_quarto_project <- function(args_engine) {
  .dep_install("quarto")
  x_return <- try(do.call(quarto::quarto_render, args_engine))
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
    return(NULL)
  }
  paste0("Error rendering Quarto project ", err_msg)
}

.build_engine_qmd <- function(file, args_engine) {
  .dep_install("quarto")
  fn_vec <- .build_engine_doc_fn_get(file = file, type = "qmd")
  for (x in fn_vec) {
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

.build_engine_rmd <- function(file, args_engine) {
  .dep_install("rmarkdown")
  fn_vec <- .build_engine_doc_fn_get(file = file, type = "rmd")
  for (x in fn_vec) {
    x_return <- try(
      do.call(rmarkdown::render, list(input = x) |> append(args_engine))
    )
    if (.is_try_error(x_return)) {
      break
    }
  }
  err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
  if (is.null(err_msg)) {
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

.build_engine_doc_fn_get <- function(file,
                                           type) {
  detect_str <- switch(tolower(type),
    "qmd" = "\\.qmd$",
    "rmd" = "\\.Rmd$|\\.rmd$",
    stop(paste0("Unknown document type: ", type), call. = FALSE)
  )
  fn_vec <- switch(as.character(is.null(file)),
    "TRUE" = list.files(.path_get(), pattern = detect_str),
    "FALSE" = file[grepl(detect_str, file)] |> .file_filter_exists()
  )
  .build_engine_doc_fn_get_error(fn_vec, type)
  fn_vec |> setdiff("README.Rmd")
}

.build_engine_doc_fn_get_error <- function(fn, type) {
  if (.is_given_mid(fn) && .is_len_pos(fn)) {
    return(invisible(TRUE))
  }
  document_type <- switch(tolower(type),
    "qmd" = "Quarto",
    "rmd" = "RMarkdown"
  )
  stop(
    paste0("No ", document_type,
      " documents found that match any files specified: ",
      paste0(file, collapse = ", "),
      sep = ""
    )
  )
}
