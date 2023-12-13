.projr_build_engine <- function(file,
                                version_run_on_list,
                                ...) {
  build_error <- switch(.projr_engine_get(),
    "bookdown" = {
      .projr_dep_add("bookdown")
      if (!requireNamespace("bookdown", quietly = TRUE)) {
        renv::install("bookdown")
      }
      x_return <- try(bookdown::render_book(...))
      err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
      if (is.null(err_msg)) {
        return(NULL)
      }
      paste0("Error rendering bookdown project ", err_msg)
    },
    "quarto_project" = {
      .projr_dep_add("quarto")
      if (!requireNamespace("quarto", quietly = TRUE)) {
        renv::install("quarto")
      }
      x_return <- try(quarto::quarto_render(...))
      err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
      if (is.null(err_msg)) {
        return(NULL)
      }
      paste0("Error rendering Quarto project ", err_msg)
    },
    "quarto_document" = {
      .projr_dep_add("quarto")
      if (!requireNamespace("quarto", quietly = TRUE)) {
        renv::install("quarto")
      }
      fn_vec <- .projr_build_engine_doc_fn_get(file = file, type = "qmd")
      for (x in fn_vec) {
        x_return <- try(quarto::quarto_render(x, ...))
        if (inherits(x_return, "try-error")) {
          break
        }
      }
      err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
      if (is.null(err_msg)) {
        return(NULL)
      }
      paste0("Error rendering Quarto document ", x, ": ", err_msg)
    },
    "rmd" = {
      .projr_dep_add("rmarkdown")
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        renv::install("rmarkdown")
      }
      fn_vec <- .projr_build_engine_doc_fn_get(file = file, type = "rmd")
      for (x in fn_vec) {
        x_return <- try(rmarkdown::render(x, ...))
        if (inherits(x_return, "try-error")) {
          break
        }
      }
      err_msg <- .try_err_msg_get(x_return, require_try_error = FALSE)
      if (is.null(err_msg)) {
        return(NULL)
      }
      paste0("Error rendering RMarkdown document ", x, ": ", err_msg)
    }
  )

  if (!is.null(build_error)) {
    .projr_build_version_set_post(
      version_run_on_list = version_run_on_list,
      success = FALSE
    )
    stop(build_error)
  }
  invisible(TRUE)
}

.projr_build_engine_doc_fn_get <- function(file,
                                           type) {
  detect_str <- switch(tolower(type),
    "qmd" = "\\.qmd$",
    "rmd" = "\\.Rmd$|\\.rmd$"
  )
  fn_vec <- switch(as.character(is.null(file)),
    "TRUE" = list.files(.projr_dir_proj_get(), pattern = detect_str),
    "FALSE" = {
      file[grepl("\\.qmd$", file)] |> .projr_file_filter_exists()
    }
  )
  if (.projr_state_len_z(fn_vec)) {
    .projr_build_engine_doc_fn_get_error(type)
  }
  fn_vec
}

.projr_build_engine_doc_fn_get_error <- function(type) {
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
