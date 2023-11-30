#' @rdname projr_build_output
#' @title Build project to output
#'
#' @description `projr_build_output` Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' `projr_build_major`, `projr_build_minor` and `projr_build_patch`
#' are wrappers around `projr_build_output` with the version component
#' bumped set automatically, e.g. `projr_build_major()` is equivalent
#' `projr_build_output(bump_component = "major")`.
#'
#' @param bump_component "major", "minor", "patch" or missing.
#' Specifies version component to increment.
#' If missing, then is set equal to the lowest version component
#' in used version format.
#' No default (i.e. is missing by default).
#' @param msg character.
#' Message to append to Git commit messages.
#' Default is \code{NULL}, in which
#' case the user is prompted for a message or,
#' if the session is not interactive, it is
#' left empty.
#' Default is \code{NULL}.
#' Note that the Git messages in this case would not be blank -
#' they would simply consist of details as to the version
#' being bumped to and the stage in the build process
#' at which the commit was made.
#'
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_output <- function(bump_component,
                               msg = NULL,
                               ...) {
  if (missing(bump_component)) {
    version <- projr_version_format_get()
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  if (is.null(msg)) {
    if (!Sys.getenv("PROJR_TEST") == "TRUE") {
      if (interactive()) {
        cat("Please enter a one-line description of change", "\n")
        msg <- readline(prompt = ">> ")
      } else {
        msg <- ""
      }
    } else {
      msg <- ""
    }
  }
  .projr_build(bump_component = bump_component, msg = msg, ...)
}

#' @rdname projr_build_output
#' @export
projr_build_major <- function(msg = NULL, ...) {
  projr_build_output(bump_component = "major", msg = msg, ...)
}

#' @rdname projr_build_output
#' @export
projr_build_minor <- function(msg = NULL, ...) {
  projr_build_output(bump_component = "minor", msg = msg, ...)
}

#' @rdname projr_build_output
#' @export
projr_build_patch <- function(msg = NULL, ...) {
  projr_build_output(bump_component = "patch", msg = msg, ...)
}

#' @title Build dev project
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @param file character vector.
#' Paths to files to build.
#' Paths may be relative to project root, or absolute.
#' Default is NULL, in which case all files are built.
#' @param bump logical.
#' Whether to increment dev version for build.
#' Default is \code{FALSE}.
#' @param remove_old_dev logical.
#' If `TRUE`, then previous development builds are deleted
#' after a successful run.
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_dev <- function(file = NULL,
                            bump = FALSE,
                            remove_old_dev = TRUE, ...) {
  # NULL if FALSE and "dev" if TRUE
  bump_component <- switch(bump,
    "dev"
  )
  .projr_build(
    file = file,
    bump_component = bump_component,
    remove_old_dev = TRUE,
    ...
  )
}

.projr_build <- function(file = NULL,
                         bump_component,
                         remove_old_dev = TRUE,
                         msg = "",
                         ...) {
  # ========================
  # SET-UP
  # ========================

  # whether it's an output run  or not
  output_run <- !(is.null(bump_component) || bump_component == "dev")

  # set and check authorisation is available
  env <- environment()
  projr_env_file_activate(env)
  .projr_build_check_auth(bump_component)

  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .projr_build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .projr_build_ignore()

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .projr_build_doc_output_dir_update(FALSE)

  # get DESCRIPTION and build versions under all
  # build outcomes
  version_run_on_list <- .projr_version_run_onwards_get(
    bump_component = bump_component
  )

  # commit any unstaged files pre-run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "pre",
    msg = msg
  )

  # set the version pre-run
  .projr_build_version_set_pre(version_run_on_list)

  # empty output directories
  # (bookdown, output and data)
  .projr_build_clear_pre(output_run)

  # hash cache
  manifest_tbl_pre <- .projr_build_hash_pre(output_run = output_run)

  # ========================
  # RUN
  # ========================

  .projr_build_engine(
    file = file,
    version_run_on_list = version_run_on_list,
    ...
  )

  # ========================
  # HANDLE OUTPUTS
  # ========================

  # get version for DESCRIPTION and bookdown from run onwards

  # update lock file, help files and README
  .projr_build_renv_snapshot(output_run)
  .projr_build_roxygenise(output_run)
  .projr_build_readme_rmd_render(output_run)

  # hash data-raw and outputs
  manifest_tbl <- manifest_tbl_pre |>
    rbind(.projr_build_hash_post(output_run = output_run))

  # remove dev output files
  .projr_build_clear_post(output_run)

  # copy outputs to (final) output directory and archive
  .projr_build_copy(output_run, bump_component, version_run_on_list)

  # commit any files generated by run
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )

  # save manifest table
  .projr_manifest_write(manifest_tbl, output_run = output_run)

  # upload via piggyback
  .projr_pb_upload(output_run = output_run)

  # upload to osf
  .projr_osf_dest_upload(output_run = output_run)

  # clear projr cache
  .projr_build_clear_old_dev(output_run, remove_old_dev)

  # initate dev version
  # ------------------
  # set version
  .projr_build_version_set_post(
    version_run_on_list = version_run_on_list,
    success = TRUE
  )

  # commit dev version
  .projr_build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "dev",
    msg = msg
  )

  # push
  # --------------------

  # push to GitHub
  .projr_build_git_push()

  invisible(TRUE)
}

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
    # TODO: #156 delet
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
    "TRUE" = list.files(pattern = detect_str),
    "FALSE" = {
      fn_vec_type <- file[grepl("\\.qmd$", file)]
      fn_vec_type[file.exists(fn_vec_type)]
    }
  )
  if (length(fn_vec) == 0) {
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

  fn_vec
}
