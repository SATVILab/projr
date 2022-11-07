#' @title Build project to output
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @export
projr_build_output <- function(bump_component) {
  if (missing(bump_component)) {
    yml_projr <- projr_yml_get()
    version <- yml_projr$version
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  .projr_build(bump_component = bump_component)
}

#' @title Build dev project
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @export
#' @export
projr_build_dev <- function(bump = FALSE) {
  # NULL if FALSE and "dev" if TRUE
  .projr_build(bump_component = switch(bump,
    "dev"
  ))
}

#' @title Build project
#'
#' @param version "major", "minor", "patch".
#' Version to bump.
#' If \code{NULL}, then the lowest available
#' version component in the `version` key of `_projr.yml`
#' is used.
#' Default is \code{NULL}.
.projr_build <- function(bump_component) {
  dir_proj <- rprojroot::is_r_package$find_file()

  # read in settings
  yml_projr <- projr_yml_get()

  version_format_list <- .projr_version_format_list_get()
  proj_nm <- projr_name_get()

  dir_bookdown_orig <- .projr_yml_bd_get()$output_dir

  # get version for DESCRIPTION and bookdown from run onwards
  version_run_on_list <- .projr_version_run_onwards_get(
    bump_component = bump_component
  )

  projr_version_set(version_run_on_list$desc[["run"]], "DESCRIPTION")
  projr_version_set(version_run_on_list$bd[["run"]], "bookdown")

  dir_bookdown_run <- .projr_yml_bd_get()[["output_dir"]]

  # snapshot if need be
  if (yml_projr[["build-output"]][["renv"]] &&
    !Sys.getenv("PROJR_TEST") == "TRUE") {
    if (!is.null(bump_component)) {
      if (bump_component != "dev") {
        renv::snapshot(prompt = FALSE)
      }
    }
  }

  bd_status <- try(bookdown::render_book())
  if (identical(class(bd_status), "try-error")) {
    projr_version_set(version_run_on_list$desc[["failure"]], "DESCRIPTION")
    projr_version_set(version_run_on_list$bd[["failure"]], "bookdown")
    # TODO: #156 delet
    stop(bd_status)
  }

  # update any docs
  roxygen2::roxygenise(package.dir = dir_proj)

  # update README.Rmd, if found
  if (file.exists(file.path(dir_proj, "README.Rmd"))) {
    rmarkdown::render(
      file.path(dir_proj, "README.Rmd"),
      output_format = "md_document"
    )
  }

  # copy
  dev_run_n <- !(is.null(bump_component) || bump_component == "dev")
  copy_to_output <- projr_yml_get()[["build-dev"]][["copy_to_output"]] ||
    dev_run_n
  if (copy_to_output) {
    # copy to output
    # ----------------
    .projr_output_copy(bump_component = bump_component)

    # copy to archive
    # ----------------
    dir_output <- projr_dir_get(type = "output", output_safe = FALSE)
    dir_archive <- projr_dir_get(type = "archive")
    if (!fs::is_absolute_path(dir_output)) {
      dir_output <- file.path(dir_proj, dir_output)
    }
    if (!fs::is_absolute_path(dir_archive)) {
      dir_archive <- file.path(dir_proj, dir_archive)
    }
    fn_vec <- list.files(
      dir_output,
      recursive = TRUE, all.files = TRUE, full.names = TRUE
    )
    if (length(list.dirs(dir_output)) > 0) {
      path_archive_zip <- file.path(dir_archive, paste0(
        "V", version_run_on_list$desc[["success"]],
        ".zip"
      ))
      if (file.exists(path_archive_zip)) {
        unlink(path_archive_zip, recursive = TRUE)
      }
      zip(path_archive_zip, files = fn_vec)
    }


    # clear old dev versions
    # ------------------------
    # from the report directory
    match_regex <- paste0(
      "^",
      proj_nm,
      "V\\d+",
      paste0("\\", version_format_list[["sep"]], "\\d+", collapse = ""),
      "$"
    )
    dir_report <- basename(
      list.dirs(dirname(dir_bookdown_orig), recursive = FALSE)
    )
    dir_report_rm <- dir_report[grepl(match_regex, dir_report)]
    for (i in seq_along(dir_report_rm)) {
      unlink(file.path(
        dirname(dir_bookdown_orig), dir_report_rm
      ), recursive = TRUE)
    }
    # from the output directory
  }

  projr_version_set(version_run_on_list$desc[["success"]], "DESCRIPTION")
  projr_version_set(version_run_on_list$bd[["success"]], "bookdown")


  invisible(TRUE)
}


.projr_output_copy <- function(bump_component) {
  yml_projr <- projr_yml_get()
  dir_proj <- rprojroot::is_r_package$find_file()
  dir_bookdown <- .projr_yml_bd_get()[["output_dir"]]
  yml_projr_dir <- yml_projr[["directories"]]
  output_safe <- is.null(bump_component) || bump_component == "dev"
  dir_output <- projr_dir_get("output", output_safe = output_safe)
  copy_to_output_list <- yml_projr[["build-output"]][["copy_to_output"]]
  # make it an absolute path, stuck onto dir_proj,
  # only if it isn't already an absolute path
  if (!fs::is_absolute_path(dir_output)) {
    dir_output <- file.path(dir_proj, dir_output)
  }

  # copy data_raw and cache across, if desired
  for (type in c("data_raw", "cache")) {
    copy <- copy_to_output_list[[type]]
    if (is.logical(copy)) {
      if (!copy) next
    } else {
      stop("non-logical versions of copy not supported yet")
    }
    yml_projr_dir_type <- yml_projr_dir[names(yml_projr_dir) == type]
    for (i in seq_along(yml_projr_dir_type)) {
      dir_input <- yml_projr_dir_type[[i]][["path"]]
      if (!fs::is_absolute_path(dir_input)) {
        dir_input <- file.path(dir_proj, dir_input)
      }
      fn_vec <- list.files(
        dir_input,
        recursive = TRUE, all.files = TRUE,
        full.names = TRUE
      )
      if (length(fn_vec) == 0) next
      path_save <- file.path(
        dir_output,
        type,
        paste0(yml_projr_dir_type[[i]][["name"]], ".zip")
      )
      if (file.exists(path_save)) unlink(path_save, recursive = TRUE)
      if (!dir.exists(dirname(path_save))) {
        dir.create(dirname(path_save), recursive = TRUE)
      }
      zip(path_save, files = fn_vec)
    }
  }

  # copy generated report
  copy_bookdown <- copy_to_output_list[["bookdown"]]
  if (is.logical(copy_bookdown)) {
    if (copy_bookdown) {
      path_zip <- file.path(
        dir_output, "bookdown", paste0(basename(dir_bookdown), ".zip")
      )
      if (dir.exists(dirname(path_zip))) unlink(path_zip, recursive = TRUE)
      if (!dir.exists(dirname(path_zip))) {
        dir.create(dirname(path_zip), recursive = TRUE)
      }
      zip(
        path_zip,
        files = list.files(
          dir_bookdown,
          recursive = TRUE, full.names = TRUE
        )
      )
    }
  } else {
    stop("copy not being logical for bookdown not supported yet")
  }

  # build package
  if (yml_projr[["build-output"]][["copy_to_output"]][["package"]]) {
    dir_pkg <- file.path(dir_output, "pkg")
    if (!dir.exists(dir_pkg)) dir.create(dir_pkg)

    devtools::build(
      pkg = dir_proj,
      path = dir_pkg,
      binary = FALSE,
      quiet = TRUE
    )
  }
  invisible(TRUE)
}
