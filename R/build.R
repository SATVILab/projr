#' @title Build project to output
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @param bump_component "major", "minor", "patch" or missing.
#' Specifies version component to increment.
#' If missing, then is set equal to the lowest version component
#' in used version format.
#' No default (i.e. is missing by default).
#'
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_output <- function(bump_component, ...) {
  if (missing(bump_component)) {
    yml_projr <- projr_yml_get()
    version <- yml_projr[["version-format"]]
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  .projr_build(bump_component = bump_component, ...)
}

#' @title Build dev project
#'
#' @description Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' @param bump logical.
#' Whether to increment dev version for build.
#' Default is \code{FALSE}.
#' @param ... Arguments passed to \code{bookdown::render}.
#'
#' @export
projr_build_dev <- function(bump = FALSE, ...) {
  # NULL if FALSE and "dev" if TRUE
  bump_component <- switch(bump,
    "dev"
  )
  .projr_build(bump_component = bump_component, ...)
}

.projr_build <- function(bump_component, ...) {
  # read in settings
  yml_projr <- projr_yml_get()

  # get version for DESCRIPTION and bookdown from run onwards
  version_run_on_list <- .projr_version_run_onwards_get(
    bump_component = bump_component
  )

  projr_version_set(version_run_on_list$desc[["run"]], "DESCRIPTION")
  projr_version_set(version_run_on_list$bd[["run"]], "bookdown")

  # snapshot if need be
  if (yml_projr[["build-output"]][["renv"]] &&
    !Sys.getenv("PROJR_TEST") == "TRUE") {
    if (!is.null(bump_component)) {
      if (bump_component != "dev") {
        renv::snapshot(prompt = FALSE)
      }
    }
  }

  # empty output directory
  dev_run_n <- !(is.null(bump_component) || bump_component == "dev")
  unlink(projr_dir_get("output", output_safe = !dev_run_n), recursive = TRUE)
  bd_status <- try(bookdown::render_book(...))
  if (identical(class(bd_status), "try-error")) {
    projr_version_set(version_run_on_list$desc[["failure"]], "DESCRIPTION")
    projr_version_set(version_run_on_list$bd[["failure"]], "bookdown")

    # TODO: #156 delet
    stop(bd_status)
  }

  dir_proj <- rprojroot::is_r_package$find_file()

  # update any docs
  suppressMessages(suppressWarnings(invisible(
    roxygen2::roxygenise(package.dir = dir_proj)
  )))

  # update README.Rmd, if found
  if (file.exists(file.path(dir_proj, "README.Rmd"))) {
    rmarkdown::render(
      file.path(dir_proj, "README.Rmd"),
      output_format = "md_document",
      quiet = TRUE
    )
  }

  # copy
  copy_to_output <- projr_yml_get()[["build-dev"]][["copy-to-output"]] ||
    dev_run_n
  if (copy_to_output) {
    # copy to output
    # ----------------
    .projr_output_copy(bump_component = bump_component)

    if (dev_run_n) {
      # copy to archive
      # ----------------
      dir_output <- projr_dir_get(label = "output", output_safe = FALSE)
      dir_archive <- projr_dir_get(label = "archive")
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
        sink(file.path(tempdir(), "zip123"))
        utils::zip(path_archive_zip, files = fn_vec, flags = "-r9Xq")
        sink(NULL)
      }
    }

    # from the output directory
  }

  # clear old dev version from docs if not a dev run itself
  if (dev_run_n) {
    proj_nm <- projr_name_get()
    version_format_list <- .projr_version_format_list_get()
    match_regex <- paste0(
      "^",
      proj_nm,
      "V\\d+",
      paste0("\\", version_format_list[["sep"]], "\\d+", collapse = ""),
      "$"
    )
    dir_report <- basename(
      list.dirs(dirname(
        projr_dir_get("bookdown", create = FALSE)
      ), recursive = FALSE)
    )
    dir_report_rm <- dir_report[grepl(match_regex, dir_report)]
    for (i in seq_along(dir_report_rm)) {
      unlink(file.path(
        dirname(projr_dir_get("bookdown", create = FALSE)), dir_report_rm[[i]]
      ), recursive = TRUE)
    }
  }

  # clear old output from projr_version_get
  if (dev_run_n) {
    dir_version <- projr_dir_get("output", output_safe = FALSE)
    version_fn <- paste0("VERSION - ", projr_version_get())
    file.create(file.path(dir_version, version_fn))

    dir_tmp_vec_output <- list.dirs(
      projr_dir_get("cache", "projr_output"),
      recursive = FALSE
    )

    for (x in dir_tmp_vec_output) {
      unlink(x, recursive = TRUE)
    }
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

  copy_to_output_list <- yml_projr[["build-output"]][["copy-to-output"]]
  # make it an absolute path, stuck onto dir_proj,
  # only if it isn't already an absolute path
  if (!fs::is_absolute_path(dir_output)) {
    dir_output <- file.path(dir_proj, dir_output)
  }

  # attempt package build first if required
  if (yml_projr[["build-output"]][["copy-to-output"]][["package"]]) {
    dir_pkg <- file.path(dir_output)
    if (!dir.exists(dir_pkg)) dir.create(dir_pkg)
    version_pkg <- .projr_desc_get()[, "Version"][[1]]
    fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
    path_pkg <- file.path(dir_pkg, fn_pkg)
    pkgbuild::build(
      path = dir_proj,
      dest_path = path_pkg,
      binary = FALSE,
      quiet = TRUE
    )
  }

  # copy data_raw and cache across, if desired
  data_raw_or_cache_ind <- grepl("^data_raw|^cache", names(copy_to_output_list))
  label_vec <- names(copy_to_output_list)[data_raw_or_cache_ind]
  label <- label_vec[1]
  for (label in label_vec) {
    copy <- copy_to_output_list[[label]]
    if (is.logical(copy)) {
      if (!copy) next
    } else {
      stop("non-logical versions of copy not supported yet")
    }
    dir_input <- yml_projr_dir[[label]][["path"]]
    if (!fs::is_absolute_path(dir_input)) {
      dir_input <- file.path(dir_proj, dir_input)
    }
    fn_vec <- list.files(
      dir_input,
      recursive = TRUE, all.files = TRUE,
      full.names = TRUE
    )
    remove_projr_output <- label == "cache" &&
      paste0(dir_input, "/", "projr_output") %in%
        list.dirs(dir_input, recursive = FALSE)
    if (remove_projr_output) {
      fn_vec_rem <- list.files(
        file.path(dir_input, "projr_output"),
        recursive = TRUE, all.files = TRUE,
        full.names = TRUE
      )
      fn_vec <- setdiff(fn_vec, fn_vec_rem)
    }
    if (length(fn_vec) == 0) next

    setwd(yml_projr_dir[[label]][["path"]])
    path_zip <- paste0(label, ".zip")
    if (file.exists(path_zip)) {
      file.remove(path_zip)
    }
    utils::zip(
      path_zip,
      files = list.files(getwd(), recursive = TRUE, full.names = FALSE),
      flags = "-r9Xq"
    )
    setwd(dir_proj)
    path_copy <- file.path(
      projr_dir_get("output", output_safe = output_safe), path_zip
    )
    if (file.exists(path_copy)) file.remove(path_copy)
    file.copy(file.path(yml_projr_dir[[label]][["path"]], path_zip), path_copy)
    file.remove(file.path(yml_projr_dir[[label]][["path"]], path_zip))
  }

  # copy generated report
  copy_bookdown <- copy_to_output_list[["bookdown"]]
  if (is.logical(copy_bookdown)) {
    if (copy_bookdown) {
      path_zip <- file.path(
        dirname(dir_bookdown), "bookdown.zip"
      )
      if (file.exists(path_zip)) {
        file.remove(path_zip)
      }
      if (!dir.exists(dirname(path_zip))) {
        dir.create(dirname(path_zip))
      }
      setwd(dir_bookdown)
      path_zip <- paste0(basename(dir_bookdown), ".zip")
      utils::zip(
        path_zip,
        files = list.files(getwd(), recursive = TRUE, full.names = FALSE),
        flags = "-r9Xq"
      )
      setwd(dir_proj)
      path_copy <- file.path(
        projr_dir_get("output", output_safe = output_safe)
      )
      if (!dir.exists(dirname(path_copy))) {
        dir.create(path_copy, recursive = TRUE)
      }
      file.copy(file.path(dir_bookdown, path_zip), path_copy)
      file.remove(file.path(dir_bookdown, path_zip))
      # unzip(
      #  path_copy,
      #  exdir = projr_dir_get("output", "test", output_safe = output_safe)
      # )
    }
  } else {
    stop("copy not being logical for bookdown not supported yet")
  }

  # copy output across at end because it's the least error prone
  # and is the only one that moves and doesn't just copy
  dir_output_safe <- projr_dir_get(
    "output",
    output_safe = TRUE
  )
  if (!fs::is_absolute_path(dir_output_safe)) {
    dir_output_safe <- fs::path_abs(dir_output_safe)
  }
  fn_vec_output_safe <- list.files(
    dir_output_safe,
    recursive = TRUE, full.names = FALSE
  )
  if (length(fn_vec_output_safe) > 0) {
    if (!fs::is_absolute_path(dir_output)) {
      dir_output <- file.path(dir_proj, dir_output)
    }
    fn_vec_output <- file.path(dir_output, fn_vec_output_safe)
    dir_vec <- unique(dirname(fn_vec_output))
    for (x in dir_vec) {
      if (!dir.exists(x)) {
        dir.create(x, recursive = TRUE)
      }
    }
    copy_success <- all(file.rename(
      file.path(dir_output_safe, fn_vec_output_safe), fn_vec_output
    ))
    if (copy_success && !output_safe) {
      unlink(dir_output_safe, recursive = TRUE)
    }
  }

  invisible(TRUE)
}
