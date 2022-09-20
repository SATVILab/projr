#' @title Build project
#'
#' @param version "major", "minor", "patch".
#' Version to bump.
#' If \code{NULL}, then the lowest available
#' version component in the `version` key of `_projr.yml`
#' is used.
#' Default is \code{NULL}.
.projr_build <- function(bump_component, wd_var = "LOCAL_WORKSPACE_FOLDER") {
  dir_proj <- rprojroot::is_r_package$find_file()

  # read in settings
  yml_projr <- projr_get_yml_active(
    wd_var = wd_var,
    path_yml = file.path(dir_proj, "_projr.yml"),
    silent = TRUE
  )
  yml_bd_orig <- yaml::read_yaml(
    file.path(dir_proj, "_bookdown.yml")
  )

  desc_orig <- read.dcf(file.path(dir_proj, "DESCRIPTION"))

  version_format_list <- .get_version_format_list(
    version_format = yml_projr[["version"]]
  )
  proj_nm <- .get_proj_nm(
    fn = yml_bd_orig$book_filename,
    version_format = yml_projr[["version"]]
  )

  dir_output_orig <- yml_bd_orig$output_dir

  # get version for DESCRIPTION and bookdown from run onwards

  version_orig_vec <- .get_version_orig_vec(
    fn = yml_bd_orig$book_filename,
    version_desc = desc_orig[1, "Version"][[1]],
    proj_nm = proj_nm
  )
  version_run_on_list <- .get_version_run_on(
    version_orig_vec = version_orig_vec,
    bump_component = bump_component,
    version_format_list = version_format_list
  )

  projr_version_set(
    version_run_on_list$desc[["run"]], "DESCRIPTION"
  )
  projr_version_set(
    version_run_on_list$bd[["run"]], "bookdown"
  )

  # snapshot if need be
  if (yml_projr[["build-bump_version"]]$renv) {
    if (!is.null(bump_component)) {
      if (bump_component != "dev") {
        renv::snapshot(prompt = FALSE)
      }
    }
  }

  bd_status <- try(bookdown::render_book())
  if (identical(class(bd_status), "try-error")) {
    projr_version_set(
      version_run_on_list$desc[["failure"]], "DESCRIPTION"
    )
    projr_version_set(
      version_run_on_list$bd[["failure"]], "bookdown"
    )
    stop(bd_status)
  }

  projr_version_set(
    version_run_on_list$desc[["success"]], "DESCRIPTION"
  )
  projr_version_set(
    version_run_on_list$bd[["success"]], "bookdown"
  )

  # browser()
  if (yml_projr[["build-bump_version"]]$package_build) {
    # START HERE
    yml_projr_active <- projr_get_yml_active(
      wd_var = wd_var,
      path_yml = file.path(dir_proj, "_projr.yml"),
      silent = TRUE
    )
    dir_output <- yml_projr_active[["directories"]]$output$path
    if (fs::is_absolute_path(dir_output)) {
      dir_output_pkg_tarball <- dir_output
    } else {
      dir_output_pkg_tarball <- file.path(dir_proj, dir_output)
    }
    devtools::build(
      pkg = dir_proj,
      path = dir_output_pkg_tarball,
      binary = FALSE
    )
  }

  # delete old dev versions
  if (!is.null(bump_component)) {
    if (bump_component %in% c("major", "minor", "patch")) {
      match_regex <- paste0(
        "^",
        proj_nm,
        "V\\d+",
        paste0("\\", version_format_list[["sep"]], "\\d+", collapse = ""),
        "$"
      )
      dir_report <- basename(
        list.dirs(dirname(dir_output_orig), recursive = FALSE)
      )
      dir_report_rm <- dir_report[grepl(match_regex, dir_report)]
      for (i in seq_along(dir_report_rm)) {
        unlink(file.path(
          dirname(dir_output_orig), dir_report_rm
        ), recursive = TRUE)
      }
    }
  }

  invisible(TRUE)
}

#' @export
projr_build_output <- function(bump_component, wd_var = "LOCAL_WORKSPACE_FOLDER") {
  if (missing(bump_component)) {
    yml_projr <- yaml::read_yaml(
      rprojroot::is_r_package$find_file("_projr.yml")
    )
    version <- yml_projr$version
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  .projr_build(bump_component = bump_component, wd_var = wd_var)
}

#' @export
projr_build_dev <- function(bump = FALSE, wd_var = "LOCAL_WORKSPACE_FOLDER") {
  .projr_build(bump_component = switch(bump,
    "dev"
  ), wd_var = wd_var)
}

projr_bump_version_dev <- function() {
  yml_bd <- yaml::read_yaml(
    rprojroot::is_r_package$find_file("_bookdown.yml")
  )
  book_fn <- yml_bd$book_filename
  string_version_regex <- "V\\d+$"
  pos_match <- regexpr(string_version_regex, book_fn_dev_n)[[1]]
  book_fn <- substr(book_fn_dev_n, start = 1, stop = pos_match - 1)
  version_location <- stringr::str_locate(book_fn, "-9\\d+")
  version_new <- as.numeric(
    substr(book_fn, version_location[1, "start"] + 1, nchar(book_fn))
  ) + 1
  book_fn <- gsub("-9\\d+", "", book_fn)
  book_fn <- paste0(book_fn, "-", version_new)
  dir_output <- file.path(dirname(yml_bd$output_dir), book_fn)
  yml_bd$output_dir <- dir_output
  yml_bd$book_filename <- book_fn
  yaml::write_yaml(
    yml_bd, rprojroot::is_r_package$find_file("_bookdown.yml")
  )
  invisible(TRUE)
}
