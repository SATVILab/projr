#' @title Build project
#'
#' @param version "major", "minor", "patch".
#' Version to bump.
#' If \code{NULL}, then the lowest available
#' version component in the `version` key of `_projr.yml`
#' is used.
#' Default is \code{NULL}.
.projr_build <- function(bump_component, wd_var) {
  dir_proj <- rprojroot::is_r_package$find_file()
  # read in settings
  yml_projr <- projr_get_yml_active(
    wd_var = "LOCAL_WORKSPACE_FOLDER",
    path_yml = rprojroot::is_r_package$find_file("_projr.yml"),
    silent = TRUE
  )
  yml_bd_orig <- yaml::read_yaml(
    rprojroot::is_r_package$find_file("_bookdown.yml")
  )

  dir_output_orig <- yml_bd_orig$output_dir

  # get final version
  version_and_fn_final_vec <- .get_version_and_fn_final(
    version_format = yml_projr$version,
    fn_orig = yml_bd_orig$book_filename,
    bump_component = bump_component
  )

  yml_bd_run <- yml_bd_orig
  yml_bd_run$book_filename <- version_and_fn_final_vec[["fn"]]

  yml_bd_run$output_dir <- gsub(
    yml_bd_orig$book_filename,
    version_and_fn_final_vec[["fn"]],
    yml_bd_orig$output_dir
  )

  desc_file_orig <- read.dcf(file.path(dir_proj, "DESCRIPTION"))
  desc_file_update <- desc_file_orig
  desc_file_update[1, "Version"] <- version_and_fn_final_vec["version"]

  yaml::write_yaml(
    yml_bd_run,
    rprojroot::is_r_package$find_file("_bookdown.yml")
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
    yaml::write_yaml(
      yml_bd_orig, rprojroot::is_r_package$find_file("_bookdown.yml")
    )
    stop(bd_status)
    # check if version in DESCRIPTION isn't dev, and update if it is
    desc_file_orig[1, "Version"]
  }
  # update DESCRIPTION file
  write.dcf(desc_file_update, file = file.path(dir_proj, "DESCRIPTION"))
  browser()
  if (yml_projr[["build-bump_version"]]$package_build) {
    # START HERE
    yml_projr_active <- projr_get_yml_active(
      wd_var = wd_var,
      path_yml = file.path(dir_proj, "_bookdown.yml"),
      silent = TRUE
    )
    dir_output <- yml_bd_active[["directories"]]$output$path
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
  invisible(TRUE)
}

projr_build_output <- function(bump_component, wd_var) {
  .projr_build(bump_component = bump_component, wd_var = wd_var)
}

projr_build_dev <- function(bump = FALSE, wd_var) {
  .projr_build(bump_component = switch(bump,
    "dev"
  ), wd_var = wd_var)
}
projr_bd <- projr_build_dev

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

projr_bvd <- projr_bump_version_dev
