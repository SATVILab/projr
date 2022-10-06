library(testthat)
devtools::load_all()
devtools::test_active_file("R/activate.R")

devtools::test()

names(yml_active)
names(yml_active_dir)
expect_true(all())
# cache from container radian:
# /tmp/RtmpSUi8pC/renv/cache/v5/linux-ubuntu-focal/R-4.2/x86_64-pc-linux-gnu
# cache from system radian:
# /tmp/RtmpuaWI9P/renv/cache/v5/R-4.2/x86_64-pc-linux-gnu

.get_version_updated <-
  .projr_build <- function(bump_component, wd_var) {
    dir_proj <- rprojroot::is_r_package$find_file()
    # read in settings
    yml_projr <- projr_get_yml_active(
      wd_var = "PROJR_WORKING_DIRECTORY",
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
    if (yml_projr$renv) {
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
    write.dcf(desc_file, file = file.path(dir_proj, "DESCRIPTION"))
    invisible(TRUE)
  }

projr_build_output <- function(bump_component, wd_var) {
  .projr_build(bump_component = bump_component)
}

projr_build_dev <- function(bump, wd_var) {
  .projr_build(bump_component = switch(bump,
    "dev"
  ))
}


projr_build_output <- function(bump_component, wd_var = "PROJR_WORKING_DIRECTORY") {

  # read in settings
  yml_projr <- projr_get_yml_active(
    wd_var = wd_var,
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

  yaml::write_yaml(
    yml_bd_run,
    rprojroot::is_r_package$find_file("_bookdown.yml")
  )

  # snapshot if need be
  if (yml_projr$renv) {
    renv::snapshot(prompt = FALSE)
  }

  bd_status <- try(bookdown::render_book())
  if (identical(class(bd_status), "try-error")) {
    yaml::write_yaml(
      yml_bd_orig, rprojroot::is_r_package$find_file("_bookdown.yml")
    )
    stop(bd_status)
  }
}


projr_bo <- projr_build_output

projr_build_dev <- function(bump = FALSE,
                            wd_var = "PROJR_WORKING_DIRECTORY") {
  dir_proj <- rprojroot::is_r_package$find_file()

  # read in settings
  yml_projr <- projr_get_yml_active(
    wd_var = "PROJR_WORKING_DIRECTORY",
    path_yml = file.path(dir_proj, "_projr.yml"),
    silent = TRUE
  )

  yml_bd_orig <- yaml::read_yaml(
    file.path(dir_proj, "_bookdown.yml")
  )

  dir_output_orig <- yml_bd_orig$output_dir

  # get final version
  version_and_fn_final_vec <- .get_version_and_fn_final(
    version_format = yml_projr$version,
    fn_orig = yml_bd_orig$book_filename,
    bump_component = switch(bump,
      "dev"
    )
  )

  if (bump) {
    yml_bd_run <- yml_bd_orig
    yml_bd_run$book_filename <- version_and_fn_final_vec[["fn"]]

    yml_bd_run$output_dir <- gsub(
      yml_bd_orig$book_filename,
      version_and_fn_final_vec[["fn"]],
      yml_bd_orig$output_dir
    )

    yaml::write_yaml(
      yml_bd_run,
      rprojroot::is_r_package$find_file("_bookdown.yml")
    )
  }

  desc_file <- read.dcf(file.path(dir_proj, "DESCRIPTION"))
  desc_file[1, "Version"] <- version_and_fn_final_vec["version"]

  # need to bump DESCRIPTION to match
  bd_status <- try(bookdown::render_book())
  if (identical(class(bd_status), "try-error")) {
    yaml::write_yaml(
      yml_bd_orig, file.path(dir_proj, "_bookdown.yml")
    )
    stop(bd_status)
  }
  write.dcf(desc_file, file = file.path(dir_proj, "DESCRIPTION"))
  invisible(TRUE)
}
