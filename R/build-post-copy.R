.projr_build_copy <- function(output_run,
                              bump_component,
                              version_run_on_list) {
  # copy document across to correct directories.
  # this always happens because rmds' and qmds'
  # outputs need to be collected after build
  .projr_build_copy_docs(output_run)

  # consider not copying output and data.
  # will leave in cache directories otherwise
  if (!.projr_build_copy_check(output_run)) {
    return(invisible(FALSE))
  }

  # copy to unsafe directories
  .projr_build_copy_to_unsafe(output_run)

  # package
  .projr_build_copy_pkg(output_run)

  # save to output
  .projr_build_copy_dir(output_run, dest_type = "output")

  invisible(TRUE)
}

.projr_build_copy_check <- function(output_run) {
  output_run || .projr_yml_build_get_dev_output_complete(NULL)
}

.projr_build_copy_to_unsafe <- function(output_run) {
  # copy items from safe directory across to final directory.
  if (!.projr_build_copy_output_direct_check(output_run)) {
    # don't do it if not an output run (leave it in dev)
    return(invisible(FALSE))
  }
  label_vec_output <- .projr_yml_dir_get_label_output() |> c("data")
  for (x in label_vec_output) {
    .projr_dir_move(
      projr_dir_get(x, safe = TRUE), projr_dir_get(x, safe = FALSE)
    )
  }
  invisible(TRUE)
}

.projr_build_copy_output_direct_check <- function(output_run) {
  output_run
}

# attempt package build first if required
# at this point, we don't need to check for
# whether it's a dev run or not
# as that's already been done.
# we copy if it says we do in build$copy-to-output$package
.projr_build_copy_pkg <- function(output_run) {
  if (!.projr_build_copy_pkg_check()) {
    return(invisible(FALSE))
  }
  .projr_build_copy_pkg_build()
  for (x in .projr_build_copy_pkg_get_label()) {
    .projr_file_copy(
      .projr_build_copy_pkg_build_path_get(),
      projr_dir_get(x, "pkg", safe = !output_run)
    )
  }
  invisible(TRUE)
}

.projr_build_copy_pkg_check <- function() {
  .projr_build_copy_pkg_get_label() |>
    .projr_state_nz() |>
    all()
}

.projr_build_copy_pkg_get_label <- function() {
  sapply(
    .projr_yml_dir_get_label_output(),
    .projr_yml_dir_get_pkg_nm_complete,
    profile = NULL
  )
}

.projr_build_copy_pkg_build <- function() {
  path_dir_pkg <- .projr_build_copy_pkg_build_path_setup()
  version_pkg <- .projr_desc_get()[, "Version"][[1]]
  fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
  path_pkg <- file.path(path_dir_pkg, fn_pkg)
  .projr_dep_install("pkgbuild")
  pkgbuild::build(
    path = .projr_dir_proj_get(),
    dest_path = path_pkg,
    binary = FALSE,
    quiet = TRUE
  )
}

.projr_build_copy_pkg_build_path_setup <- function() {
  path_dir_pkg <- .projr_build_copy_pkg_build_path_get()
  if (dir.exists(path_dir_pkg)) {
    unlink(path_dir_pkg, recursive = TRUE)
  }
  dir.create(path_dir_pkg, recursive = TRUE)
  path_dir_pkg
}

.projr_build_copy_pkg_build_path_get <- function() {
  file.path(tempdir(), "projr", "pkg")
}

# copy all requested items to out and archive
# ===========================================

.projr_build_copy_dir <- function(output_run) {
  label_vec <- .projr_build_copy_dir_get_label()
  for (x in label_vec) {
    output_vec <- .projr_yml_dir_get_output_nm_complete(x, NULL)
    for (i in seq_along(output_vec)) {
      projr::projr_dir_mimick(
        projr_dir_get(x, safe = !output_run),
        projr_dir_get(output_vec[[i]], x, safe = !output_run)
      )
    }
  }
  invisible(TRUE)
}

.projr_build_copy_dir_get_label <- function() {
  .projr_yml_dir_get_label_output() |>
    .projr_yml_dir_get_label_data_raw() |>
    .projr_yml_dir_get_label_cache() |>
    c("data")
}
