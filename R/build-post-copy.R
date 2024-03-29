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
  .projr_build_copy_dir(output_run)

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
  label_vec_output <- .projr_yml_dir_get_label_output(NULL) |>
    c("data") |>
    unique()
  for (x in label_vec_output) {
    .dir_move(
      projr_path_get_dir(x, safe = TRUE),
      projr_path_get_dir(x, safe = FALSE)
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
  # build
  .projr_build_copy_pkg_build()
  # copy to output directories
  for (x in .projr_build_copy_pkg_get_label()) {
    file.copy(
      .projr_build_copy_pkg_build_path_get() |>
        .file_ls(full.names = TRUE),
      projr_path_get_dir(x[[1]], "pkg", safe = !output_run)
    )
  }
  invisible(TRUE)
}

.projr_build_copy_pkg_check <- function() {
  .projr_build_copy_pkg_get_label() |>
    unlist() |>
    stats::setNames(NULL) |>
    .is_len_pos()
}

.projr_build_copy_pkg_get_label <- function() {
  sapply(
    .projr_yml_dir_get_label_output(NULL),
    .projr_yml_dir_get_pkg_complete,
    profile = NULL
  ) |>
    stats::setNames(NULL)
}

.projr_build_copy_pkg_build <- function() {
  .projr_dep_install("pkgbuild")
  pkgbuild::build(
    path = .dir_proj_get(),
    dest_path = .projr_build_copy_pkg_build_path_setup(),
    binary = FALSE,
    quiet = TRUE
  ) |>
    invisible()
}

.projr_build_copy_pkg_build_get_path <- function() {
  path_dir_pkg <-
    version_pkg <- .projr_desc_get()[, "Version"][[1]]
  fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
  file.path(path_dir_pkg, fn_pkg)
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
  for (x in .projr_build_copy_dir_get_label()) {
    output_vec <- .projr_yml_dir_get_output_nm_complete(x, NULL)
    for (i in seq_along(output_vec)) {
      .dir_copy_exact(
        projr_path_get_dir(x, safe = !output_run),
        projr_path_get_dir(output_vec[[i]], x, safe = !output_run),
        dir_exc = .projr_build_label_get_dir_exc(x)
      )
    }
  }
  invisible(TRUE)
}

.projr_build_copy_dir_get_label <- function() {
  c(
    .projr_yml_dir_get_label_output(NULL),
    .projr_yml_dir_get_label_data_raw(NULL),
    .projr_yml_dir_get_label_cache(NULL)
  )
}
