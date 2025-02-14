.build_copy <- function(output_run,
                              bump_component,
                              version_run_on_list) {
  # copy document across to correct directories.
  # this always happens because rmds' and qmds'
  # outputs need to be collected after build
  .build_copy_docs(output_run)

  # consider not copying output and data.
  # will leave in cache directories otherwise
  if (!.build_copy_check(output_run)) {
    return(invisible(FALSE))
  }

  # copy to unsafe directories
  .build_copy_to_unsafe(output_run)

  # package
  .build_copy_pkg(output_run)

  # save to output
  .build_copy_dir(output_run)

  invisible(TRUE)
}

.build_copy_check <- function(output_run) {
  output_run || .yml_build_get_dev_output_complete(NULL)
}

.build_copy_to_unsafe <- function(output_run) {
  # copy items from safe directory across to final directory.
  if (!.build_copy_output_direct_check(output_run)) {
    # don't do it if not an output run (leave it in dev)
    return(invisible(FALSE))
  }
  label_vec_output <- .yml_dir_get_label_output(NULL) |>
    c("data") |>
    unique()
  for (x in label_vec_output) {
    .dir_move(
     .path_get_dir(x, safe = TRUE),
     .path_get_dir(x, safe = FALSE)
    )
  }
  invisible(TRUE)
}

.build_copy_output_direct_check <- function(output_run) {
  output_run
}

# attempt package build first if required
# at this point, we don't need to check for
# whether it's a dev run or not
# as that's already been done.
# we copy if it says we do in build$copy-to-output$package
.build_copy_pkg <- function(output_run) {
  if (!.build_copy_pkg_check()) {
    return(invisible(FALSE))
  }
  # build
  .build_copy_pkg_build()
  # copy to output directories
  for (x in .build_copy_pkg_get_label()) {
    file.copy(
      .build_copy_pkg_build_path_get() |>
        .file_ls(full.names = TRUE),
     .path_get_dir(x[[1]], "pkg", safe = !output_run)
    )
  }
  invisible(TRUE)
}

.build_copy_pkg_check <- function() {
  .build_copy_pkg_get_label() |>
    unlist() |>
    stats::setNames(NULL) |>
    .is_len_pos()
}

.build_copy_pkg_get_label <- function() {
  sapply(
    .yml_dir_get_label_output(NULL),
    .yml_dir_get_pkg_complete,
    profile = NULL
  ) |>
    stats::setNames(NULL)
}

.build_copy_pkg_build <- function() {
  .dep_install("pkgbuild")
  pkgbuild::build(
    path = .path_get(),
    dest_path = .build_copy_pkg_build_path_setup(),
    binary = FALSE,
    quiet = TRUE
  ) |>
    invisible()
}

.build_copy_pkg_build_get_path <- function() {
  path_dir_pkg <-
    version_pkg <- .desc_get()[, "Version"][[1]]
  fn_pkg <- paste0.name_get(), "_", version_pkg, ".tar.gz")
  file.path(path_dir_pkg, fn_pkg)
}

.build_copy_pkg_build_path_setup <- function() {
  path_dir_pkg <- .build_copy_pkg_build_path_get()
  if (dir.exists(path_dir_pkg)) {
    unlink(path_dir_pkg, recursive = TRUE)
  }
  dir.create(path_dir_pkg, recursive = TRUE)
  path_dir_pkg
}

.build_copy_pkg_build_path_get <- function() {
  file.path(tempdir(), "projr", "pkg")
}

# copy all requested items to out and archive
# ===========================================

.build_copy_dir <- function(output_run) {
  for (x in .build_copy_dir_get_label()) {
    output_vec <- .yml_dir_get_output_nm_complete(x, NULL)
    for (i in seq_along(output_vec)) {
      .dir_copy_exact(
       .path_get_dir(x, safe = !output_run),
       .path_get_dir(output_vec[[i]], x, safe = !output_run),
        dir_exc = .build_label_get_dir_exc(x)
      )
    }
  }
  invisible(TRUE)
}

.build_copy_dir_get_label <- function() {
  c(
    .yml_dir_get_label_output(NULL),
    .yml_dir_get_label_raw(NULL),
    .yml_dir_get_label_cache(NULL)
  )
}
