.build_copy <- function(output_run,
                        bump_component,
                        version_run_on_list,
                        file = NULL) {
  .cli_debug("Starting .build_copy()")
  .cli_debug("  output_run: {output_run}")
  .cli_debug("  bump_component: {bump_component}")
  .cli_debug("  file: {paste(file, collapse = ', ')}")

  # copy document across to correct directories.
  # this always happens because rmds' and qmds'
  # outputs need to be collected after build
  .cli_debug("Calling .build_copy_docs()")
  .build_copy_docs(output_run, file = file)
  .cli_debug("Completed .build_copy_docs()")

  # consider not copying output and data.
  # will leave in cache directories otherwise
  if (!.build_copy_check(output_run)) {
    .cli_debug("Skipping output/data copy: check failed")
    .cli_debug("Finished .build_copy() - early return")
    return(invisible(FALSE))
  }

  .cli_debug("Proceeding with output/data copy")

  # copy directories with output configuration first
  # (before copying outputs from temp to final)
  .cli_debug("Calling .build_copy_dir()")
  .build_copy_dir(output_run)
  .cli_debug("Completed .build_copy_dir()")

  # copy to unsafe directories
  .cli_debug("Calling .build_copy_to_unsafe()")
  .build_copy_to_unsafe(output_run)
  .cli_debug("Completed .build_copy_to_unsafe()")

  # package
  .cli_debug("Calling .build_copy_pkg()")
  .build_copy_pkg(output_run)
  .cli_debug("Completed .build_copy_pkg()")

  .cli_debug("Finished .build_copy()")
  invisible(TRUE)
}

.build_copy_check <- function(output_run) {
  result <- output_run || .yml_build_get_dev_output_complete(NULL)
  .cli_debug("  .build_copy_check() => {result} (output_run={output_run}, dev_output_complete={.yml_build_get_dev_output_complete(NULL)})")
  result
}

.build_copy_to_unsafe <- function(output_run) {
  .cli_debug("Starting .build_copy_to_unsafe()")
  .cli_debug("  output_run: {output_run}")

  # copy items from safe directory across to final directory.
  if (!.build_copy_output_direct_check(output_run)) {
    # don't do it if not an output run (leave it in dev)
    .cli_debug("Skipping copy to unsafe: not an output run")
    .cli_debug("Finished .build_copy_to_unsafe() - early return")
    return(invisible(FALSE))
  }

  label_vec_output <- .yml_dir_get_label_output(NULL) |>
    c("data") |>
    unique()
  .cli_debug("  Labels to copy: {paste(label_vec_output, collapse = ', ')}")

  for (x in label_vec_output) {
    .cli_debug("  Processing label: {x}")

    # Get source directory without creating it
    source_dir <- projr_path_get_dir(x, safe = TRUE, create = FALSE)
    .cli_debug("    Source dir: {source_dir}")

    # Skip if source directory doesn't exist or is empty
    if (!dir.exists(source_dir)) {
      .cli_debug("    Source directory does not exist, skipping")
      next
    }

    # Check if directory has any files
    files_in_source <- list.files(source_dir, recursive = TRUE)
    .cli_debug("    Files in source: {length(files_in_source)}")
    if (length(files_in_source) == 0) {
      .cli_debug("    Source directory is empty, skipping")
      next
    }

    # Get destination directory (will be created by .dir_move if needed)
    dest_dir <- projr_path_get_dir(x, safe = FALSE)
    .cli_debug("    Dest dir: {dest_dir}")

    # Move files from safe to unsafe directory
    .cli_debug("    Calling .dir_move() from {source_dir} to {dest_dir}")
    .dir_move(source_dir, dest_dir)
    .cli_debug("    Completed .dir_move()")

    .cli_info("Copied {x} from {source_dir} to {dest_dir}")
  }

  .cli_debug("Finished .build_copy_to_unsafe()")
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
  .cli_debug("Starting .build_copy_pkg()")
  .cli_debug("  output_run: {output_run}")

  if (!.build_copy_pkg_check()) {
    .cli_debug("Skipping package copy: check failed")
    .cli_debug("Finished .build_copy_pkg() - early return")
    return(invisible(FALSE))
  }

  .cli_debug("Proceeding with package build and copy")

  # build
  .cli_debug("Calling .build_copy_pkg_build()")
  .build_copy_pkg_build()
  .cli_debug("Completed .build_copy_pkg_build()")

  # copy to output directories
  label_list <- .build_copy_pkg_get_label()
  .cli_debug("  Labels for package copy: {paste(unlist(label_list), collapse = ', ')}")

  for (x in label_list) {
    .cli_debug("  Copying package to label: {x[[1]]}")

    pkg_files <- .build_copy_pkg_build_path_get() |>
      .file_ls(full.names = TRUE)
    dest_path <- projr_path_get_dir(x[[1]], "pkg", safe = !output_run)

    .cli_debug("    Package files: {paste(basename(pkg_files), collapse = ', ')}")
    .cli_debug("    Destination: {dest_path}")

    fs::file_copy(pkg_files, dest_path)
    .cli_debug("    Completed copy to {x[[1]]}")
  }

  .cli_debug("Finished .build_copy_pkg()")
  invisible(TRUE)
}

.build_copy_pkg_check <- function() {
  .build_copy_pkg_get_label() |>
    unlist() |>
    stats::setNames(NULL) |>
    .is_len_pos()
}

.build_copy_pkg_get_label <- function() {
  # sapply needed here as function can return character() (length 0)
  sapply(
    .yml_dir_get_label_output(NULL),
    .yml_dir_get_pkg_complete,
    profile = NULL,
    USE.NAMES = FALSE
  )
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
  fn_pkg <- paste0(projr_name_get(), "_", version_pkg, ".tar.gz")
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
  .cli_debug("Starting .build_copy_dir()")
  .cli_debug("  output_run: {output_run}")

  label_vec <- .build_copy_dir_get_label()
  .cli_debug("  Labels to process: {paste(label_vec, collapse = ', ')}")

  for (x in label_vec) {
    .cli_debug("  Processing label: {x}")

    output_vec <- .yml_dir_get_output_nm_complete(x, NULL)
    .cli_debug("    Output names: {paste(output_vec, collapse = ', ')}")

    for (i in seq_along(output_vec)) {
      source_path <- projr_path_get_dir(x, safe = !output_run)
      dest_path <- projr_path_get_dir(output_vec[[i]], x, safe = !output_run)
      dir_exc <- .build_label_get_dir_exc(x)

      .cli_debug("    Copying {i}/{length(output_vec)}: {source_path} -> {dest_path}")
      .cli_debug("      Excluded dirs: {paste(dir_exc, collapse = ', ')}")

      .dir_copy_exact(
        source_path,
        dest_path,
        dir_exc = dir_exc
      )
      .cli_debug("    Completed copy {i}/{length(output_vec)}")
    }
  }

  .cli_debug("Finished .build_copy_dir()")
  invisible(TRUE)
}

.build_copy_dir_get_label <- function() {
  # Return all directory labels that can be copied
  # Excludes "code" and "project" which are not meant for copying
  # The actual filtering by output configuration happens in .build_copy_dir()
  .opt_dir_get_label_get(NULL)
}
