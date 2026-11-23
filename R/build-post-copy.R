.build_copy <- function(output_run,
                        bump_component,
                        version_run_on_list,
                        engines = .engines_get(),
                        clear_output = "never") {
  # copy document across to correct directories.
  # this always happens because rmds' and qmds'
  # outputs need to be collected after build
  .build_copy_docs(output_run, engines)

  copy_output <- .build_copy_check(output_run)
  directories <- .yml_get(NULL)[["directories"]]
  plan <- .yml_dir_include_plan(directories, NULL, legacy = copy_output)
  for (target in names(plan)) {
    sources <- directories[[target]][["include"]]
    if (!length(sources)) next
    # These paths were cleared for this build. Existing files belong to a script.
    safe_paths <- if (output_run && clear_output != "never") c(TRUE, FALSE) else TRUE
    for (safe in safe_paths) {
      root <- projr_path_get_dir(target, safe = safe, create = FALSE, format = "absolute")
      paths <- file.path(root, sources)
      conflict <- paths[file.exists(paths) | fs::link_exists(paths)]
      if (length(conflict)) {
        stop("Rendered output conflicts with a directory include: ", conflict[1L], call. = FALSE)
      }
    }
  }

  if (copy_output) {
    # Without receiver-side includes, preserve the existing copy order.
    if (!length(plan)) .build_copy_dir(output_run)
    .build_copy_to_unsafe(output_run)
    .build_copy_pkg(output_run)
  }
  # Include the source directories' licenses in every published copy.
  .license_dir_create_all_post(output_run)
  if (length(plan)) .build_copy_includes(plan, directories, output_run)

  invisible(copy_output)
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
    c(intersect("data", names(.yml_dir_get(NULL)))) |>
    unique()

  for (x in label_vec_output) {
    # Get source directory without creating it
    source_dir <- projr_path_get_dir(x, safe = TRUE, create = FALSE)

    # Skip if source directory doesn't exist or is empty
    if (!dir.exists(source_dir)) {
      next
    }

    # Check if directory has any files
    files_in_source <- list.files(source_dir, all.files = TRUE, no.. = TRUE)
    if (length(files_in_source) == 0) {
      next
    }

    # Get destination directory (will be created by .dir_move if needed)
    dest_dir <- projr_path_get_dir(x, safe = FALSE)

    # Move files from safe to unsafe directory
    .dir_move(source_dir, dest_dir)

    .cli_info("Copied {x} from {source_dir} to {dest_dir}")
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
  label_list <- .build_copy_pkg_get_label()

  for (x in label_list) {
    pkg_files <- .build_copy_pkg_build_path_get() |>
      .file_ls(full.names = TRUE)
    dest_path <- projr_path_get_dir(x[[1]], "pkg", safe = !output_run)

    fs::file_copy(pkg_files, dest_path)
    .cli_info("Copied built package to {dest_path}")
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
  label_vec <- .build_copy_dir_get_label()

  for (x in label_vec) {
    output_vec <- .yml_dir_get_output_nm_complete(x, NULL)

    for (i in seq_along(output_vec)) {
      source_path <- projr_path_get_dir(x, safe = !output_run)
      dest_path <- projr_path_get_dir(output_vec[[i]], x, safe = !output_run)
      dir_exc <- .build_label_get_dir_exc(x)

      .dir_copy_exact(
        source_path,
        dest_path,
        dir_exc = dir_exc
      )
    }
  }

  invisible(TRUE)
}

.build_copy_dir_get_label <- function() {
  # Return all directory labels that can be copied
  # Excludes "code" and "project" which are not meant for copying
  # The actual filtering by output configuration happens in .build_copy_dir()
  .opt_dir_get_label_get(NULL)
}
