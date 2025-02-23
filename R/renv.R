#' @title Test renv restore
#'
#' @description
#' Tests `renv::restore()` without using the cache in
#' a clean, temporary environment.
#' Automatically creates a temporary project directory, initializes `renv`,
#' copies required files, disables the cache via
#' `.Rprofile`, and then performs `renv::restore()`.
#' Afterwards, it deletes the first library path where `renv` restored packages.
#'
#' **Note:**
#' To ensure isolation, the test runs in a directory that is completely separate
#' from the parent project and executes `Rscript` with the `--vanilla` option.
#' The `--vanilla` flag seems essential to prevent the project
#' `renv` settings from being affected by testing.
#'
#' @param files_to_copy character vector.
#' Paths to files to copy into the temporary directory before restoring.
#' Note that `renv.lock` is always copied.
#' @param delete_lib Logical.
#' If `TRUE`, the restored library path is deleted after the test.
#' Default is `TRUE`.
#'
#' @return `TRUE` if `renv::restore()` succeeds, `FALSE` otherwise.
#' @export
projr_renv_test <- function(files_to_copy = NULL, delete_lib = TRUE) {
  # Creates a temporary environment to test renv::restore()
  # without using the cache.
  # Initializes renv, copies files, disables cache, and runs renv::restore().
  # Optionally deletes the restored library path afterwards.

  working_dir <- tempfile("projr_renv_test_")
  dir.create(working_dir, recursive = TRUE, showWarnings = FALSE)

  wd_old <- getwd()
  on.exit(setwd(wd_old), add = TRUE)
  setwd(working_dir)
  cli::cli_alert_info(paste0("Working directory: ", working_dir))

  .renv_rest_init()
  .renv_rest_activate()

  .dir_copy_file(files_to_copy |> union("renv.lock"), wd_old, working_dir)

  .renv_rest_disable_cache()

  success <- .renv_rest_restore()

  if (delete_lib) {
    .renv_rest_cleanup_lib()
  }

  success
}


#----------------- Sub-Functions -----------------#

.renv_rest_init <- function() {
  # Initializes renv in the current working directory using Rscript.
  # Stops execution on failure.

  cmd <- paste0("renv::init(); message(renv::project())")
  res <- .renv_rest_run_rscript(cmd, FALSE, TRUE)

  if (!res$success) {
    stop("Failed to initialize renv: ", res$error)
  }
}

.renv_rest_activate <- function() {
  # Activates renv in the current working directory using Rscript.
  # Stops execution on failure.

  working_dir <- getwd()
  cmd <- paste0(
    "renv::activate('", working_dir, "'); message(renv::project())"
  )
  res <- .renv_rest_run_rscript(cmd, FALSE, TRUE)

  if (!res$success) {
    stop("Failed to activate renv: ", res$error)
  }
}

.renv_test_test_snapshot <- function() {
  # Creates a _dependencies.R file, installs 'tinytest',
  # and snapshots the environment.
  # Uses Rscript to run commands under renv. Stops on failure.

  working_dir <- getwd()
  writeLines(
    "library(tinytest)\nrenv::install('tinytest')\n",
    "_dependencies.R"
  )
  cmd <- paste0(
    "renv::activate(project = '", working_dir, "');",
    "renv::install('tinytest', project = '", working_dir, "', prompt = FALSE);",
    "renv::snapshot(project = '", working_dir, "', prompt = FALSE)"
  )
  res <- .renv_rest_run_rscript(cmd, FALSE, TRUE)

  if (!res$success) {
    stop("Failed to run commands: ", res$error)
  }
}

.renv_test_test_lockfile_create <- function(file_path, bad = FALSE) {
  # Creates a renv.lock file for testing.
  # If 'bad' is TRUE, sets an unrealistic package version and hash.

  current_r_version <- R.version$major
  current_r_minor_version <- R.version$minor
  full_version <- paste(current_r_version, current_r_minor_version, sep = ".")
  renv_lock <- list(
    R = list(
      Version = full_version,
      Repositories = list(
        list(Name = "CRAN", URL = "https://p3m.dev/cran/latest"),
        list(Name = "PPM", URL = "https://p3m.dev/cran/latest"),
        list(Name = "RSPM", URL = "https://p3m.dev/cran/latest")
      )
    ),
    Packages = list(
      tinytest = list(
        Package = "tinytest",
        Version = if (bad) "1000000000000.0.0" else "1.4.1",
        Source = "Repository",
        Repository = "CRAN",
        Requirements = c("R", "parallel", "utils"),
        Hash = if (bad) {
          "1f344373b4f0fe61b6zzzze3d3c842ca"
        } else {
          "1f344373b4f0fe61b6a0d4e3d3c842ca"
        }
      )
    )
  )

  jsonlite::write_json(
    renv_lock,
    path = file_path, pretty = TRUE, auto_unbox = TRUE
  )
}

.renv_rest_copy_files <- function(files_to_copy) {
  # Removes existing files and copies specified files (including renv.lock)
  # into the working directory.
  # Stops if file operations fail.

  files_to_copy <- union(files_to_copy, "renv.lock")

  # Note: the snippet references 'files_in_dir' but it isn't defined.
  # If needed, define or implement removal logic here.
  # i.e. .file_rm(files_in_dir)

  if (!is.null(files_to_copy) && length(files_to_copy) > 0) {
    .dir_copy_file(fn = files_to_copy, path_dir_to = getwd())
  }
}

.renv_rest_disable_cache <- function() {
  # Disables the renv cache by appending a setting to
  # .Rprofile in the current directory.
  # Stops execution on failure.

  append_success <- tryCatch(
    {
      cat(
        "renv::settings$use.cache(FALSE)\n",
        file = ".Rprofile", append = TRUE
      )
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (!append_success) {
    stop("Failed to modify .Rprofile to disable renv cache.")
  }
}

.renv_rest_restore <- function() {
  # Runs renv::restore() via Rscript with --vanilla in the current directory.
  # Returns TRUE if successful, FALSE otherwise.

  wd <- getwd()
  cmd <- paste0(
    "renv::activate('", wd, "'); ",
    "renv::restore(project = '", wd, "', prompt = FALSE, rebuild = TRUE)"
  )
  res <- .renv_rest_run_rscript(cmd, vanilla = TRUE)

  if (!res$success) {
    cli::cli_alert_danger("renv::restore() failed.")
    message("res$error")
    return(FALSE)
  } else {
    cli::cli_alert_success("renv::restore() successful.")
  }

  res$success
}

.renv_rest_cleanup_lib <- function() {
  # Deletes the first library path from .libPaths()
  # using Rscript with renv activated.
  # Stops on failure.

  wd <- getwd()
  cmd <- paste0(
    "renv::activate('", wd, "'); ",
    "first_lib <- message(.libPaths()[1]); ",
    "if (dir.exists(first_lib)) {",
    "unlink(first_lib, recursive = TRUE, force = TRUE);",
    "message('Deleted library: ', first_lib);",
    "} else {",
    "message('Library path does not exist and cannot be deleted: ', first_lib);", # nolint
    "}"
  )
  res <- .renv_rest_run_rscript(cmd, FALSE, TRUE)

  if (!res$success) {
    stop("Failed to delete library: ", res$error)
  }
}

.renv_rest_run_rscript <- function(expr, message_path = TRUE, vanilla = FALSE) {
  # Runs the given R expression in a separate Rscript process.
  # If `vanilla` is TRUE, uses --vanilla.
  # If `message_path` is TRUE, logs stdout/stderr file paths.
  # Returns a list with 'success' and 'error'.

  rscript <- .path_rscript_get()

  if (rscript == "") {
    return(list(success = FALSE, error = "Rscript executable not found."))
  }

  stdout_file <- tempfile("rscript_stdout_")
  stderr_file <- tempfile("rscript_stderr_")

  if (message_path) {
    cli::cli_alert_info(paste0("stdout_file: ", stdout_file))
    cli::cli_alert_info(paste0("stderr_file: ", stderr_file))
  }

  if (vanilla) {
    args <- c("--vanilla", "-e", shQuote(expr))
  } else {
    args <- c("-e", shQuote(expr))
  }

  res <- tryCatch(
    {
      system2(
        command = rscript,
        args = args,
        stdout = stdout_file,
        stderr = stderr_file
      )
    },
    error = function(e) {
      return(-1)
    }
  )

  if (res != 0) {
    error_msg <- tryCatch(
      {
        paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
      },
      error = function(e) {
        "Unknown error."
      }
    )
    return(list(success = FALSE, error = error_msg))
  }

  list(success = TRUE, error = NULL)
}

.generate_random_string <- function(length = 10) {
  # Generates a random string of a given length (default 10)
  # consisting of letters and digits.

  chars <- c(letters, LETTERS, 0:9)
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

.ensure_cli <- function() {
  # Ensures that the 'cli' package is available.
  # If not installed, attempts to install it using renv.

  if (!requireNamespace("cli", quietly = TRUE)) {
    try(renv::install("cli", prompt = FALSE))
  }
}

.ensure_biocmanager <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    try(renv::install("BiocManager", prompt = FALSE))
  }
}

.check_renv <- function() {
  # Checks if 'renv' is installed.
  # Stops execution if not available.

  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("The 'renv' package is required but not installed.")
  }
}


#' @title Restore or Update renv Lockfile Packages
#'
#' @description
#' Functions to manage the restoration and updating of packages specified in the `renv` lockfile.
#'
#' - .renv_restore()`: Restores packages from the lockfile, attempting to install the lockfile versions.
#' - .renv_update()`: Updates packages to their latest available versions, ignoring the lockfile versions.
#' - .renv_restore_and_update()`: First restores packages from the lockfile, then updates them to the latest versions.
#'
#' @details
#' Control whether to process GitHub packages, non-GitHub packages (CRAN and Bioconductor), or both using the `github` and `non_github` arguments.
#'
#' @param github Logical. Whether to process GitHub packages. Default is `TRUE`.
#' @param non_github Logical. Whether to process non-GitHub packages (CRAN and Bioconductor). Default is `TRUE`.
#' @param biocmanager_install Logical.
#' If `TRUE`, Bioconductor packages will be installed using `BiocManager::install`; otherwise,
#' `renv::install("bioc::<package_name>")` will be used.
#' Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Restore all packages
#' projr_renv_restore()
#'
#' # Update all packages
#' projr_renv_update()
#'
#' # Restore and then update all packages
#' projr_renv_restore_and_update()
#'
#' # Only restore non-GitHub packages
#' projr_renv_restore(github = FALSE)
#'
#' # Only update GitHub packages
#' projr_renv_update(non_github = FALSE)
#' }
#'
#' @export
#' @rdname projr_renv_restore
projr_renv_restore <- function(github = TRUE,
                               non_github = TRUE,
                               biocmanager_install = FALSE) {
  # Restores packages from the lockfile, installing specified versions.

  .check_renv()
  .ensure_cli()

  cli::cli_h1("Starting renv environment restoration")

  package_list <- .renv_lockfile_pkg_get()
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = TRUE,
    biocmanager_install = biocmanager_install
  )
  cli::cli_h1("renv environment restoration completed")
  invisible(TRUE)
}

#' @export
#' @rdname projr_renv_restore
projr_renv_update <- function(github = TRUE,
                              non_github = TRUE,
                              biocmanager_install = FALSE) {
  # Updates packages to their latest versions, ignoring the lockfile.

  .check_renv()
  .ensure_cli()

  cli::cli_h1("Starting renv environment update")

  package_list <- .renv_lockfile_pkg_get()
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = FALSE,
    biocmanager_install = biocmanager_install
  )
  cli::cli_h1("renv environment update completed")
  invisible(TRUE)
}

#' @export
#' @rdname projr_renv_restore
projr_renv_restore_and_update <- function(github = TRUE,
                                          non_github = TRUE,
                                          biocmanager_install = FALSE) {
  # First restores packages to the lockfile versions, then updates them to the latest versions.

  .renv_restore(github, non_github, biocmanager_install)
  .renv_update(github, non_github, biocmanager_install)
}

.renv_lockfile_pkg_get <- function() {
  # Reads the current renv lockfile, extracts and categorizes packages as regular, Bioconductor, or GitHub.
  # Returns a list of package vectors.

  renv::activate()
  lockfile_list_pkg <- renv::lockfile_read()$Package
  pkg_vec_regular <- character()
  pkg_vec_bioc <- character()
  pkg_vec_gh <- character()

  for (package_name in names(lockfile_list_pkg)) {
    package_info <- lockfile_list_pkg[[package_name]]
    remote_username <- package_info$RemoteUsername
    source <- tolower(package_info$Source)

    if (is.null(remote_username)) {
      is_bioc <- grepl("bioc", source)
      if (is_bioc) {
        pkg_vec_bioc <- c(pkg_vec_bioc, package_name)
      } else {
        pkg_vec_regular <- c(pkg_vec_regular, package_name)
      }
    } else {
      pkg_vec_gh <- c(pkg_vec_gh, paste0(remote_username, "/", package_name))
    }
  }

  list(
    regular = pkg_vec_regular,
    bioc = pkg_vec_bioc,
    gh = pkg_vec_gh
  )
}

.renv_restore_or_update_impl <- function(package_list,
                                         github,
                                         non_github,
                                         restore,
                                         biocmanager_install) {
  # Internal helper for.renv_restore and.renv_update.
  # Calls the appropriate wrapper for CRAN, Bioconductor, and GitHub packages
  # based on whether restore or update operations are requested.

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["regular"]],
    act = non_github,
    restore = restore,
    source = "CRAN",
    biocmanager_install = biocmanager_install
  )

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["bioc"]],
    act = non_github,
    restore = restore,
    source = "Bioconductor",
    biocmanager_install = biocmanager_install
  )

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["gh"]],
    act = github,
    restore = restore,
    source = "GitHub",
    biocmanager_install = biocmanager_install
  )
  invisible(TRUE)
}

.renv_restore_or_update_actual_wrapper <- function(pkg,
                                                   act,
                                                   restore,
                                                   source,
                                                   biocmanager_install) {
  # Wraps the restore/update operations for a given source type (CRAN, Bioconductor, GitHub).
  # If 'act' is TRUE, proceed. Otherwise, skip.

  if (length(pkg) == 0L) {
    cli::cli_alert_info("No {source} packages to process.")
    return(invisible(FALSE))
  }

  if (act) {
    action <- if (restore) "Restoring" else "Installing latest" # nolint: object_usage_linter.
    cli::cli_alert_info("{action} {source} packages.")
    .renv_restore_update_impl(
      pkg,
      restore,
      biocmanager_install,
      is_bioc = (source == "Bioconductor")
    )
  } else {
    action <- if (restore) "restoring" else "installing"
    cli::cli_alert_info("Skipping {action} {source} packages.")
  }
}

.renv_restore_update_impl <- function(pkg, restore, biocmanager_install, is_bioc) {
  # Performs the actual restore or update of packages.
  # On restore, attempts renv::restore() and checks for missing packages.
  # On update, installs the latest versions.
  # Also attempts to install remaining missing packages after the main operation.

  if (length(pkg) == 0L) {
    return(invisible(FALSE))
  }

  .ensure_cli()

  pkg_type <- if (is_bioc) { # nolint: object_usage_linter.
    "Bioconductor"
  } else if (all(grepl("/", pkg))) {
    "GitHub"
  } else {
    "CRAN"
  }
  pkg_names <- sapply(pkg, function(x) sub("^.*/", "", x))

  if (restore) {
    cli::cli_alert_info("Attempting to restore {pkg_type} packages: {.pkg {pkg_names}}")
    tryCatch(
      renv::restore(
        packages = pkg_names, transactional = FALSE, prompt = FALSE
      ),
      error = function(e) {
        cli::cli_alert_danger("Failed to restore {pkg_type} packages: {.pkg {pkg_names}}. Error: {e$message}")
      }
    )
    cli::cli_alert_info("Checking for packages that failed to restore.")
    .renv_restore_remaining(pkg_names)
  } else {
    cli::cli_alert_info("Installing latest {pkg_type} packages: {.pkg {pkg_names}}")
    .renv_install(pkg, biocmanager_install, is_bioc)
  }

  cli::cli_alert_info("Checking for packages that are still not installed.")
  .renv_install_remaining(pkg, biocmanager_install, is_bioc)
  invisible(TRUE)
}

.renv_restore_remaining <- function(pkg) {
  # Checks for packages that failed to restore and tries restoring them individually.

  .ensure_cli()

  installed_pkgs <- rownames(installed.packages())
  pkg_remaining <- pkg[!pkg %in% installed_pkgs]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages restored successfully.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning("Packages that failed to restore: {.pkg {pkg_remaining}}")
  cli::cli_alert_info("Attempting to restore packages individually.")

  for (x in pkg_remaining) {
    if (!requireNamespace(x, quietly = TRUE)) {
      tryCatch(
        renv::restore(packages = x, transactional = FALSE),
        error = function(e) {
          cli::cli_alert_danger("Failed to restore package: {.pkg {x}}. Error: {e$message}")
        }
      )
    }
  }
}

.renv_install <- function(pkg, biocmanager_install, is_bioc) {
  # Installs packages using renv. If is_bioc and biocmanager_install are TRUE, uses BiocManager::install().
  # Otherwise, uses renv::install(). Logs errors on failure.

  .ensure_cli()

  if (is_bioc) {
    if (biocmanager_install) {
      .ensure_biocmanager()
      cli::cli_alert_info("Installing Bioconductor packages using BiocManager: {.pkg {pkg}}")
      tryCatch(
        BiocManager::install(pkg, update = TRUE, ask = FALSE),
        error = function(e) {
          cli::cli_alert_danger("Failed to install Bioconductor packages using BiocManager: {.pkg {pkg}}. Error: {e$message}")
        }
      )
    } else {
      cli::cli_alert_info("Installing Bioconductor packages using renv: {.pkg {pkg}}")
      tryCatch(
        renv::install(paste0("bioc::", pkg), prompt = FALSE),
        error = function(e) {
          cli::cli_alert_danger("Failed to install Bioconductor packages via renv: {.pkg {pkg}}. Error: {e$message}")
        }
      )
    }
  } else {
    cli::cli_alert_info("Installing packages: {.pkg {pkg}}")
    tryCatch(
      renv::install(pkg, prompt = FALSE),
      error = function(e) {
        cli::cli_alert_danger("Failed to install packages: {.pkg {pkg}}. Error: {e$message}")
      }
    )
  }
}

.renv_install_remaining <- function(pkg, biocmanager_install, is_bioc) {
  # Checks for packages still missing after the initial restore/update process.
  # Attempts to install them again, and if they remain missing, tries one by one.

  .ensure_cli()

  installed_pkgs <- rownames(utils::installed.packages())
  pkg_remaining <- pkg[
    !sapply(pkg, function(x) sub("^.*/", "", x)) %in% installed_pkgs
  ]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages are installed.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning("Packages that are still missing: {.pkg {pkg_remaining}}")
  cli::cli_alert_info("Attempting to install remaining packages.")
  .renv_install(pkg_remaining, biocmanager_install, is_bioc)

  pkg_still_missing <- pkg_remaining[
    !sapply(pkg_remaining, function(x) {
      requireNamespace(sub("^.*/", "", x), quietly = TRUE)
    })
  ]

  if (length(pkg_still_missing) == 0L) {
    cli::cli_alert_success("All remaining packages installed successfully.")
    return(invisible(TRUE))
  }

  cli::cli_alert_warning("Packages that failed to install: {.pkg {pkg_still_missing}}")
  cli::cli_alert_info("Attempting to install missing packages individually.")

  for (x in pkg_still_missing) {
    if (!requireNamespace(sub("^.*/", "", x), quietly = TRUE)) {
      .renv_install(x, biocmanager_install, is_bioc)
    }
  }

  pkg_final_missing <- pkg_still_missing[
    !sapply(pkg_still_missing, function(x) {
      requireNamespace(sub("^.*/", "", x), quietly = TRUE)
    })
  ]

  if (length(pkg_final_missing) == 0L) {
    cli::cli_alert_success("All packages installed successfully after individual attempts.")
  } else {
    cli::cli_alert_danger("Some packages failed to install: {.pkg {pkg_final_missing}}")
  }
}
