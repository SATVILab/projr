.reproduce_run <- function(version, scratch) {
  root <- getwd()
  method <- if (file.exists("renv.lock")) {
    "renv"
  } else if (file.exists("DESCRIPTION")) {
    "pak"
  } else {
    "session"
  }
  libs <- .libPaths()
  driver <- getNamespaceInfo(asNamespace("projr"), "path")
  library <- file.path(scratch, "library")
  cache <- Sys.getenv("RENV_PATHS_CACHE", renv::paths$root("cache"))
  vars <- c(
    R_LIBS = paste(libs, collapse = .Platform$path.sep),
    R_PROFILE_USER = file.path(root, ".Rprofile"),
    R_ENVIRON_USER = file.path(root, ".Renviron"),
    R_PROFILE = file.path(scratch, "no-site-profile"),
    R_ENVIRON = file.path(scratch, "no-site-environ"),
    R_DEFAULT_PACKAGES = NA_character_,
    R_TESTS = NA_character_,
    RENV_PROJECT = root,
    RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE",
    RENV_CONFIG_USER_PROFILE = "FALSE",
    RENV_PATHS_ROOT = file.path(scratch, "renv"),
    RENV_PATHS_LIBRARY = library,
    RENV_PATHS_CACHE = paste(
      file.path(scratch, "renv", "cache"),
      cache,
      sep = ";"
    ),
    PKG_CACHE_DIR = file.path(scratch, "pak"),
    PKG_SYSREQS = "false",
    PROJR_REPRODUCE_ISOLATED = if (method == "session") "FALSE" else "TRUE",
    PROJR_AUTO_INSTALL = "FALSE"
  )
  # An activated caller may have supplied absolute paths to its own lockfile,
  # library, or renv profile. Keep the caller's package cache available for reads.
  inherited <- grep(
    "^(RENV_PATHS_|RENV_PROFILE$|RENV_CONFIG_(EXTERNAL_LIBRARIES|REPOS_OVERRIDE)$|PKG_(METADATA|PACKAGE)_CACHE_DIR$)",
    names(Sys.getenv()),
    value = TRUE
  )
  clear <- setdiff(inherited, names(vars))
  old <- xfun::set_envvar(c(
    vars,
    stats::setNames(rep(NA_character_, length(clear)), clear)
  ))
  on.exit(xfun::set_envvar(old))

  # Development checkouts have no installed namespace for child renderers.
  if (!file.exists(file.path(driver, "Meta", "package.rds"))) {
    driver_lib <- file.path(scratch, "driver")
    fs::dir_create(driver_lib)
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      shQuote(c(
        "CMD",
        "INSTALL",
        "--no-test-load",
        "--no-multiarch",
        "--no-byte-compile",
        "--no-docs",
        "-l",
        driver_lib,
        driver
      )),
      stdout = TRUE,
      stderr = TRUE
    ))
    if (!is.null(attr(output, "status"))) {
      stop(
        "Could not install the replay driver:\n",
        paste(output, collapse = "\n"),
        call. = FALSE
      )
    }
    driver <- file.path(driver_lib, "projr")
    libs <- c(driver_lib, libs)
  }

  call <- function(fun, args) {
    environment(fun) <- baseenv()
    xfun::Rscript_call(fun, args, options = "--vanilla")
  }
  analysis_libs <- libs
  if (method != "session") {
    analysis_libs <- call(
      .reproduce_install,
      list(method, library, libs, getOption("repos"))
    )
  }
  call(.reproduce_worker, list(version, method, analysis_libs, driver, libs))
}

# Resolve dependencies in a separate process: pak and the renv used to restore
# the library may differ from the versions required when running the analysis.
.reproduce_install <- function(method, library, bootstrap_libs, repos) {
  .libPaths(bootstrap_libs, include.site = FALSE)
  if (method == "renv") {
    message("Restoring packages from renv.lock")
    library <- renv::paths$library(project = getwd())
    dir.create(library, recursive = TRUE, showWarnings = FALSE)
    renv::restore(
      project = getwd(),
      library = library,
      lockfile = "renv.lock",
      prompt = FALSE
    )
  } else {
    message(
      "Installing DESCRIPTION dependencies with pak. Package versions are unpinned."
    )
    if (!requireNamespace("pak", quietly = TRUE)) {
      stop(
        "Install pak to reproduce an analysis from DESCRIPTION.",
        call. = FALSE
      )
    }
    dir.create(library, recursive = TRUE, showWarnings = FALSE)
    .libPaths(library, include.site = FALSE)
    options(repos = repos)
    pak::local_install_deps(
      root = getwd(),
      lib = library,
      dependencies = TRUE,
      ask = FALSE
    )
  }
  library
}

.reproduce_worker <- function(
  version,
  method,
  analysis_libs,
  driver,
  bootstrap_libs
) {
  if (method == "session") {
    message(
      "No renv.lock or DESCRIPTION. Using available R libraries with unpinned versions."
    )
  }
  # Only the replay driver can use its installed dependencies as a fallback.
  # Analysis processes inherit the resolved libraries independently.
  .libPaths(c(analysis_libs, bootstrap_libs), include.site = FALSE)
  ns <- loadNamespace("projr", lib.loc = dirname(driver))
  child_libs <- paste(analysis_libs, collapse = .Platform$path.sep)
  Sys.setenv(
    R_LIBS = child_libs,
    R_LIBS_USER = child_libs,
    R_LIBS_SITE = "NULL"
  )
  get(".reproduce_render", envir = ns)(version)
  available <- utils::installed.packages(
    lib.loc = unique(c(analysis_libs, .Library)),
    noCache = TRUE
  )
  available <- available[!duplicated(available[, "Package"]), , drop = FALSE]
  packages <- data.frame(
    package = available[, "Package"],
    version = available[, "Version"],
    row.names = NULL
  )
  list(
    method = method,
    pinned = method == "renv",
    packages = packages[order(packages$package), , drop = FALSE]
  )
}
