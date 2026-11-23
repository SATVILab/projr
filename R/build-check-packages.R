#' @title Check if required packages for build are available
#'
#' @description
#' Checks if all packages required for the current project build are installed.
#' Returns structured information about missing packages and installation commands.
#'
#' This is particularly useful in CI/CD environments where the installation
#' commands need to be captured and executed programmatically.
#'
#' @param profile character.
#' `projr` profile to use. Default is NULL (use current profile).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{available}{Logical. TRUE if all required packages are installed, FALSE otherwise.}
#'   \item{missing}{Character vector of missing package names. Empty if all packages are available.}
#'   \item{install_cmds}{Character vector of R commands to install missing packages. Empty if all packages are available.}
#'   \item{message}{Character. Human-readable message about package status.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check packages for current project
#' pkg_status <- projr_build_check_packages()
#'
#' if (!pkg_status$available) {
#'   cat("Missing packages:", paste(pkg_status$missing, collapse = ", "), "\n")
#'   cat("Install commands:\n")
#'   cat(paste(pkg_status$install_cmds, collapse = "\n"), "\n")
#'
#'   # In CI/CD, you could execute:
#'   # for (cmd in pkg_status$install_cmds) {
#'   #   eval(parse(text = cmd))
#'   # }
#' }
#' }
projr_build_check_packages <- function(profile = NULL) {
  restore_library <- .build_renv_library_start()
  on.exit(restore_library())
  scripts <- .scripts_get(NULL, "patch", profile)
  engines <- .engines_get(scripts)
  missing <- .build_renv_missing_renderers(engines, inspect = FALSE)
  .build_check_packages(profile, names(engines), missing)
}

.build_check_packages <- function(
  profile,
  engines,
  render_missing = character()
) {
  # Collect all packages that will be needed during the build
  pkg_required <- character(0)

  # Check for packages needed by build engines
  pkg_required <- c(
    pkg_required,
    .build_check_packages_engine(profile, engines)
  )

  # Check for packages needed by remote destinations
  pkg_required <- c(pkg_required, .build_check_packages_remote(profile))

  # Check packages required for citations
  pkg_required <- c(pkg_required, .build_check_packages_citations(profile))

  # Remove duplicates
  pkg_required <- unique(pkg_required)

  # return if no packages are required
  if (length(pkg_required) == 0L) {
    return(list(
      available = TRUE,
      missing = character(0),
      install_cmds = character(0),
      message = "No additional packages are required for the build."
    ))
  }

  # Check which packages are missing
  pkg_missing <- pkg_required[
    vapply(
      pkg_required,
      function(x) !requireNamespace(x, quietly = TRUE),
      logical(1)
    )
  ]

  if (length(render_missing)) {
    # A renderer can be installed and fail to load because an import is missing.
    wrappers <- intersect(pkg_missing, c("quarto", "bookdown"))
    installed <- basename(find.package(
      wrappers,
      lib.loc = .libPaths(),
      quiet = TRUE
    ))
    return(.build_renv_render_status(union(
      render_missing,
      setdiff(pkg_missing, installed)
    )))
  }

  # If all packages are available
  if (length(pkg_missing) == 0L) {
    return(list(
      available = TRUE,
      missing = character(0),
      install_cmds = character(0),
      message = "All required packages are installed."
    ))
  }

  # Create installation commands
  install_cmds <- .dep_get_install_cmds(pkg_missing)

  # Build human-readable message
  if (length(pkg_missing) == 1L) {
    msg <- paste0(
      "Required package '",
      pkg_missing,
      "' is not installed.\n",
      "Install it using:\n  ",
      install_cmds
    )
  } else {
    msg <- paste0(
      "Required packages are not installed: ",
      paste(pkg_missing, collapse = ", "),
      "\n",
      "Install them using:\n  ",
      paste(install_cmds, collapse = "\n  ")
    )
  }

  list(
    available = FALSE,
    missing = pkg_missing,
    install_cmds = install_cmds,
    message = msg
  )
}

# Internal helper functions for package checking
# These are updated versions that accept a profile parameter

.build_check_packages_engine <- function(
  profile = NULL,
  engines = .engine_get_auto()
) {
  packages <- c(
    bookdown = "bookdown",
    quarto_project = "quarto",
    quarto_document = "quarto",
    rmd = "rmarkdown"
  )
  required <- unname(packages[intersect(engines, names(packages))])
  if (.build_readme_rmd_render_check(output_run = TRUE)) {
    required <- c(required, "rmarkdown")
    if (.build_readme_rmd_render_detect_pkg_use()) {
      required <- c(required, "pkgbuild")
    }
  }
  unique(required)
}

.build_check_packages_remote <- function(profile = NULL) {
  pkg_needed <- character(0)

  # Get list of remote destinations
  remote_vec <- .remote_ls()
  if (length(remote_vec) == 0L) {
    return(pkg_needed)
  }

  # Check which packages are needed for each remote type
  for (remote in remote_vec) {
    if (remote == "github") {
      pkg_needed <- c(pkg_needed, "gh", "httr")
    }
  }

  unique(pkg_needed)
}

.build_check_packages_citations <- function(profile = NULL) {
  packages <- c(cff = "cffr", codemeta = "codemeta", "inst-citation" = "cffr")
  unique(unname(packages[.build_cite_get_yml(profile)]))
}
