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
  # Collect all packages that will be needed during the build
  pkg_required <- character(0)

  # Check for packages needed by build engines
  pkg_required <- c(pkg_required, .build_check_packages_engine(profile))

  # Check for packages needed by remote destinations
  pkg_required <- c(pkg_required, .build_check_packages_remote(profile))

  # Remove duplicates
  pkg_required <- unique(pkg_required)

  # Check which packages are missing
  pkg_missing <- pkg_required[
    vapply(pkg_required, function(x) !requireNamespace(x, quietly = TRUE), logical(1))
  ]

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
      "Required package '", pkg_missing, "' is not installed.\n",
      "Install it using:\n  ",
      install_cmds
    )
  } else {
    msg <- paste0(
      "Required packages are not installed: ", paste(pkg_missing, collapse = ", "), "\n",
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

.build_check_packages_engine <- function(profile = NULL) {
  pkg_needed <- character(0)

  # Determine engine from project configuration
  engine <- .engine_get()

  # If no engine detected (empty character), return early
  if (length(engine) == 0 || !nzchar(engine)) {
    return(pkg_needed)
  }

  # Map engine to required packages
  pkg_needed <- switch(engine,
    "bookdown" = "bookdown",
    "quarto_project" = "quarto",
    "quarto_document" = "quarto",
    "rmd" = "rmarkdown",
    character(0)
  )

  unique(pkg_needed)
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
      pkg_needed <- c(pkg_needed, "piggyback", "gh")
    } else if (remote == "osf") {
      pkg_needed <- c(pkg_needed, "osfr")
    }
  }

  unique(pkg_needed)
}
