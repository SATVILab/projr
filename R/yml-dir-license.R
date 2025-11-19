#' Set license for a directory
#'
#' @description
#' Set license configuration for a specific directory label in `_projr.yml`.
#' Licenses are automatically generated during builds.
#'
#' @param type character.
#' License type. Supported types: "CC-BY", "CC0", "Apache-2.0", "MIT", "Proprietary".
#' Common variations are also accepted (e.g., "ccby", "apache", "mit").
#'
#' @param label character.
#' Directory label (e.g., "output", "raw-data", "docs", "cache").
#'
#' @param authors character vector.
#' Authors or copyright holders. If NULL, attempts to get from DESCRIPTION file
#' or uses "Project Authors" as default.
#'
#' @param year integer.
#' Copyright year. If NULL, uses current year.
#'
#' @param profile character.
#' Profile to use. Default is "default".
#'
#' @return
#' Invisible TRUE if license was set successfully.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple format - just license type
#' projr_yml_dir_license_set("CC-BY", "output")
#'
#' # Full format with custom authors and year
#' projr_yml_dir_license_set(
#'   "MIT",
#'   "raw-data",
#'   authors = c("Jane Doe", "John Smith"),
#'   year = 2024
#' )
#'
#' # Set license for multiple directories
#' projr_yml_dir_license_set("Apache-2.0", "output")
#' projr_yml_dir_license_set("Apache-2.0", "docs")
#' }
projr_yml_dir_license_set <- function(type,
                                      label,
                                      authors = NULL,
                                      year = NULL,
                                      profile = "default") {
  .assert_string(type, TRUE)
  .assert_string(label, TRUE)
  .assert_chr(authors)
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1) {
      stop("year must be a single numeric value", call. = FALSE)
    }
  }
  .assert_string(profile)

  # Validate license type
  type <- .license_type_normalize(type)

  # Create license config
  if (is.null(authors) && is.null(year)) {
    # Simple format
    license_config <- type
  } else {
    # Full format
    license_config <- list(type = type)
    if (!is.null(authors)) {
      license_config[["authors"]] <- authors
    }
    if (!is.null(year)) {
      license_config[["year"]] <- year
    }
  }

  # Set in yml
  .yml_dir_set_license(license_config, label, profile)

  invisible(TRUE)
}

#' Get license configuration for a directory
#'
#' @description
#' Get license configuration for a specific directory label from `_projr.yml`.
#'
#' @param label character.
#' Directory label (e.g., "output", "raw-data", "docs", "cache").
#'
#' @param profile character.
#' Profile to use. Default is "default".
#'
#' @return
#' License configuration (character or list) or NULL if no license is configured.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get license for output directory
#' projr_yml_dir_license_get("output")
#' }
projr_yml_dir_license_get <- function(label, profile = "default") {
  .assert_string(label, TRUE)
  .assert_string(profile)

  .yml_dir_get_license(label, profile)
}

#' Remove license configuration for a directory
#'
#' @description
#' Remove license configuration for a specific directory label from `_projr.yml`.
#'
#' @param label character.
#' Directory label (e.g., "output", "raw-data", "docs", "cache").
#'
#' @param profile character.
#' Profile to use. Default is "default".
#'
#' @return
#' Invisible TRUE if license configuration was removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove license configuration for output directory
#' projr_yml_dir_license_rm("output")
#' }
projr_yml_dir_license_rm <- function(label, profile = "default") {
  .assert_string(label, TRUE)
  .assert_string(profile)

  .yml_dir_set_license(NULL, label, profile)

  invisible(TRUE)
}
