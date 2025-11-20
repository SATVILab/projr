# restrictions
# -----------------

#' @rdname yml-restrictions
#' @title Set build restrictions
#' @export
#'
#' @description
#'
#' `projr_yml_restrictions_set` sets build restrictions in `_projr.yml`.
#'
#' The options are:
#' \itemize{
#'  \item branch: controls which branches can perform builds.
#'    If `TRUE` (default), builds are allowed on any branch.
#'    If a character vector, builds are only allowed on matching branches.
#'    If `FALSE`, builds are restricted (treated as empty character vector).
#'  \item not_behind: whether to check if branch is behind remote upstream.
#'    If `TRUE` (default), build fails if branch is behind remote.
#'    If `FALSE`, no check is performed.
#' }
#'
#' @param branch logical or character.
#' Controls which branches can perform builds.
#' If `TRUE`, builds allowed on any branch (default).
#' If a character vector, builds only allowed on these branches.
#' If `FALSE`, builds restricted on all branches.
#' If `NULL`, setting is not changed.
#' Default is `NULL`.
#' @param not_behind logical.
#' Controls whether to check if branch is behind remote upstream.
#' If `TRUE` (default), build fails if branch is behind.
#' If `FALSE`, no check is performed.
#' If `NULL`, setting is not changed.
#' Default is `NULL`.
#' @param profile character.
#' The profile to write to.
#' Default is "default", in which case it writes to `_projr.yml`.
#'
#' @examples
#' \dontrun{
#' # Allow builds on any branch (default)
#' projr_yml_restrictions_set(branch = TRUE)
#'
#' # Allow builds only on main and dev branches
#' projr_yml_restrictions_set(branch = c("main", "dev"))
#'
#' # Restrict builds on all branches
#' projr_yml_restrictions_set(branch = FALSE)
#'
#' # Disable check for being behind remote
#' projr_yml_restrictions_set(not_behind = FALSE)
#'
#' # Enable check for being behind remote (default)
#' projr_yml_restrictions_set(not_behind = TRUE)
#' }
#'
projr_yml_restrictions_set <- function(branch = NULL,
                                       not_behind = NULL,
                                       profile = "default") {
  .yml_restrictions_set_check(branch = branch, not_behind = not_behind, profile = profile)

  if (!is.null(branch)) {
    .yml_restrictions_set_branch(branch, profile)
  }

  if (!is.null(not_behind)) {
    .yml_restrictions_set_not_behind(not_behind, profile)
  }

  invisible(TRUE)
}

.yml_restrictions_set_check <- function(branch, not_behind, profile) {
  if (!is.null(branch)) {
    if (!is.logical(branch) && !is.character(branch)) {
      stop("branch must be logical or character")
    }
    if (is.logical(branch)) {
      .assert_flag(branch)
    }
    if (is.character(branch)) {
      .assert_chr(branch, required = TRUE)
    }
  }
  if (!is.null(not_behind)) {
    .assert_flag(not_behind)
  }
  .assert_string(profile)
}

.yml_restrictions_set_branch <- function(branch, profile) {
  # Convert FALSE to empty list (YAML will serialize this as [])
  if (isFALSE(branch)) {
    branch <- list()
  }

  # Get current restrictions
  yml_restrictions <- .yml_restrictions_get(profile) %||% list()

  # Set or remove branch restriction
  if (isTRUE(branch)) {
    # TRUE means no restriction - remove the key
    yml_restrictions[["branch"]] <- NULL
  } else {
    yml_restrictions[["branch"]] <- branch
  }

  # Set restrictions in YAML
  if (length(yml_restrictions) == 0) {
    # Remove entire restrictions section if empty
    .yml_restrictions_set(NULL, profile)
  } else {
    .yml_restrictions_set(yml_restrictions, profile)
  }

  invisible(TRUE)
}

.yml_restrictions_set_not_behind <- function(not_behind, profile) {
  # Get current restrictions
  yml_restrictions <- .yml_restrictions_get(profile) %||% list()

  # Set or remove not_behind restriction
  if (isTRUE(not_behind)) {
    # TRUE is the default - remove the key
    yml_restrictions[["not_behind"]] <- NULL
  } else {
    yml_restrictions[["not_behind"]] <- not_behind
  }

  # Set restrictions in YAML
  if (length(yml_restrictions) == 0) {
    # Remove entire restrictions section if empty
    .yml_restrictions_set(NULL, profile)
  } else {
    .yml_restrictions_set(yml_restrictions, profile)
  }

  invisible(TRUE)
}

.yml_restrictions_get_branch <- function(profile) {
  yml_restrictions <- .yml_restrictions_get(profile)
  if (is.null(yml_restrictions)) {
    return(TRUE)  # Default: no restrictions
  }
  if (is.null(yml_restrictions[["branch"]])) {
    return(TRUE)  # Default: no restrictions
  }
  branch <- yml_restrictions[["branch"]]
  # Convert empty list to empty character vector for consistency
  if (is.list(branch) && length(branch) == 0) {
    return(character(0))
  }
  branch
}

.yml_restrictions_get_not_behind <- function(profile) {
  yml_restrictions <- .yml_restrictions_get(profile)
  if (is.null(yml_restrictions)) {
    return(TRUE)  # Default: check enabled
  }
  if (is.null(yml_restrictions[["not_behind"]])) {
    return(TRUE)  # Default: check enabled
  }
  yml_restrictions[["not_behind"]]
}

.yml_restrictions_get <- function(profile) {
  .yml_get(profile)[["build"]][["restrictions"]]
}

.yml_restrictions_set <- function(yml_restrictions, profile) {
  .yml_build_set_nm(yml_restrictions, "restrictions", profile)
}
