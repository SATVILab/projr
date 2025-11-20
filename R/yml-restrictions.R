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
#' }
#'
#' @param branch logical or character.
#' Controls which branches can perform builds.
#' If `TRUE`, builds allowed on any branch (default).
#' If a character vector, builds only allowed on these branches.
#' If `FALSE`, builds restricted on all branches.
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
#' }
#'
projr_yml_restrictions_set <- function(branch = NULL,
                                       profile = "default") {
  .yml_restrictions_set_check(branch = branch, profile = profile)

  if (!is.null(branch)) {
    .yml_restrictions_set_branch(branch, profile)
  }

  invisible(TRUE)
}

.yml_restrictions_set_check <- function(branch, profile) {
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

.yml_restrictions_get <- function(profile) {
  .yml_get(profile)[["build"]][["restrictions"]]
}

.yml_restrictions_set <- function(yml_restrictions, profile) {
  .yml_build_set_nm(yml_restrictions, "restrictions", profile)
}
