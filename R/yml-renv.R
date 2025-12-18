# renv
# -----------------

#' @rdname yml-renv
#' @title Set renv snapshot options
#'
#' @description
#'
#' `projr_yml_renv_set` sets renv snapshot options for the project.
#'
#' The option controls whether `renv::snapshot()` is called
#' before and after project builds.
#'
#' If the setting is not present in `_projr.yml`,
#' then the default is `TRUE` (renv snapshots are performed).
#'
#' @param renv logical.
#' Whether to snapshot renv before and after builds.
#' If `NULL`, then setting is not changed.
#' Default is `NULL`.
#' @param simplify_default logical.
#' If `TRUE`, then if the setting is the same as the default
#' (which is `TRUE`),
#' then the setting is removed from `_projr.yml`.
#' Default is `TRUE`.
#' @param profile character.
#' Profile to add the setting to.
#' If `"default"` (the default),
#' the setting is added to the default profile,
#' which is `_projr.yml`.
#' If `NULL`, then the active profile is used
#' (i.e the merge of `_projr-local.yml`, `_projr-<profile>.yml`
#' and `_projr.yml`) and written to `_projr.yml`.
#' If another character vector, then the corresponding profile is used
#' and written to `_projr-<profile>.yml`.
#'
#' @examples
#' \dontrun{
#' # enable renv snapshots (default)
#' projr_yml_renv_set(TRUE)
#'
#' # disable renv snapshots
#' projr_yml_renv_set(FALSE)
#'
#' # revert to default (removes setting from YAML)
#' projr_yml_renv_set(TRUE, simplify_default = TRUE)
#' }
#' @export
projr_yml_renv_set <- function(renv = NULL,
                               simplify_default = TRUE,
                               profile = "default") {
  .yml_renv_set_check(renv = renv, simplify_default = simplify_default, profile = profile)

  if (!is.null(renv)) {
    .yml_renv_set_impl(renv, simplify_default, profile)
  }

  invisible(TRUE)
}

.yml_renv_set_check <- function(renv, simplify_default, profile) {
  .assert_flag(renv)
  .assert_flag(simplify_default, TRUE)
  .assert_string(profile)
}

.yml_renv_set_impl <- function(renv, simplify_default, profile) {
  # If setting to TRUE and simplify_default is TRUE, remove from YAML
  # (since TRUE is the default)
  if (isTRUE(renv) && simplify_default) {
    .yml_renv_set(NULL, profile)
    return(invisible(FALSE))
  }

  .yml_renv_set(renv, profile)
  invisible(TRUE)
}

.yml_renv_get <- function(profile) {
  yml_build <- .yml_build_get(profile)

  # Default to TRUE if not set
  if (is.null(yml_build[["renv"]])) {
    return(TRUE)
  }

  yml_build[["renv"]]
}

.yml_renv_set <- function(yml_renv, profile) {
  .yml_build_set_nm(yml_renv, "renv", profile)
}
