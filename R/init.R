#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param yml_path_from character.
#' Path to YAML file to use as `_projr.yml`.
#' If not supplied, then default `_projr.yml`
#' file is used.
#'
#' @param renv_force Logical.
#' Passed to `renv::init()`.
#' If \code{FALSE}, then `renv::init()` will not run
#' if it detects that the working directory
#' already is registered with renv.
#' Default is \code{FALSE}.
#' @param renv_bioconductor Logical.
#' Whether \code{renv} should look for packages
#' on Bioconductor.
#' Default is \code{TRUE}.
#' @param public logical.
#' Whether the GitHub repo created (if any)
#' is public or not.
#' Default is `FALSE`.
#' @seealso projr_init_renviron
#' @export
projr_init <- function(yml_path_from = NULL,
                       renv_force = FALSE,
                       renv_bioconductor = TRUE,
                       public = FALSE) {
  # create initial _proj.yml
  .projr_init_yml(yml_path_from) # nolint: object_usage_linter.

  # get metadata from user
  nm_list <- .projr_init_prompt_init()

  # DESCRIPTION file
  .projr_init_description(nm_list)

  # add document-engine docs
  .projr_init_engine(nm_list)
  
  # add various files
  .projr_init_dep() # _dependencies.R
  .projr_init_ignore() # ignore files
  .projr_init_r() # R folder
  .projr_init_license(nm_list) # license

  # initialise readme
  .projr_init_readme(nm_list)

  # renv
  .projr_init_renv(force = renv_force, bioc = renv_bioconductor)

  # finalise README
  .projr_readme_render()

  # add citation files
  .projr_init_cite(nm_list[["answer_readme"]])

  # initialise Git repo
  .projr_init_git_init(nm_list[["answer_git"]])

  # create GitHub remote
  .projr_init_github(username = nm_list[["gh"]], public = public)


  # create github remote
  invisible(TRUE)
}

#' @export 
#' @rdname projr_
projr_init_git <- function(commit = TRUE, username = NULL, public = FALSE) {
  .projr_git_system_setup()
  .projr_init_git_git(commit)
  if (!is.null(username)) {
    .projr_init_git_github(username, public)
  }
}

.projr_init_git_git <- function(commit) {
  # initialise Git repo
  .projr_init_git_init(answer_git = 1L, commit = commit)

}

.projr_init_git_github <- function(username,
                                   public) {
  if (!.projr_git_remote_check_exists()) {
    .projr_dep_install_only("usethis")
    if (!.projr_git_gh_check_auth()) {
      stop(call. = FALSE)
    }
    .projr_init_github_actual(username, public)
  }
}

#' @export 
#' @rdname projr_init
projr_init_license <- function(license, first_name, last_name) {
  if (tolower(license) == "proprietary") {
    .assert_string(first_name, TRUE)
    .assert_string(last_name, TRUE)
  }
  .projr_dep_install_only("usethis")
  .projr_init_license_create_actual(license, first_name, last_name)
}

projr_init_ignore <- function() {
  projr_ignore_auto()
}

projr_init_renv <- function(force = FALSE, bioc = TRUE) {
  .projr_dep_install_only("renv")
  .projr_init_renv(force, bioc, FALSE)
}