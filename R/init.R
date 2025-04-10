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
#' @seealso.init_renviron
#' @export
projr_init_prompt <- function(y ) {
  # create initial _proj.yml
  .init_yml(yml_path_from) # nolint: object_usage_linter.

  # get metadata from user
  nm_list <- .init_prompt_init()

  # DESCRIPTION file
  .init_description(nm_list)

  # add document-engine docs
  .init_engine(nm_list)

  # add various files
  .init_dep() # _dependencies.R
  .init_ignore() # ignore files
  .init_r() # R folder
  .init_license(nm_list) # license

  # initialise readme
  .init_readme(nm_list)

  # renv
  .init_renv(force = renv_force, bioc = renv_bioconductor)

  # finalise README
  .readme_render()

  # add citation files
  .init_cite(nm_list[["answer_readme"]])

  # initialise Git repo
  .init_git_init(nm_list[["answer_git"]])

  # create GitHub remote
  .init_github(username = nm_list[["gh"]], public = public)


  # create github remote
  invisible(TRUE)
}


.init_git_git <- function(commit) {
  # initialise Git repo
  .init_git_init(answer_git = 1L, commit = commit)
}

.init_git_github <- function(username,
                             public) {
  if (!.git_remote_check_exists()) {
    .dep_install_only("usethis")
    if (!.git_gh_check_auth()) {
      stop(call. = FALSE)
    }
    .init_github_impl(username, public)
  }
}

#' @export
#' @rdname projr_init
projr_init_license <- function(license, first_name, last_name) {
  if (tolower(license) == "proprietary") {
    .assert_string(first_name, TRUE)
    .assert_string(last_name, TRUE)
  }
  .dep_install_only("usethis")
  .init_license_create_impl(license, first_name, last_name)
}

projr_init_ignore <- function() {
  projr_ignore_auto()
}
