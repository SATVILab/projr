#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param dir_proj character.
#' Project directory.
#' Default is \code{getwd()}.
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
#' @export
projr_init <- function(dir_proj = getwd(),
                       yml_path_from = NULL,
                       renv_force = FALSE,
                       renv_bioconductor = TRUE) {
  # create initial _proj.yml
  .projr_init_yml(dir_proj, yml_path_from)

  # package name
  nm_pkg <- basename(dir_proj)
  if (!Sys.getenv("PROJR_TEST") == "TRUE") {
    cat("Project name is", paste0("`", nm_pkg, "`"), "\n")
  }

  nm_list_init <- .projr_init_prompt_init(nm_pkg)

  # DESCRIPTION file
  .projr_init_description(dir_proj, nm_list_init)

  # add various files
  .projr_init_dep() # _dependencies.R
  .projr_init_ignore() # ignore files
  .projr_init_r() # R folder
  .projr_init_license(nm_list_init) # license

  # add readme
  readme_list <- .projr_init_readme(nm_list_init)

  # add document-engine docs
  .projr_init_engine(nm_list_init)

  # renv
  .projr_init_renv(renv_force, renv_bioconductor)

  # git and github
  .projr_init_git_rdme_and_gh(
    readme = readme_list[["readme"]],
    path_readme = readme_list[["path_readme"]],
    nm_list = nm_list_init
  )

  invisible(TRUE)
}
