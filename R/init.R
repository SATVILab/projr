#' @title Initialise project
#'
#' @description Initialise project
#'
#' @param renv_force. Logical.
#' Passed to `renv::init()`.
#' If \code{FALSE}, then `renv::init()` will not run
#' if it detects that the working directory
#' already is registered with renv.
#' Default is \code{FALSE}.
#' @param renv_bioconductor. Logical.
#' Whether \code{renv} should look for packages
#' on Bioconductor.
#' Default is \code{TRUE}.
#' @export
projr_init <- function(dir_proj = getwd(),
                       yml_path_from = NULL,
                       renv_force = FALSE,
                       renv_bioconductor = TRUE) {

  # create initial _proj.yml
  if (is.null(yml_path_from)) {
    yml_path_from <- system.file(
      "project_structure",
      "_projr.yml",
      package = "projr"
    )
  } else {
    if (!file.exists(yml_path_from)) {
      stop(paste0("yml_path_from does not exist: ", yml_path_from))
    }
  }

  file.copy(
    from = yml_path_from,
    to = "_projr.yml"
  )


  if (!file.exists("_bookdown.yml")) {
    file.copy(
      system.file(
        "project_structure",
        "_bookdown.yml",
        package = "projr"
      ),
      dir_proj
    )
  }

  if (!file.exists("_output.yml")) {
    file.copy(
      system.file(
        "project_structure",
        "_output.yml",
        package = "projr"
      ),
      dir_proj
    )
  }
  file.copy(
    system.file(
      "project_structure",
      "DELETE-AFTER-DOING.md",
      package = "projr"
    ),
    dir_proj
  )

  if (!file.exists("DESCRIPTION")) {
    file.copy(
      system.file(
        "project_structure",
        "DESCRIPTION",
        package = "projr"
      ),
      dir_proj
    )
  }

  if (!file.exists("index.Rmd")) {
    file.copy(
      system.file(
        "project_structure",
        "index.Rmd",
        package = "projr"
      ),
      dir_proj
    )
  }

  if (!file.exists("appendix.Rmd")) {
    file.copy(
      system.file(
        "project_structure",
        "appendix.Rmd",
        package = "projr"
      ),
      dir_proj
    )
  }

  if (!file.exists(".gitignore")) {
    file.copy(
      system.file(
        "project_structure",
        ".gitignore",
        package = "projr"
      ),
      dir_proj
    )
  }
  if (!file.exists(".Rbuildignore")) {
    file.copy(
      system.file(
        "project_structure",
        ".Rbuildignore",
        package = "projr"
      ),
      dir_proj
    )
  }

  cat(
    "\n^DELETE-AFTER-DOING\\.md$",
    file = ".Rbuildignore",
    sep = "",
    append = TRUE
  )

  cat(
    "\n",
    "projr::projr_activate()",
    "\n",
    file = ".Rprofile",
    sep = "",
    append = TRUE
  )

  renv::init(
    force = renv_force,
    bioconductor = renv_bioconductor
  )

  cat("\n")
  cat("\n")
  message("Follow steps in DELETE-AFTER-DOING.md")

  invisible(TRUE)
}
