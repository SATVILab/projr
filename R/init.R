#' @title Initialise project
#'
#' @description Initialise project
#'
#' @export
prj_init <- function(yml_path_from = NULL) {

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



  dir_projr <- here::here("projr")
  if (!dir.exists(dir_projr)) {
    dir.create(dir_projr, recursive = TRUE)
  }

  if (!file.exists("_bookdown.yml")) {
    file.copy(
      system.file(
        "project_structure",
        "_bookdown.yml",
        package = "projr"
      ),
      here::here()
    )
  }
  if (!file.exists("_output.yml")) {
    file.copy(
      system.file(
        "project_structure",
        "_output.yml",
        package = "projr"
      ),
      here::here()
    )
  }
  file.copy(
    system.file(
      "project_structure",
      "DELETE-AFTER-DOING.md",
      package = "projr"
    ),
    here::here()
  )

  if (!file.exists("DESCRIPTION")) {
    file.copy(
      system.file(
        "project_structure",
        "DESCRIPTION",
        package = "projr"
      ),
      here::here()
    )
  }

  if (!file.exists("index.Rmd")) {
    file.copy(
      system.file(
        "project_structure",
        "index.Rmd",
        package = "projr"
      ),
      here::here()
    )
  }

  if (!file.exists("appendix.Rmd")) {
    file.copy(
      system.file(
        "project_structure",
        "appendix.Rmd",
        package = "projr"
      ),
      here::here()
    )
  }

  usethis::use_git_ignore("DELETE-AFTER-DOING.md")
  usethis::use_build_ignore("DELETE-AFTER-DOING.md")

  if (!dir.exists("renv")) {
    renv::init()
  }

  message(
    "check, and edit if need be, _projr.yml before restarting R session (Ctrl + Shift + F10 in RStudio)" # nolint
  )
  message("Then follow steps in DELETE-AFTER-DOING.md")

  write(
    "projr::prj_activate()",
    file = ".Rprofile",
    append = TRUE
  )

  invisible(TRUE)
}
