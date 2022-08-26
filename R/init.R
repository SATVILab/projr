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

  invisible(TRUE)
}
