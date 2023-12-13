.projr_yml_renv_get <- function() {
  renv_setting <- .projr_yml_build_get_renv()
  if (!is.null(renv_setting)) {
    return(renv_setting)
  }
  invisible(TRUE)
}
