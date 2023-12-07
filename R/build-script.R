.projr_build_script_run_pre <- function() {
  # run pre-build script if it exists
  path <- .projr_build_script_get_pre()
  if (!is.null(path)) {
    source(path)
  }
}
