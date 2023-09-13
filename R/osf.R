.projr_osf_upload <- function(output_run) {
  # consider early exit
  # ------------------

  if (!.projr_osf_check_run(output_run)) {
    return(invisible(FALSE))
  }

  # uploads
  # ------------------

  if (!requireNamespace("osfr", quietly = TRUE)) {
    renv::install("osfr", prompt = FALSE)
    .projr_dep_add("osfr")
  }
  for (i in seq_along(projr_yml_get()[["build"]][["osf"]])) {
    
  }
}

.projr_osf_check_run <- function(output_run) {
  yml_projr <- projr_yml_get()
  # either a dev run or else no osf upload specified
  if ((!output_run) ||
    (!"osf" %in% names(yml_projr[["build"]]))) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}
