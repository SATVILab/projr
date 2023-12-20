.projr_send_dest_osf <- function(bump_component) {
  # check if `osf` is used, and install if it is
  # and not installed
  # -----------------------------------------------
  yml_projr_osf <- projr_yml_get()[["build"]][["osf"]]

  if (is.null(yml_projr_osf)) {
    return(invisible(FALSE))
  }

  if (!requireNamespace("osfr", quietly = TRUE)) {
    renv::install("osfr", prompt = FALSE)
    .projr_dep_add("osfr")
  }

  # loop over osf entries
  # -----------------------------------------------
  for (i in seq_along(yml_projr_osf)) {
    yml_projr_osf_ind <- yml_projr_osf[[i]]
  }
}
