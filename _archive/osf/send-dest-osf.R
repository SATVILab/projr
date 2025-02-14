.send_dest_osf <- function(bump_component) {
  # check if `osf` is used, and install if it is
  # and not installed
  # -----------------------------------------------
  yml.osf <-.yml_get()[["build"]][["osf"]]

  if (is.null(yml.osf)) {
    return(invisible(FALSE))
  }

  if (!requireNamespace("osfr", quietly = TRUE)) {
    renv::install("osfr", prompt = FALSE)
    .dep_add("osfr")
  }

  # loop over osf entries
  # -----------------------------------------------
  for (i in seq_along(yml.osf)) {
    yml.osf_ind <- yml.osf[[i]]
  }
}
