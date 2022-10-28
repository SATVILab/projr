#' @title Add current working directory
#'
#' @return Invisibly returns the working directory.
projr_profile_add <- function(silent = FALSE) {
  if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    wd <- Sys.getenv("PROJR_PROFILE")
  } else {
    wd <- normalizePath(getwd(), winslash = "/")
  }

  yml <- yaml::read_yaml("_projr.yml")

  yml_dir <- yml[["directories-default"]]
  for (i in seq_along(yml_dir)) {
    yml_dir_curr <- yml_dir[[i]][-which(names(yml_dir[[i]]) %in% c("ignore"))]
    yml_dir_curr$path <- ""
    yml_dir[[i]] <- yml_dir_curr
  }
  yml <- yml |>
    append(
      list(yml_dir) |>
        stats::setNames(paste0("directories-", wd))
    )

  yaml::write_yaml(yml, "_projr.yml")
  if (!silent) {
    message(paste0("Added the following user to _projr.yml: ", wd))
  }

  invisible(wd)
}
