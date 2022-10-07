#' @title Add current working directory
#'
#' @param wd_var character.
#' Name of environment variable to determine
#' the current user/computer.
#' If this variable exists, then it determines
#' the user/computer.
#' If it does not, then the project root
#' determins the user/computer (which is
#' the directory containing DESCRIPTION).
#'
#' @return Invisibly returns the working directory.
projr_usr_add <- function(wd_var = "PROJR_WORKING_DIRECTORY",
                          silent = FALSE) {
  if (nzchar(Sys.getenv(wd_var))) {
    wd <- Sys.getenv(wd_var)
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
