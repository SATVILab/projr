#' @title Add current working directory
#'
#' @return Invisibly returns the working directory.
#' @export
projr_profile_create <- function(silent = FALSE) {

  projr_profile <- projr_profile_get()

  if (!projr_profile == "default") {
    stop(paste0(
      "projr_profile ", projr_profile,
      " already exists in _projr.yml. ",
      "To create a new profile anyway, ",
      "first run Sys.setenv('PROJR_PROFILE` = '<new_profile>'."
    ))
  }
  yml <- .projr_yml_get()

  yml_dir <- yml[["directories-default"]]
  for (i in seq_along(yml_dir)) {
    yml_dir_curr <- yml_dir[[i]][-which(names(yml_dir[[i]]) %in% c("ignore"))]
    yml_dir_curr$path <- ""
    yml_dir[[i]] <- yml_dir_curr
  }
  yml <- yml |>
    append(
      list(yml_dir) |>
        stats::setNames(paste0("directories-", projr_profile))
    )

  .projr_yml_set(yml) {

  if (!silent) {
    message(paste0("Added the following profile: ", projr_profile))
  }

  invisible(wd)
}

#' @title
#'
#' @description
#' Get active \code{projr} profile.
#'
#' @return
#' Character vector of length 1
#' corresponding to active \code{projr} profile.
#'
#' @export
projr_profile_get <- function() {
  if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    projr_profile <- Sys.getenv("PROJR_PROFILE")
  } else {
    projr_profile <- normalizePath(getwd(), winslash = "/")
  }
  yml <- .projr_yml_get()
  if (paste0("directories-", projr_profile) %in% names(yml)) {
    return(projr_profile)
  }
  "default"
}
