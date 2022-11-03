#' @title Add projr profile
#'
#' @description
#' Adds either the environment variable "PROJR_PROFILE"
#' (if set) or the current project directory (i.e.
#' where the DESCRIPTION file is)
#' as a new projr profile.
#'
#' @return Invisibly returns the new projr profile.
#' @export
projr_profile_create <- function(silent = FALSE) {
  if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    projr_profile <- Sys.getenv("PROJR_PROFILE")
  } else {
    projr_profile <- rprojroot::is_r_package$find_file()
  }

  yml_projr <- .projr_yml_get()
  if (paste0("directories-", projr_profile) %in% names(yml_projr)) {
    message(
      "projr profile ", projr_profile,
      " already exists in _projr.yml. ",
      "To create a new profile anyway, ",
      "first run Sys.setenv('PROJR_PROFILE` = '<new_profile>'."
    )
    return(invisible(projr_profile))
  }

  yml_dir <- yml_projr[["directories-default"]]
  for (i in seq_along(yml_dir)) {
    yml_dir_curr <- yml_dir[[i]][-which(names(yml_dir[[i]]) %in% c("ignore"))]
    yml_dir_curr$path <- ""
    yml_dir[[i]] <- yml_dir_curr
  }
  yml_projr <- yml_projr |>
    append(
      list(yml_dir) |>
        stats::setNames(paste0("directories-", projr_profile))
    )

  .projr_yml_set(yml_projr)

  if (!silent) {
    message(paste0("Added the following profile: ", projr_profile))
  }

  invisible(projr_profile)
}

#' @title Get active projr profile
#'
#' @description
#' Get active \code{projr} profile.
#' First looks for `\code{Sys.getenv("PROJR_PROFILE")}`
#' and then, if that is empty, takes the current
#' project directory
#' (as returned by \code{rprojroot::is_r_package()$find_file())},
#' i.e. the folder containing the file \code{DESCRIPTION}.
#' If there is no entry in \code{_projr.yml} matching the above selected
#' profile, then the profile \code{"default"} is returned.
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
    projr_profile <- rprojroot::is_r_package$find_file()
  }
  yml <- .projr_yml_get()
  if (paste0("directories-", projr_profile) %in% names(yml)) {
    return(projr_profile)
  }
  "default"
}

#' @title Delete a projr profile from _projr.yml
#'
#' @description
#' Deletes a \code{projr} profile from \code{_projr.yml}.
#'
#' @param profile character.
#' \code{projr} profile to delete.
#' If not supplied, then no profile is deleted.
#'
#' @return \code{invisible(TRUE)}.
#'
#' @export
projr_profile_delete <- function(profile) {
  if (missing(profile)) {
    return(invisible(TRUE))
  }
  if (!is.character(profile)) {
    stop("profile must be of type character.")
  }
  if (profile == "default") {
    return(invisible(TRUE))
  }
  yml_projr <- .projr_yml_get()
  yml_projr <- yml_projr[
    !names(yml_projr) == paste0("directories-", profile)
  ]
  .projr_yml_set(yml_projr)
  invisible(TRUE)
}
