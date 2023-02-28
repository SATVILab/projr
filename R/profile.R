#' @title Add projr profile
#'
#' @description
#' Creates a new `projr` profile that overrides
#' settings in `_projr.yml`.
#' The profile becomes active when either the working directory
#' (`rprojroot::is_r_package$find_file()`) or
#' `Sys.getenv("PROJR_PROFILE")` is equal to `profile`.
#'
#' @param profile character.
#' If not `NULL`, then this is the name of the profile.
#' If `NULL` and the environment variable `PROJR_PROFILE` is set,
#' then made equal to its value. If the variable is not set,
#' then made equal to the working directory.
#' @param method "key" or "file".
#' Specifies where the `projr` profile is specified.
#' If "key" (default), then the additional profile is added
#' as new keys in `_projr.yml` in the form `directories-<profile>` and
#' `build-<profile>`.
#' If "file", then the file `_projr-<profile>.yml` is created
#' with all-null values for the sub-keys of the `build` and `directories`
#' keys in `_projr.yml`.
#'
#' Default is `NULL`.
#'
#' @param silent logical.
#' If `TRUE`, then any messages are suppressed.
#' Default is `TRUE` is the environment
#' variable `PROJR_TEST` is `"TRUE"`.
#'
#' @details
#' Note that if any setting in `projr` profile-specific setting
#' is empty (indicated by `~` in the YAML file by default), then
#' the corresponding setting in `_projr.yml` is used.
#'
#' If using `method = "file"`, then the profile
#' cannot be automatically specified from the working directory
#' but must be specified by either `PROJR_PROFILE` or the `profile` argument.
#'
#' @return Invisibly returns the new projr profile.
#' @seealso projr_profile_create_local,projr_yml_get
#' @export
#'
projr_profile_create <- function(profile = NULL,
                                 method = "key",
                                 silent = Sys.getenv("PROJR_TEST") == "TRUE") {
  # get and validate profile
  # --------------------------

  if (!is.logical(silent)) {
    stop("`silent` parameter must be of type local.")
  }

  if (!is.null(profile)) {
    if (profile == "local") {
      stop(
        "Cannot create profile named 'local' in this way.
        Use `projr_profile_create_local()` instead."
      )
    }
    if (profile == "default") {
      stop("Cannot create profile named 'default' as it is used internally.")
    }
  } else if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    profile <- Sys.getenv("PROJR_PROFILE")
  } else {
    if (method == "file") {
      stop("The projr profile cannot be the working directory
      if settings method = `file`.")
    }
    profile <- rprojroot::is_r_package$find_file()
  }
  if (!method %in% c("key", "file")) {
    stop("method must be either 'key' or 'file'")
  }
  if (identical(sort(as.character(method)), sort(c("key", "file")))) {
    stop("method must be either 'key' or 'file' and not both")
  }
  if (method == "file") {
    .projr_profile_check(profile)
  }
  yml_projr_root <- .projr_yml_get_root_full()
  key_ind_dir <- paste0("directories-", profile) %in% names(yml_projr_root)
  key_ind_build <- paste0("build-", profile) %in% names(yml_projr_root)
  file_ind <- file.exists(
    file.path(
      rprojroot::is_r_package$find_file(),
      paste0("_projr-", profile, ".yml")
    )
  )
  if (any(key_ind_dir, key_ind_build, file_ind)) {
    if (!silent) {
      message("projr profile ", profile, " already exists")
    }
    return(invisible(FALSE))
  }

  # create profile settings
  # -------------------------

  yml_projr_root_default <- .projr_yml_get_root_default()
  yml_projr_root_full <- .projr_yml_get_root_full()
  if (method == "key") {
    yml_final <- yml_projr_root_full |>
      append({
        yml_projr_add <- .projr_list_elem_as_null(yml_projr_root_default)
        yml_projr_add |>
          stats::setNames(paste0(names(yml_projr_add), "-", profile))
      })
    yaml::write_yaml(
      yml_final,
      file.path(rprojroot::is_r_package$find_file(), "_projr.yml")
    )
  } else {
    yaml::write_yaml(
      .projr_list_elem_as_null(yml_projr_root_default),
      file = file.path(
        rprojroot::is_r_package$find_file(),
        paste0("_projr-", profile, ".yml")
      )
    )
  }

  if (!silent) {
    message(paste0("Added the following profile: ", profile))
  }


  invisible(profile)
}

#' @title Create a local `projr` profile
#'
#' @description Create a `projr` profile with highest precedence
#' (i.e. its settings overwrite any others) that is
#' ignored by Git.
#' Useful to avoid bloat through many entries in `_projr.yml`
#' or extra `_projr-<profile>.yml` files.
#' Creates a file `_projr-local.yml` with empty settings and ignores it from
#' `.Rbuildignore` and `.gitignore`.
#'
#' @details
#' Note that if any setting in `_projr-local.yml` is empty,
#' then a lower-precendence file's setting
#' (i.e. from `_projr-<profile>.yml` or `_projr.yml`) is used.
#' Empty settings are by default indicated by `~`.
#' @seealso projr_profile_create_local,projr_yml_get
projr_profile_create_local <- function() {
  path_fn <- file.path(
    rprojroot::is_r_package$find_file(), "_projr-local.yml"
  )
  if (file.exists(path_fn)) {
    stop("_projr-local.yml already exists")
  }
  yml_projr_root_default <- .projr_yml_get_root_default()
  yml_projr_local <- yml_projr_root_default[c("directories", "build")]
  yml_projr_local <- .projr_list_elem_as_null(yml_projr_local)
  yaml::write_yaml(yml_projr_local, file = path_fn)
  gitignore <- readLines(
    file.path(rprojroot::is_r_package$find_file(), ".gitignore")
  )
  if (!"_projr-local.yml" %in% gitignore) {
    writeLines(
      c(gitignore, "_projr-local.yml"),
      file.path(rprojroot::is_r_package$find_file(), ".gitignore")
    )
  }
  rbuildignore <- readLines(
    file.path(rprojroot::is_r_package$find_file(), ".Rbuildignore")
  )
  if (!"^_projr-local\\.yml$" %in% rbuildignore) {
    writeLines(
      c(rbuildignore, "^_projr-local\\.yml$"),
      file.path(rprojroot::is_r_package$find_file(), ".Rbuildignore")
    )
  }

  invisible(TRUE)
}

.projr_list_elem_as_null <- function(x) {
  lapply(x, function(elem) {
    if (!is.list(elem)) {
      return(NULL)
    }
    .projr_list_elem_as_null(elem)
  }) |>
    stats::setNames(names(x))
}

.projr_profile_check <- function(x) {
  x <- gsub("\\w", "", x)
  x <- gsub("-", "", x)
  if (nzchar(x)) {
    stop("If projr profile settings are kept in a file,
    then the profile can only use alphumeric characters,
    the underscore and the hyphen.")
  }
  invisible(TRUE)
}

#' @title Get active projr profile
#'
#' @description
#' Get active \code{projr} profile.
#' For each setting, preference is given if specified
#' in `_projr-local.yml` and, failing that, in `_projr-<profile>.yml`
#' (or in the `build-<profile>` and `directories-<profile>` keys).
#' Note that the active `projr` profile is only
#' equal to the `PROJR_PROFILE` environment variable
#' if the latter is set and the corresponding profile exists
#' (either as keys  in `_projr.yml` or as a file `_projr-<profile>.yml`).
#'
#' @return
#' Character vector of length 1
#' corresponding to active \code{projr} profile.
#' If "default" is returned, then no profile is active.
#' Note that any `_projr-local.yml` file will
#' always overwrite profile-specific and default settings.
#'
#' @export
projr_profile_get <- function() {
  yml_projr <- .projr_yml_get_root_full()
  if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    projr_profile <- Sys.getenv("PROJR_PROFILE")
    key_ind <- paste0("directories-", projr_profile) %in% names(yml_projr)
    file_ind <- file.exists(
      file.path(paste0("_projr-", projr_profile, ".yml"))
    )
    if (key_ind || file_ind) {
      return(projr_profile)
    }
  }
  projr_profile <- rprojroot::is_r_package$find_file()
  key_ind_dir <- paste0("directories-", projr_profile) %in% names(yml_projr)
  key_ind_build <- paste0("build-", projr_profile) %in% names(yml_projr)
  if (key_ind_dir || key_ind_build) {
    return(projr_profile)
  }
  "default"
}

#' @title Delete a projr profile from _projr.yml
#'
#' @description
#' Deletes a \code{projr} profile from \code{_projr.yml}
#' and/or its corresponding `_projr-<profile>.yml` file.
#'
#' @param profile character.
#' \code{projr} profile to delete.
#'
#' @return \code{invisible(TRUE)}.
#'
#' @export
projr_profile_delete <- function(profile) {
  if (missing(profile)) {
    stop("profile not specified")
  }
  if (!is.character(profile)) {
    stop("profile must be of type character.")
  }
  if (profile == "default") {
    return(invisible(FALSE))
  }

  # delete
  # -----------------

  # key
  yml_projr_root <- .projr_yml_get_root_default()
  dir_ind <- which(
    names(yml_projr_root) == paste0("directories-", profile)
  )
  if (length(dir_ind) > 0) {
    yml_projr_root <- yml_projr_root[-dir_ind]
  }
  build_ind <- which(
    names(yml_projr_root) == paste0("build-", profile)
  )
  if (length(build_ind) > 0) {
    yml_projr_root <- yml_projr_root[-build_ind]
  }
  .projr_yml_set(yml_projr_root)

  # file
  path_fn <- file.path(
    rprojroot::is_r_package$find_file(),
    paste0("_projr-", profile, ".yml")
  )
  if (file.exists(path_fn)) {
    file.remove(path_fn)
  }

  invisible(TRUE)
}

#' @title Delete local `projr` settings file.
#'
#' @description Deletes `_projr-local.yml` file.
projr_profile_delete_local <- function() {
  path_fn <- file.path(
    rprojroot::is_r_package$find_file(),
    paste0("_projr-local.yml")
  )
  if (file.exists(path_fn)) {
    file.remove(path_fn)
  }
  invisible(TRUE)
}
