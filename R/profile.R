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
#' then made equal to its value.
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
#' @return Invisibly returns the new projr profile.
#' @seealso projr_profile_create_local,projr_yml_get
#' @export
#'
projr_profile_create <- function(profile = NULL,
                                 silent = FALSE) {
  # get and validate profile
  # --------------------------

  if (Sys.getenv("PROJR_TEST") == "TRUE") {
    silent <- TRUE
  }
  .assert_flag_full(silent, required = TRUE)
  profile <- profile %||% Sys.getenv("PROJR_PROFILE")
  .assert_string(profile, required = TRUE)
  .assert_opt_not_single(profile, opt = c("default", "local"), required = TRUE)
  .projr_profile_check(profile)
  file_ind <- file.exists(
    .projr_dir_proj_get(paste0("_projr-", profile, ".yml"))
  )
  if (file_ind) {
    message(paste0("projr profile ", profile, " already exists"))
    return(invisible(FALSE))
  }

  yml_projr_root <- .projr_yml_get_root_full()
  key_ind_dir <- paste0("directories-", profile) %in% names(yml_projr_root)
  key_ind_build <- paste0("build-", profile) %in% names(yml_projr_root)

  # create profile settings
  # -------------------------

  yml_projr_root_default <- .projr_yml_get_root_default()
  yml_projr_root_full <- .projr_yml_get_root_full()

  yaml::write_yaml(
    .projr_list_elem_as_null(yml_projr_root_default),
    file = .projr_dir_proj_get(paste0("_projr-", profile, ".yml"))
  )

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
#' @param overwrite logical.
#' If `TRUE`, then overwrite `_projr-local.yml` if it already exists.
#' Default is `FALSE`.
#'
#' @details
#' Note that if any setting in `_projr-local.yml` is empty,
#' then a lower-precendence file's setting
#' (i.e. from `_projr-<profile>.yml` or `_projr.yml`) is used.
#' Empty settings are by default indicated by `~`.
#' @seealso projr_profile_create_local,projr_yml_get
projr_profile_create_local <- function(overwrite = FALSE) {
  projr_profile_create_local_check(overwrite)
  .projr_profile_create_local_actual()
  .projr_profile_create_local_ignore()
  invisible(TRUE)
}

projr_profile_create_local_check <- function(overwrite) {
  if (!overwrite && file.exists(.projr_dir_proj_get("_projr-local.yml"))) {
    stop("overwrite = TRUE and _projr-local.yml already exists", call. = FALSE)
  }
  invisible(TRUE)
}

.projr_profile_create_local_actual <- function() {
  yml_projr_root_default <- .projr_yml_get_root_default()
  yml_projr_local <- yml_projr_root_default[c("directories", "build")]
  yml_projr_local <- .projr_list_elem_as_null(yml_projr_local)
  yaml::write_yaml(yml_projr_local, file = .projr_dir_proj_get("_projr-local.yml"))
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

.projr_profile_create_local_ignore <- function() {
  .projr_profile_create_local_ignore_git()
  .projr_profile_create_local_ignore_rbuild()
}
.projr_profile_create_local_ignore_git <- function() {
  gitignore <- readLines(.projr_dir_proj_get(".gitignore"))
  if (!"_projr-local.yml" %in% gitignore) {
    writeLines(
      c(gitignore, "_projr-local.yml"),
      .projr_dir_proj_get(".gitignore")
    )
    .projr_newline_append(.projr_dir_proj_get(".gitignore"))
  }
}
.projr_profile_create_local_ignore_rbuild <- function() {
  rbuildignore <- readLines(
    .projr_dir_proj_get(".Rbuildignore")
  )
  if (!"^_projr-local\\.yml$" %in% rbuildignore) {
    writeLines(
      c(rbuildignore, "^_projr-local\\.yml$"),
      .projr_dir_proj_get(".Rbuildignore")
    )
    .projr_newline_append(.projr_dir_proj_get(".Rbuildignore"))
  }
}

.projr_profile_check <- function(x) {
  if (nzchar(gsub("\\w|-|_", "", x))) {
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
  Sys.getenv("PROJR_PROFILE", unset = "default")
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
  .projr_profile_delete_check(profile)
  .projr_profile_delete_actual(profile)
}

.projr_profile_delete_check <- function(profile) {
  .assert_given(profile, "profile")
  .assert_string(profile, "profile")
  if (.is_opt(profile, "default")) {
    stop("Cannot delete profile named 'default' as it is used internally.")
  }
}

.projr_profile_delete_actual <- function(profile) {
  path_fn <- .projr_dir_proj_get(paste0("_projr-", profile, ".yml"))
  if (!file.exists(path_fn)) {
    message("No such profile detected: ", profile)
    return(invisible(FALSE))
  }
  invisible(file.remove(path_fn))
}

#' @title Delete local `projr` settings file.
#'
#' @description Deletes `_projr-local.yml` file.
projr_profile_delete_local <- function() {
  projr_profile_delete("local")
}
