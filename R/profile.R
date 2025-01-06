#' @title Add projr profile file
#'
#' @description
#' Creates a new `projr` profile that can override
#' settings in `_projr.yml`.
#' If the associated file does not exists, it
#' creates a blank file.
#' The file is ignored from the R build process and, if
#' it is the `local` profile, from Git as well.
#'
#' @param profile character.
#' Name of the profile.
#' If not supplied, then the profile is named `default`
#' (and the file `_projr.yml` is created).
#'
#' @return
#' Invisibly returns `TRUE` if the file was created,
#' and `FALSE` if the file already exists.
#' 
#' @seealso projr_profile_create_local,projr_profile_get
#' @export
projr_profile_create <- function(profile) {
  .assert_len_1(profile)
  if (missing(profile) || profile == "default") {
    profile_spec <- ""
  } else {
    .assert_string(profile)
    profile_spec <- paste0("-", profile)
  }
  path_file <- paste0("_projr", profile_spec, ".yml") |>
    .dir_proj_get()
  .projr_ignore_rbuild_set(basename(path_file), "ignore")
  if (profile == "local") {
    .projr_profile_create_local_ignore_git()
  }
  if (file.exists(path_file)) {
    message(paste0("File ", path_file, " already exists"))
    return(invisible(FALSE))
  }
  file.create(path_file)

  message(paste0(
    "Created file ", basename(path_file), " in root of project"
  ))

  invisible(TRUE)
}

#' @title Create a local `projr` profile
#'
#' @description Create a `projr` profile with highest precedence
#' (i.e. its settings overwrite any others) that is
#' ignored by Git.
#'
#' @details
#' Note that if any setting in `_projr-local.yml` is empty,
#' then a lower-precendence file's setting
#' (i.e. from `_projr-<profile>.yml` or `_projr.yml`) is used.
#' Empty settings are by default indicated by `~`.
#' @seealso projr_profile_create_local,projr_yml_get
projr_profile_create_local <- function() {
  projr_profile_create(local)
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
  gitignore <- readLines(.dir_proj_get(".gitignore"))
  if (!"_projr-local.yml" %in% gitignore) {
    writeLines(
      c(gitignore, "_projr-local.yml"),
      .dir_proj_get(".gitignore")
    )
    .projr_newline_append(.dir_proj_get(".gitignore"))
  }
}
.projr_profile_create_local_ignore_rbuild <- function() {
  rbuildignore <- readLines(
    .dir_proj_get(".Rbuildignore")
  )
  if (!"^_projr-local\\.yml$" %in% rbuildignore) {
    writeLines(
      c(rbuildignore, "^_projr-local\\.yml$"),
      .dir_proj_get(".Rbuildignore")
    )
    .projr_newline_append(.dir_proj_get(".Rbuildignore"))
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
#' Get active \code{projr} profile(s).
#'
#' @return
#' Character vector of length equal to number of active profiles.
#' 
#' @details 
#' Note that `local` and `default` are not returned, but are always active (if 
#' they exist).
#' `local` corresponds to `_projr-local.yml` and `default` to `_projr.yml`.
#'
#' @export
projr_profile_get <- function() {
  .projr_profile_get_var()
}

.projr_profile_get_var <- function() {
  # get explicitly specified PROJR_PROFILES
  # (other than "default")
  projr_profile <- .projr_profile_get_var_unsplit()
  if (!nzchar(projr_profile)) {
    return(character())
  }
  projr_profile_vec <- strsplit(projr_profile, ",")[[1]]
  vapply(projr_profile_vec, trimws, character(1)) |>
    setdiff(c("default", "local", "")) |>
    stats::setNames(NULL)
}

.projr_profile_get_var_unsplit <- function() {
  .projr_profile_get_raw() |>
    setdiff(c("default", "local"))
}

.projr_profile_get_raw <- function() {
  Sys.getenv("PROJR_PROFILE")
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
  .assert_string(profile, TRUE)
  if (.is_opt(profile, "default")) {
    stop("Cannot delete profile named 'default' as it is used internally.")
  }
}

.projr_profile_delete_actual <- function(profile) {
  path_fn <- .dir_proj_get(paste0("_projr-", profile, ".yml"))
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
