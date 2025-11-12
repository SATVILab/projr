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
#' @seealso.profile_create_local.profile_get
#' @export
projr_profile_create <- function(profile) {
  # Validate that profile is provided
  if (missing(profile)) {
    stop("profile parameter is required", call. = FALSE)
  }
  
  .assert_len_1(profile)
  .assert_string(profile, required = TRUE)
  
  # Reject protected profile names
  if (profile %in% c("local", "default")) {
    stop(
      "Cannot create profile named '", profile, 
      "' as it is reserved for internal use.",
      call. = FALSE
    )
  }
  
  # Validate profile name characters
  .profile_check(profile)
  
  # Create the profile file
  .profile_create(profile = profile, silent = TRUE)
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
#' @seealso.profile_create_local.yml_get
#' @export
projr_profile_create_local <- function() {
  .profile_create_local()
}

# Internal function to create a profile file
.profile_create <- function(profile, silent = TRUE) {
  # Validate input
  if (missing(profile)) {
    # Get profile from environment if not provided
    profile <- .profile_get_raw()
    if (!nzchar(profile)) {
      profile <- "default"
    }
  }
  
  # Validate silent parameter
  if (!is.logical(silent) || length(silent) != 1) {
    stop("silent must be a single logical value", call. = FALSE)
  }
  
  .assert_string(profile, required = TRUE)
  
  # Check if profile name is valid
  .profile_check(profile)
  
  # Create profile file
  profile_spec <- paste0("-", profile)
  path_file <- paste0("_projr", profile_spec, ".yml") |>
    .path_get()
  
  # Add to rbuildignore
  projr_ignore_file_rbuild(basename(path_file))
  
  # Check if file already exists
  if (file.exists(path_file)) {
    if (!silent) {
      message(paste0("projr profile ", profile, " already exists"))
    }
    return(invisible(FALSE))
  }
  
  # Create the file
  file.create(path_file)
  
  if (!silent) {
    message(paste0("Added the following profile: ", profile))
  }
  
  invisible(TRUE)
}

# Internal function to create local profile with proper structure
.profile_create_local <- function() {
  path_file <- .path_get("_projr-local.yml")
  
  # Check if file already exists
  if (file.exists(path_file)) {
    stop("_projr-local.yml already exists", call. = FALSE)
  }
  
  # Get the default yml structure and make elements NULL
  yml_default <- .yml_get_default_raw()
  yml_local <- .list_elem_as_null(yml_default)
  
  # Write the file
  yaml::write_yaml(yml_local, path_file)
  
  # Add to gitignore and rbuildignore
  .profile_create_local_ignore_git()
  .profile_create_local_ignore_rbuild()
  
  invisible(TRUE)
}

.list_elem_as_null <- function(x) {
  lapply(x, function(elem) {
    if (!is.list(elem)) {
      return(NULL)
    }
    .list_elem_as_null(elem)
  }) |>
    stats::setNames(names(x))
}

.profile_create_local_ignore <- function() {
  projr_ignore_file("_projr-local.yml")
}

.profile_create_local_ignore_git <- function() {
  gitignore <- readLines(.path_get(".gitignore"), warn = FALSE)
  if (!"_projr-local.yml" %in% gitignore) {
    projr_ignore_file_git("_projr-local.yml")
  }
}
.profile_create_local_ignore_rbuild <- function() {
  rbuildignore <- readLines(
    .path_get(".Rbuildignore"),
    warn = FALSE
  )
  if (!"^_projr-local\\.yml$" %in% rbuildignore) {
    writeLines(
      c(rbuildignore, "^_projr-local\\.yml$"),
      .path_get(".Rbuildignore")
    )
    .newline_append(.path_get(".Rbuildignore"))
  }
}

.profile_check <- function(x) {
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
  profile <- .profile_get_var()
  # Return "default" if no profile is set
  if (length(profile) == 0) {
    return("default")
  }
  profile
}

# Internal function to get the active profile name (including "default")
.profile_get <- function() {
  profile <- .profile_get_raw()
  if (!nzchar(profile)) {
    return("default")
  }
  profile
}

.profile_get_var <- function() {
  # get explicitly specified.PROFILES
  # (other than "default")
  profile <- .profile_get_var_unsplit()
  if (!nzchar(profile)) {
    return(character())
  }
  profile_vec <- strsplit(profile, ",|;")[[1]]
  vapply(profile_vec, trimws, character(1)) |>
    setdiff(c("default", "local", "")) |>
    stats::setNames(NULL)
}

.profile_get_var_unsplit <- function() {
  .profile_get_raw() |>
    setdiff(c("default", "local"))
}

.profile_get_raw <- function() {
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
  .profile_delete_check(profile)
  .profile_delete_impl(profile)
}

.profile_delete_check <- function(profile) {
  .assert_string(profile, TRUE)
  if (.is_opt(profile, "default")) {
    stop("Cannot delete profile named 'default' as it is used internally.")
  }
}

.profile_delete_impl <- function(profile) {
  path_fn <- .path_get(paste0("_projr-", profile, ".yml"))
  if (!file.exists(path_fn)) {
    message("No such profile detected: ", profile)
    return(invisible(FALSE))
  }
  invisible(file.remove(path_fn))
}

# Internal wrapper function for profile deletion
.profile_delete <- function(profile) {
  .profile_delete_impl(profile)
}

#' @title Delete local `projr` settings file.
#'
#' @description Deletes `_projr-local.yml` file.
#' @export
projr_profile_delete_local <- function() {
  .profile_delete("local")
}
