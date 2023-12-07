#' @title Get active `projr` settings and checks for validity
#'
#' @description
#' Gets active `projr` settings, which merges settings and resolves conflicts
#' between local (`_projr-local.yml`), profile (`_projr-<projr>.yml` or as
#' <key>-<profile> keys in `_projr.yml`)
#' and default (`_projr.yml`) settings.
#' Where there are conflicts, local settings has highest precedence
#' (i.e. are always preferred) and default settings have lowest precedence
#' (i.e. are never preferred).
#'
#' Note that an error is thrown if the active settings
#' are invalid.
#'
#' @seealso projr_yml_get_unchecked,projr_yml_check
#'
#' @return A named list, if the settings are valid.
#'
#' @export
projr_yml_get <- function() {
  yml_projr <- projr_yml_get_unchecked()
  projr_yml_check(yml_projr)
  yml_projr
}

#' @title Get active `projr` settings and do no check
#'
#' @description A list of the active `projr` settings,
#' without doing any error checking.
#' Gets active `projr` settings, which merges settings and resolves conflicts
#' between local (`_projr-local.yml`), profile (`_projr-<projr>.yml` or as
#' <key>-<profile> keys in `_projr.yml`)
#' and default (`_projr.yml`) settings.
#' Where there are conflicts, local settings has highest precedence
#' (i.e. are always preferred) and default settings have lowest precedence
#' (i.e. are never preferred).
#'
#' @return A named list.
#'
#' @seealso projr_yml_get,projr_yml_check
#' @export
projr_yml_get_unchecked <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_profile <- .projr_yml_get_profile()
  yml_projr_local <- .projr_yml_get_local()

  .projr_yml_merge(
    yml_projr_root, yml_projr_profile, yml_projr_local
  )
}

.projr_yml_merge <- function(yml_projr_root_default,
                             yml_projr_profile,
                             yml_projr_local) {
  nm_vec <- names(yml_projr_root_default) |>
    c(names(yml_projr_profile), names(yml_projr_local)) |>
    unique()
  lapply(nm_vec, function(nm) {
    elem_default <- yml_projr_root_default[[nm]]
    elem_profile <- yml_projr_profile[[nm]]
    elem_local <- yml_projr_local[[nm]]
    # return early if highest-precedence element
    # is not a list.
    # Otherwise, make any lower-precedence elements
    # NULL.
    # if local is a list, then make others
    # empty lists if not lists
    if (is.list(elem_local)) {
      if (!is.list(elem_profile)) {
        elem_profile <- list()
      }
      if (!is.list(elem_default)) {
        elem_default <- list()
      }
    } else {
      # if elem_local is not a list and is not NULL,
      # then simply return elem_local
      if (!is.null(elem_local)) {
        return(elem_local)
      } else {
        # if elem_local is not a list and is NULL,
        # then consider what elem_profile is
        if (is.list(elem_profile)) {
          if (!is.list(elem_default)) {
            elem_default <- list()
          }
        } else {
          # if elem_profile is not a list and is not NULL,
          # then simply return elem_profile
          if (!is.null(elem_profile)) {
            return(elem_profile)
          } else {
            # if elem_profile is not a list and is NULL,
            # then it must be that elem_default is not NULL
            # (otherwise we would not have a name entry here)
            if (!is.list(elem_default)) {
              return(elem_default)
            }
          }
        }
      }
    }
    # carry on if no return has been made
    # because the highest-precedence element was not a list
    .projr_yml_merge(elem_default, elem_profile, elem_local)
  }) |>
    stats::setNames(nm_vec)
}

.projr_yml_get_root_full <- function() {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  if (!file.exists(path_yml)) {
    stop(paste0("_projr.yml not found at ", path_yml))
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_get_root_default <- function() {
  yml_projr_root_full <- .projr_yml_get_root_full()
  nm_vec <- c("directories", "build")
  if ("version-format" %in% names(yml_projr_root_full)) {
    nm_vec <- c(nm_vec, "version-format")
  }
  yml_projr_root_full[nm_vec]
}

.projr_yml_get_profile <- function() {
  profile <- projr_profile_get()
  if (profile == "default") {
    return(list())
  }
  yml_projr_root_full <- .projr_yml_get_root_full()
  key_root_dir <- paste0("directories-", profile)
  key_root_build <- paste0("build-", profile)
  root_dir_ind <- key_root_dir %in% names(yml_projr_root_full)
  root_build_ind <- key_root_build %in% names(yml_projr_root_full)
  path_yml_projr_profile <- .projr_dir_proj_get(
    paste0("_projr-", profile, ".yml")
  )
  path_projr_profile_root <- file.exists(path_yml_projr_profile)
  if ((root_build_ind || root_dir_ind) && path_projr_profile_root) {
    stop(paste0(
      "Settings for profile ", profile,
      " found in both _projr.yml and _projr-", profile,
      ".yml. Please either delete _projr-", profile,
      ".yml, or remove the profile's settings in _projr.yml"
    ))
  }
  if (root_build_ind || root_dir_ind) {
    pos_dir <- which(names(yml_projr_root_full) == key_root_dir)
    pos_build <- which(names(yml_projr_root_full) == key_root_build)
    pos_either <- c(pos_dir, pos_build)
    yml_projr_profile <- yml_projr_root_full[pos_either]
    return(yml_projr_profile)
  }
  if (!file.exists(path_yml_projr_profile)) {
    return(list())
  }
  yml_projr_init <- yaml::read_yaml(path_yml_projr_profile)
  pos_dir <- which(names(yml_projr_init) == key_root_dir)
  pos_build <- which(names(yml_projr_init) == key_root_build)
  pos_either <- c(pos_dir, pos_build)
  yml_projr_init[pos_either]
}

.projr_yml_get_local <- function() {
  path_yml <- .projr_dir_proj_get("_projr-local.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }

  yml_projr_init <- yaml::read_yaml(path_yml)
  pos_dir <- which(names(yml_projr_init) == "directories")
  pos_build <- which(names(yml_projr_init) == "build")
  pos_either <- c(pos_dir, pos_build)
  yml_projr_init[pos_either]
}

.projr_yml_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_set_root <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_bd_get <- function() {
  path_yml <- .projr_dir_proj_get("_bookdown.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_quarto_get <- function() {
  path_yml <- .projr_dir_proj_get("_quarto.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.projr_yml_bd_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_bookdown.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_quarto_set <- function(list_save) {
  path_yml <- .projr_dir_proj_get("_quarto.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_desc_get <- function() {
  path_desc <- .projr_dir_proj_get("DESCRIPTION")
  read.dcf(path_desc)
}
