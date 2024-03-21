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
#' @param profile character.
#' \code{projr} profile to use.
#' If \code{NULL}, then the active profile is used.
#' Default is `NULL`.
#' @param check logical.
#' Whether to check the validity of the settings.
#' Default is `FALSE`.
#'
#' @seealso projr_yml_get_unchecked,projr_yml_check
#'
#' @return A named list, if the settings are valid.
#'
#' @export
projr_yml_get <- function(profile = NULL, check = FALSE) {
  .projr_yml_get(profile)
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
#' @param profile character.
#' If supplied, the specific profile file to read in.
#' "default" loads `_projr.yml`, but another value
#' loads `_projr-<profile>.yml`.
#' If NULL, then the active profile is used.
#' It not supplied, then treated as `NULL`.
#'
#' @return A named list.
#'
#' @seealso projr_yml_get,projr_yml_check
.projr_yml_get <- function(profile) {
  if (!is.null(profile)) {
    return(.projr_yml_get_profile(profile))
  }
  .projr_yml_get_null()
}

.projr_yml_get_profile <- function(profile) {
  .assert_string(profile)
  switch(profile,
    "local" = .projr_yml_get_local(),
    "default" = .projr_yml_get_root_default(),
    .projr_yml_get_profile_spec()
  )
}

.projr_yml_get_null <- function() {
  yml_projr_root <- .projr_yml_get_root_default()
  yml_projr_profile <- .projr_yml_get_profile_spec()
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
  path_yml <- .dir_proj_get("_projr.yml")
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


.projr_yml_get_profile_spec <- function(profile) {
  if (!.is_given_mid(profile)) {
    profile <- projr_profile_get()
  }
  if (profile == "default") {
    return(list())
  }
  profile_vec <- strsplit(profile, ";")[[1]]
  if (length(profile_vec) == 1L) {
    return(.projr_yml_get_profile_ind(profile_vec))
  }

  # read in with at least 3
  profile_list <- .projr_yml_get_profile_list_min_3(profile_vec)

  yml_projr_profile <- .projr_yml_merge_list(profile_list[1:3])

  if (length(profile_list) == 3L) {
    return(yml_projr_profile)
  }

  n_done <- 3
  # merge next two in at a time
  while (n_done < length(profile_list)) {
    end_iter <- min(n_done + 2, length(profile_list))
    end_iter_max <- n_done + 2
    if (end_iter < end_iter_max) {
      profile_list <- profile_list |>
        append(lapply(1, function(x) list()))
    }
    yml_projr_profile <- .projr_yml_merge_list_add(
      yml_projr_profile, profile_list[(n_done + 1):end_iter_max]
    )
    n_done <- end_iter_max
  }
  yml_projr_profile
}

.projr_yml_get_profile_ind <- function(profile) {
  path_profile <- .dir_proj_get(
    paste0("_projr-", profile, ".yml")
  )
  if (!file.exists(path_profile)) {
    return(list())
  }
  yaml::read_yaml(path_profile)
}

.projr_yml_get_profile_list_min_3 <- function(profile_vec) {
  profile_list <- lapply(profile_vec, .projr_yml_get_profile_ind)
  if (length(profile_list) == 3L) {
    return(profile_list)
  }
  # ensure we have at least three
  rem <- 3 - length(profile_list) %% 3
  profile_list |>
    append(lapply(seq_len(rem), function(x) list()))
}

.projr_yml_merge_list <- function(profile_list) {
  .projr_yml_merge(
    yml_projr_root_default = profile_list[[3]],
    yml_projr_profile = profile_list[[2]],
    yml_projr_local = profile_list[[1]]
  )
}

.projr_yml_merge_list_add <- function(yml_projr_profile, profile_list) {
  .projr_yml_merge(
    yml_projr_root_default = profile_list[[3]],
    yml_projr_profile = profile_list[[2]],
    yml_projr_local = yml_projr_profile
  )
}

.projr_yml_get_local <- function() {
  path_yml <- .dir_proj_get("_projr-local.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }

  yml_projr_init <- yaml::read_yaml(path_yml)
  pos_dir <- which(names(yml_projr_init) == "directories")
  pos_build <- which(names(yml_projr_init) == "build")
  pos_either <- c(pos_dir, pos_build)
  yml_projr_init[pos_either]
}

.projr_yml_set <- function(list_save, profile = "default") {
  path_yml <- .projr_yml_get_path(profile)
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_yml_get_path <- function(profile) {
  if (!.is_given_mid(profile) || profile == "default") {
    return(.dir_proj_get("_projr.yml"))
  }
  .dir_proj_get(paste0("_projr-", profile, ".yml"))
}

.projr_yml_set_root <- function(list_save) {
  path_yml <- .dir_proj_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .projr_newline_append(path_yml)
  invisible(TRUE)
}

.projr_desc_get <- function() {
  path_desc <- .dir_proj_get("DESCRIPTION")
  read.dcf(path_desc)
}

.projr_yml_complete <- function(yml, nm, default) {
  switch(class(default)[[1]],
    "NULL" = .projr_yml_complete_null(yml, nm),
    .projr_yml_complete_default(yml, nm, default)
  )
}

.projr_yml_complete_default <- function(yml, nm, default) {
  yml[[nm]] <- yml[[nm]] %||% default
  yml
}

.projr_yml_complete_null <- function(yml, nm) {
  if (!nm %in% names(yml)) {
    yml <- yml |> append(list(NULL) |> stats::setNames(nm))
  }
  yml
}
