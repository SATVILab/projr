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
#' @seealso.yml_get_unchecked.yml_check
#'
#' @return A named list, if the settings are valid.
#'
#' @export
projr_yml_get <- function(profile = NULL, check = FALSE) {
  .yml_get(profile)
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
#' @seealso.yml_get.yml_check
.yml_get <- function(profile) {
  if (!is.null(profile)) {
    return(.yml_get_profile(profile))
  }
  .yml_get_null()
}

.yml_get_profile <- function(profile) {
  .assert_string(profile)
  switch(profile,
    "local" = .yml_get_local(),
    "default" = .yml_get_default(),
    .yml_get_profile_spec(profile)
  )
}

.yml_get_null <- function() {
  yml_projr_root <- .yml_get_default()
  yml_projr_profile <- .yml_get_profile_spec(profile)
  yml_projr_local <- .yml_get_local()

  .yml_merge(
    yml_projr_root, yml_projr_profile, yml_projr_local
  )
}

.yml_merge <- function(yml_projr_root_default,
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
    .yml_merge(elem_default, elem_profile, elem_local)
  }) |>
    stats::setNames(nm_vec)
}

.yml_get_default_raw <- function() {
  path_yml <- .path_get("_projr.yml")
  if (!file.exists(path_yml)) {
    return(list())
  }
  yaml::read_yaml(path_yml)
}

.yml_get_default <- function() {
  .yml_get_profile_ind("default") |>
    .yml_get_filter_top_level()
}


.yml_get_profile_spec <- function(profile) {
  if (!.is_given_mid(profile)) {
    profile_vec <- .profile_get_var()
  } else {
    .assert_string(profile)
    profile_vec <- vapply(profile, trimws, character(1)) |>
      setdiff(c("default", "local", "")) |>
      stats::setNames(NULL)
  }
  if (!all(nzchar(profile_vec))) {
    return(list())
  }
  if (length(profile_vec) == 1L) {
    return(.yml_get_profile_ind(profile_vec))
  }

  # read in with at least 3
  profile_list <- .yml_get_profile_list_min_3(profile_vec)

  yml_projr_profile <- .yml_merge_list(profile_list[1:3])

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
    yml_projr_profile <- .yml_merge_list_add(
      yml_projr_profile, profile_list[(n_done + 1):end_iter_max]
    )
    n_done <- end_iter_max
  }
  yml_projr_profile
}

.yml_get_profile_ind <- function(profile) {
  .yml_get_profile_ind_raw(profile) |>
    .yml_get_filter_top_level()
}

.yml_get_profile_ind_raw <- function(profile) {
  .assert_string(profile)
  if (missing(profile) || profile == "default") {
    profile_spec <- ""
  } else {
    profile_spec <- paste0("-", profile)
  }
  path_profile <- .path_get(
    paste0("_projr", profile_spec, ".yml")
  )
  if (!file.exists(path_profile)) {
    return(list())
  }
  yaml::read_yaml(path_profile)
}

.yml_get_profile_list_min_3 <- function(profile_vec) {
  profile_list <- lapply(profile_vec, .yml_get_profile_ind)
  if (length(profile_list) == 3L) {
    return(profile_list)
  }
  # ensure we have at least three
  rem <- 3 - length(profile_list) %% 3
  profile_list |>
    append(lapply(seq_len(rem), function(x) list()))
}

.yml_merge_list <- function(profile_list) {
  .yml_merge(
    yml_projr_root_default = profile_list[[3]],
    yml_projr_profile = profile_list[[2]],
    yml_projr_local = profile_list[[1]]
  )
}

.yml_merge_list_add <- function(yml_projr_profile, profile_list) {
  .yml_merge(
    yml_projr_root_default = profile_list[[3]],
    yml_projr_profile = profile_list[[2]],
    yml_projr_local = yml_projr_profile
  )
}

.yml_get_local <- function() {
  .yml_get_profile_ind("local") |>
    .yml_get_filter_top_level()
}

.yml_get_filter_top_level <- function(yml) {
  nm_list <- list(
    "directories",
    "build",
    par_nm_vec,
    "metadata"
  )

  pos_list <- lapply(nm_list, .yml_get_filter_top_level_ind, yml = yml)
  pos_vec <- unlist(Filter(Negate(is.null), pos_list), use.names = FALSE)

  yml[pos_vec]
}


.yml_get_filter_top_level_ind <- function(yml, nm) {
  idx <- match(nm, names(yml), nomatch = 0)
  idx <- idx[idx > 0]
  if (length(idx) > 0) idx[1] else NULL
}

.yml_set <- function(list_save, profile = "default") {
  path_yml <- .yml_get_path(profile)
  yaml::write_yaml(list_save, path_yml)
  .newline_append(path_yml)
  invisible(TRUE)
}

.yml_get_path <- function(profile) {
  if (!.is_given_mid(profile) || profile == "default") {
    return(.path_get("_projr.yml"))
  }
  .path_get(paste0("_projr-", profile, ".yml"))
}

.yml_set_root <- function(list_save) {
  path_yml <- .path_get("_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  .newline_append(path_yml)
  invisible(TRUE)
}

.desc_get <- function() {
  if (!file.exists(.path_get("DESCRIPTION"))) {
    stop("DESCRIPTION file not found")
  }
  path_desc <- .path_get("DESCRIPTION")
  read.dcf(path_desc)
}

.yml_complete <- function(yml, nm, default) {
  switch(class(default)[[1]],
    "NULL" = .yml_complete_null(yml, nm),
    .yml_complete_default(yml, nm, default)
  )
}

.yml_complete_default <- function(yml, nm, default) {
  yml[[nm]] <- yml[[nm]] %||% default
  yml
}

.yml_complete_null <- function(yml, nm) {
  if (!nm %in% names(yml)) {
    yml <- yml |> append(list(NULL) |> stats::setNames(nm))
  }
  yml
}

.yml_order <- function(yml) {
  nm_vec <- character(0L)
  order_vec <- c(
    "directories", par_nm_vec, "build"
  )
  for (x in order_vec) {
    if (x %in% names(yml)) {
      nm_vec <- c(nm_vec, x)
    }
  }
  nm_vec <- c(nm_vec, setdiff(names(yml), nm_vec))
  yml[nm_vec]
}
