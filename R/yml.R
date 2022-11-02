#' @title Get settings for active profile profile
#'
#' @description
#' Reads in \code{_projr.yml} and returns
#' it after setting the directories to the directories
#' of the currently active profile.
#'
#' @return A named list.
projr_yml_get <- function() {
  projr_profile <- .projr_profile_get()
  yml <- .projr_yml_get()
  dir_profile <- paste0("directories-", projr_profile)
  if (projr_profile == "default") {
    if (!"directories-default" %in% names(yml)) {
      stop("Default projr profile active without default directories set.")
    }
    yml_dir <- yml[dir_profile]
    yml <- yml[!grepl("directories", names(yml))]
    return(yml_dir |> append(yml))
  }

  yml_dir <- yml[[dir_profile]]
  yml_default <- yml[["directories-default"]]
  # need to allow for multiple directories of the
  # same type
  nm_vec_default <- sapply(seq_along(yml_default), function(i) {
    paste0(names(yml_default)[i], "_", yml_default[[i]][["name"]])
  })
  nm_vec_dir <- sapply(seq_along(yml_dir), function(i) {
    paste0(names(yml_dir)[i], "_", yml_dir[[i]][["name"]])
  })
  nm_vec <- setdiff(nm_vec_default, nm_vec_dir)
  nm_vec_ind <- which(nm_vec_default %in% nm_vec)
  # add any settings that are missing entirely
  if (length(nm_vec) > 0) {
    # message(paste0(
    #  "Adding the following settings to the current projr_profile's dirs: ",
    #  paste0(nm_vec, collapse = "; ")
    # ))
    yml_dir <- append(yml_dir, yml_default[nm_vec_ind])
  }
  # replace paths with defaults if they are not present at all
  # (I think I deleted this previously)

  nm_vec_default <- sapply(seq_along(yml_default), function(i) {
    paste0(names(yml_default)[i], "_", yml_default[[i]][["name"]])
  })
  nm_vec_dir <- sapply(seq_along(yml_dir), function(i) {
    paste0(names(yml_dir)[i], "_", yml_dir[[i]][["name"]])
  })
  # use default path settings if path
  # not specified
  for (i in seq_along(nm_vec_dir)) {
    # replace it if it's either NULL
    # or it is zero characters long
    rep_val <- is.null(yml_dir[[i]][["path"]])
    if (!rep_val) {
      rep_val <- !nzchar(yml_dir[[i]][["path"]])
    } else {
      rep_val <- rep_val
    }
    if (!rep_val) next

    nm_ind <- which(nm_vec_default == nm_vec_dir[i])[[1]]
    yml_dir[[i]][["path"]] <- yml_default[[nm_ind]][["path"]]
  }
  # add ignore settings, as they are not present
  for (i in seq_along(nm_vec_dir)) {
    nm_ind <- which(nm_vec_default == nm_vec_dir[i])[[1]]
    yml_dir[[i]][["ignore"]] <- yml_default[[nm_ind]][["ignore"]]
  }
}
yml_active <- append(
  stats::setNames(list(yml_dir), "directories"),
  yml[!grepl("^directories", names(yml))]
)
yml_active


.projr_yml_get <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_yml <- file.path(dir_proj, "_projr.yml")
  if (!file.exists(path_yml)) {
    stop(paste0(
      "_projr.yml",
      path_yml
    ))
  }
  if (!"default-directories" %in% names(projr_yml)) {
    stop("default directories not set in _projr.yml.")
  }
  yaml::read_yaml(path_yaml)
}

.projr_yml_set <- function(list_save) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_yml <- file.path(dir_proj, "_projr.yml")
  yaml::write_yaml(list_save, path_yml)
  invisible(TRUE)
}

.projr_yml_bd_get <- function() {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_yml <- file.path(dir_proj, "_bookdown.yml")
  if (!file.exists(path_yml)) {
    stop(paste0(
      "_bookdown.yml",
      path_yml
    ))
  }
  yaml::read_yaml(path_yaml)
}

.projr_yml_bd_set <- function(list_save) {
  dir_proj <- rprojroot::is_r_package$find_file()
  path_yml <- file.path(dir_proj, "_bookdown.yml")
  yaml::write_yaml(list_save, path_yml)
  invisible(TRUE)
}
