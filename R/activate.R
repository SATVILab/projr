projr_get_yml_active <- function(path_yml,
                                 silent) {
  if (nzchar(Sys.getenv("PROJR_PROFILE"))) {
    wd <- Sys.getenv("PROJR_PROFILE")
  } else {
    wd <- normalizePath(getwd(), winslash = "/")
  }
  if (!file.exists(path_yml)) {
    stop(paste0(
      "specified path to YAML file not found: ",
      path_yml
    ))
  }
  yml <- yaml::read_yaml(path_yml)
  if (paste0("directories-", wd) %in% names(yml)) {
    yml_dir <- yml[[paste0("directories-", wd)]]
    if ("directories-default" %in% names(yml)) {
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
        #  "Adding the following settings to the current wd's dirs: ",
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
  } else {
    if (!"directories-default" %in% names(yml)) {
      stop("Current working directory not in path_yml and there is no default")
    } else {
      yml_dir <- yml[["directories-default"]]
    }
  }
  yml_active <- append(
    stats::setNames(list(yml_dir), "directories"),
    yml[!grepl("^directories", names(yml))]
  )
  yml_active
}

projr_dir_create <- function(yml_active,
                             version_current) {
  dir_proj <- rprojroot::is_r_package$find_file()
  gitignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".gitignore")
  ))
  rbuildignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".Rbuildignore")
  ))
  yml_active_dir <- yml_active[["directories"]]
  for (i in seq_along(yml_active_dir)) {
    yml_curr_orig <- yml_active_dir[i]
    # create one where the output and archive
    # directories are versioned.
    # separate, original one kept for
    # git versioning
    yml_curr_versioned <- yml_curr_orig
    if (names(yml_curr_orig) %in% c("output", "archive")) {
      yml_curr_versioned[[1]][["path"]] <- file.path(
        yml_curr_versioned[[1]][["path"]], version_current
      )
    }
    if (!dir.exists(yml_curr_versioned[[1]][["path"]])) {
      dir.create(yml_curr_versioned[[1]][["path"]], recursive = TRUE)
    }

    within_wd <- fs::path_has_parent(
      yml_curr_orig[[1]][["path"]],
      dir_proj
    )
    if (!within_wd) next

    dir_path <- fs::path_rel(yml_curr_orig[[1]][["path"]], dir_proj)

    txt_gitignore <- paste0(gsub("/*$", "", dir_path), "/**/*")
    txt_rbuildignore <- paste0("^", Hmisc::escapeRegex(dir_path))

    if (!is.logical(yml_curr_orig[[1]][["ignore"]])) next
    if (yml_curr_orig[[1]][["ignore"]]) {
      if (!txt_gitignore %in% gitignore) {
        cat(
          "\n",
          txt_gitignore,
          "\n",
          file = file.path(dir_proj, ".gitignore"),
          sep = "",
          append = TRUE
        )
      }
      if (!txt_rbuildignore %in% rbuildignore) {
        cat(
          "\n",
          txt_rbuildignore,
          "\n",
          file = file.path(dir_proj, ".Rbuildignore"),
          sep = "",
          append = TRUE
        )
      }
    } else {
      if (txt_gitignore %in% gitignore) {
        gitignore <- gitignore[
          -which(gitignore == txt_gitignore),
        ]
        cat(
          gitignore,
          file = file.path(dir_proj, ".gitignore"),
          sep = "",
          append = FALSE
        )
      }
      if (txt_rbuildignore %in% rbuildignore) {
        rbuildignore <- rbuildignore[
          -which(rbuildignore == txt_rbuildignore),
        ]
        cat(
          rbuildignore,
          file = file.path(dir_proj, ".Rbuildignore"),
          sep = "",
          append = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}
