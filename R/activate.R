#' @title Activate project
#'
#' @description Activate project
#'
#' @param silent. Logical.
#' If \code{TRUE}, then messages are suppressed
#' (but not warnings or errors).
#' Default is \code{FALSE}.
#'
#' @export
projr_activate <- function(wd_var = "PROJR_WORKING_DIRECTORY",
                           path_yml = "_projr.yml",
                           create_var = TRUE,
                           env_var = .GlobalEnv,
                           silent = FALSE) {
  dir_proj <- rprojroot::is_r_package$find_file()

  # get active directories
  yml_active <- projr_get_yml_active(
    wd_var = wd_var,
    path_yml = file.path(dir_proj, "_projr.yml"),
    silent = silent
  )

  # get current version
  version_format_list <- .get_version_format_list(
    version_format = yml_active[["version"]]
  )
  yml_bd <- yaml::read_yaml(
    file.path(dir_proj, "_bookdown.yml")
  )
  proj_nm <- .get_proj_nm(
    fn = yml_bd$book_filename,
    version_format = yml_active[["version"]]
  )
  version_current <- gsub(
    paste0("^", proj_nm), "", yml_bd$book_filename
  )

  # create directories and add variables
  # to global directory
  projr_set_up_dir(
    yml_active = yml_active,
    version_current = version_current,
    create_var = create_var,
    env = env_var
  )

  invisible(yml_active)
}


projr_get_yml_active <- function(wd_var = "PROJR_WORKING_DIRECTORY",
                                 path_yml,
                                 silent) {
  if (nzchar(Sys.getenv(wd_var))) {
    wd <- Sys.getenv(wd_var)
  } else {
    wd <- normalizePath(getwd(), winslash = "/")
  }

  if (!file.exists(path_yml)) {
    stop(
      paste0("specified path to YAML file not found: ", path_yml)
    )
  }

  yml <- yaml::read_yaml(path_yml)

  # get directories done correctly
  if (paste0("directories-", wd) %in% names(yml)) {
    yml_dir <- yml[[paste0("directories-", wd)]]
    if ("directories-default" %in% names(yml)) {
      yml_default <- yml[["directories-default"]]
      nm_vec <- setdiff(names(yml_default), yml_dir)
      if (length(nm_vec) > 0) {
        message(paste0(
          "Adding the following settings to the current wd's dirs: ",
          paste0(nm_vec, collapse = "; ")
        ))
        yml_dir <- append(yml_dir, yml_default[nm_vec])
      }
    }
  } else {
    if (!"directories-default" %in% names(yml)) {
      stop("Current working directory not in path_yml and there is no default")
    } else {
      yml_dir <- yml["directories-default"]
    }
  }
  yml_active <- stats::setNames(yml_dir, "directories") |>
    append(
      yml[!grepl("^directories", names(yml))]
    )

  yml_active
}


projr_set_up_dir <- function(yml_active,
                             create_var,
                             version_current,
                             env) {
  dir_proj <- rprojroot::is_r_package$find_file()
  gitignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".gitignore")
  ))
  rbuildignore <- suppressWarnings(readLines(
    file.path(dir_proj, ".Rbuildignore")
  ))
  yml_active_dir <- yml_active[["directories"]]
  for (i in seq_along(yml_active_dir)) {
    yml_curr_orig <- yml_active_dir[[i]]
    # create one where the output and archive
    # directories are versioned.
    # separate, original one kept for
    # git versioning
    yml_curr_versioned <- yml_curr_orig
    if (names(yml_active_dir)[i] %in% c("output", "archive")) {
      yml_curr_versioned[["path"]] <- file.path(
        yml_curr_versioned[["path"]], version_current
      )
    }
    if (!dir.exists(yml_curr_versioned[["path"]])) {
      dir.create(yml_curr_versioned[["path"]], recursive = TRUE)
    }
    if (create_var) {
      assign(
        yml_curr_versioned[["name"]],
        yml_curr_versioned[["path"]],
        envir = env
      )
    }

    within_wd <- fs::path_has_parent(
      yml_curr_orig[["path"]],
      dir_proj
    )
    if (!within_wd) next

    dir_path <- fs::path_rel(yml_curr_orig[["path"]], dir_proj)

    txt_gitignore <- paste0("\n", gsub("/*$", "", dir_path), "/**/*")
    txt_rbuildignore <- paste0("\n^", Hmisc::escapeRegex(dir_path))
    if (!is.logical(yml_curr_orig[["ignore"]])) next
    if (yml_curr_orig[["ignore"]]) {
      if (!txt_gitignore %in% gitignore) {
        cat(
          txt_gitignore,
          "\n",
          file = file.path(dir_proj, ".gitignore"),
          sep = "",
          append = TRUE
        )
      }
      if (!txt_rbuildignore %in% rbuildignore) {
        cat(
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
