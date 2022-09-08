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
projr_activate <- function(wd_var = "LOCAL_WORKSPACE_FOLDER",
                           path_yml = "_projr.yml",
                           create_var = TRUE,
                           env_var = .GlobalEnv,
                           silent = FALSE) {
  yml_active <- projr_get_yml_active(
    wd_var = wd_var,
    path_yml = path_yml,
    silent = silent
  )

  projr_set_up_dir(
    yml_active = yml_active,
    create_var = create_var,
    env = env_var
  )

  invisible(yml_active)
}


projr_get_yml_active <- function(wd_var = "LOCAL_WORKSPACE_FOLDER",
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
  yml_active <- setNames(yml_dir, "directories") |>
    append(
      yml[!grepl("^directories", names(yml))]
    )

  yml_active
}


projr_set_up_dir <- function(yml_active,
                             create_var,
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
    yml_curr <- yml_active_dir[[i]]
    if (!dir.exists(yml_curr[["path"]])) {
      dir.create(yml_curr$path, recursive = TRUE)
    }
    if (create_var) {
      assign(
        yml_curr[["name"]],
        yml_curr[["path"]],
        envir = env
      )
    }

    within_wd <- fs::path_has_parent(
      yml_curr[["path"]],
      dir_proj
    )
    if (!within_wd) next

    dir_path <- fs::path_rel(yml_curr[["path"]], dir_proj)

    txt_gitignore <- paste0("\n", gsub("/*$", "", dir_path), "/**/*")
    txt_rbuildignore <- paste0("\n^", Hmisc::escapeRegex(dir_path))
    if (!is.logical(yml_curr[["ignore"]])) next
    if (yml_curr[["ignore"]]) {
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
