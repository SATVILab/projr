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
  yml_active <- .get_yml_active(
    wd_var = wd_var,
    path_yml = path_yml,
    silent = silent
  )

  projr_set_up_dir(
    yml_active = yml_active,
    create_var = create_var,
    env_var = env_var
  )

  .projr_ignore(yml_active = yml_active)

  renv::init()

  invisible(yml_active)
}

.get_yml_active <- function(wd_var = "LOCAL_WORKSPACE_FOLDER",
                            path_yml,
                            silent) {
  if (nzchar(Sys.getenv(wd_var))) {
    wd <- Sys.getenv(wd_var)
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
  if (wd %in% names(yml)) {
    yml_active <- yml[[wd]]
    if ("default" %in% names(yml)) {
      yml_default <- yml[["default"]]
      nm_vec <- setdiff(names(yml_default), names(yml_active))
      if (length(nm_vec) > 0) {
        if (!silent) {
          message(paste0(
            "Adding the following settings to the current wd's dirs: ",
            paste0(nm_vec, collapse = "; ")
          ))
        }
        yml_active <- append(yml_active, yml_default[nm_vec])
      }
    }
  } else {
    if (!"default" %in% names(yml)) {
      stop("Current working directory not in path_yml and there is no default")
    }
    yml_active <- yml[["default"]]
  }
  yml_active
}

projr_set_up_dir <- function(yml_active,
                             create_var,
                             env,
                             dir_proj = getwd()) {
  gitignore <- suppressWarnings(read.table(".gitignore"))
  rbuildignore <- suppressWarnings(read.table(".Rbuildignore"))
  for (i in seq_along(yml_active)) {
    yml_curr <- yml_active[[i]]
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
    if (yml_curr[["ignore"]]) {
      if (!txt_gitignore %in% gitignore[["V1"]]) {
        cat(
          txt_gitignore,
          file = ".gitignore",
          append = TRUE
        )
        cat(
          txt_rbuildignore,
          file = ".Rbuildignore",
          append = TRUE
        )
      }
    } else {
      if (txt_gitignore %in% gitignore[["V1"]]) {
        gitignore <- gitignore[
          -which(gitignore[["V1"]] == txt_gitignore),
        ]
        cat(
          gitignore,
          file = ".gitignore",
          append = FALSE
        )
      }
      if (txt_rbuildignore %in% rbuildignore[["V1"]]) {
        rbuildignore <- rbuildignore[
          -which(rbuildignore[["V1"]] == txt_rbuildignore),
        ]
        cat(
          rbuildignore,
          file = ".Rbuildignore",
          append = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}
