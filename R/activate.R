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
prj_activate <- function(wd_var = "LOCAL_WORKSPACE_FOLDER",
                         path_yml = "_projr.yml",
                         create_var = TRUE,
                         env_var = .GlobalEnv,
                         silent = FALSE) {
  yml_active <- .get_yml_active(
    wd_var = wd_var,
    path_yml = path_yml,
    silent = silent
  )

  prj_set_up_dir(
    yml_active = yml_active,
    create_var = create_var,
    env_var = env_var
  )

  .prj_ignore(yml_active = yml_active)

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

prj_set_up_dir <- function(yml_active,
                           create_var,
                           env_var) {
  for (i in seq_along(yml_active)) {
    if (!dir.exists(yml_active[[i]])) {
      dir.create(yml_active[[i]], recursive = TRUE)
    }
    if (create_var) {
      assign(
        names(yml_active)[i],
        yml_active[[i]],
        envir = env_var
      )
    }
  }
  invisible(TRUE)
}

.prj_ignore <- function(yml_active,
                        wd_var = "LOCAL_WORKSPACE_FOLDER") {
  dcf <- read.dcf("projr/settings.dcf")

  if (missing(yml_active)) {
    yml_active <- .get_yml_active(
      wd_var = wd_var,
      path_yml = "_projr.yml",
      silent = silent
    )
  }

  for (i in seq_along(dcf)) {
    ignore <- dcf[1, i][[1]]
    nm <- colnames(dcf)[i]
    dir_nm <- paste0("dir_", gsub("vcs_and_build\\.ignore\\.", "", nm))
    gitignore <- readLines(system.file(
      "_project_structure",
      ".gitignore",
      package = "projr"
    ))
    if (dir_nm %in% names(yml_active)) {
      dir_path <- yml_active[[dir_nm]]
      within_wd <- fs::path_has_parent(
        dir_path,
        here::here()
      )
      if (!within_wd) next

      dir_path <- fs::path_rel(normalizePath(dir_path), here::here())

      if (ignore) {
        usethis::use_git_ignore(paste0(dir_path, "/*"))
        usethis::use_build_ignore(
          paste0("^", Hmisc::escapeRegex(dir_path)),
          escape = FALSE
        )
      } else {
        # #102
      }
    }
  }
  invisible(TRUE)
}
