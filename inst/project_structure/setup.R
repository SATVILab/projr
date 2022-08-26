
#' @param path_yml character.
#' Path to YAML file.
#' Default is "_projr.yml".
#' @param env_var character.
#' Name of environment variable to
#' use as the selecting variable for which
#' configuration to use.
#' If the environnment variable is empty,
#' then the working directory is used.
#' Default is "LOCAL_WORKSPACE_FOLDER".
.setup_dir <- function(env_var = "LOCAL_WORKSPACE_FOLDER",
                       path_yml = "_projr.yml",
                       create_dir_var = TRUE,
                       env_dir_var = .GlobalEnv) {
  if (nzchar(Sys.getenv(env_var))) {
    wd <- Sys.getenv(env_var)
  } else {
    wd <- normalizePath(getwd(), winslash = "/")
  }

  if (!file.exists(path_yml)) {
    stop(
      paste0("specified path to YAML file not found: ", path_yml)
    )
  }

  yml <- yaml::read_yaml(path_yml)

  if (wd %in% names(yml)) {
    yml_curr <- yml[[wd]]
    if ("default" %in% names(yml)) {
      yml_default <- yml[["default"]]
      nm_vec <- setdiff(names(yml_default), yml_curr)
      if (nzchar(nm_vec)) {
        message(paste0(
          "Adding the following settings to the current wd's dirs: ",
          paste0(nm_vec, collapse = "; ")
        ))
        yml_curr <- append(yml_curr, yml_default[nm_vec])
      }
    }
  } else {
    if (!"default" %in% names(yml)) {
      stop("Current working directory not in path_yml and there is no default")
    }
    yml_curr <- yml[["default"]]
  }

  for (i in seq_along(yml_curr)) {
    if (!dir.exists(yml_curr[[i]])) {
      dir.create(yml_curr, recursive = TRUE)
    }
    if (create_dir_var) {
      assign(
        names(yml_curr)[i],
        yml_curr[[i]],
        envir = env_dir_var
      )
    }
  }

  invisible(yml_curr)
}
