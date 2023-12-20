#' @title Activate environment variables in files
#' @export
projr_env_file_activate <- function(file = NULL, env = NULL) {
  if (!is.null(file)) {
    for (x in file) {
      .projr_build_env_file_activate_ind(file = x, env = env)
    }
    return(invisible(TRUE))
  }
  .projr_build_env_file_activate_ind(env, "_environment.local")
  quarto_profile_vec <- .projr_quarto_profile_get()
  for (i in seq_along(quarto_profile_vec)) {
    .projr_build_env_file_activate_ind(
      env, paste0("_environment-", quarto_profile_vec[[i]])
    )
  }
  .projr_build_env_file_activate_ind(env, "_environment")
}

.projr_quarto_profile_get <- function() {
  quarto_profile <- Sys.getenv("QUARTO_PROFILE")
  if (!nzchar(quarto_profile)) {
    return(character())
  }
  quarto_profile_vec <- strsplit(quarto_profile, ",")[[1]]
  vapply(quarto_profile_vec, trimws, character(1)) |> stats::setNames(NULL)
}

.projr_build_env_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  # check implied remotes
  .projr_build_check_auth_remote()
  .projr_build_env_file_required_check()
  invisible(TRUE)
}


.projr_build_env_file_activate_ind <- function(env, file) {
  path_env <- .dir_proj_get(file)
  if (!file.exists(path_env)) {
    return(invisible(FALSE))
  }
  .projr_build_env_file_activate_ind_ignore(file)
  fn_vec_local <- readLines(path_env)
  for (i in seq_along(fn_vec_local)) {
    .projr_build_env_var_set_line(fn_vec_local[[i]], env)
  }
}

.projr_build_env_file_activate_ind_ignore <- function(file) {
  if (!fs::path_has_parent(file, .dir_proj_get())) {
    return(invisible(FALSE))
  }
  if (file == "_environment.local") {
    gitignore <- .projr_ignore_git_read()
    if (!file %in% gitignore) {
      return(invisible(FALSE))
    }
    gitignore <- c(gitignore, file)
    .projr_ignore_git_write(gitignore, append = FALSE)
  }
  rbuildignore <- .projr_ignore_rbuild_read()
  rbuildignore <- c(rbuildignore, utils::glob2rx(file))
  .projr_ignore_rbuild_write(rbuildignore, append = FALSE)
}

.projr_build_env_var_set_line <- function(line, env) {
  # handle comments
  line <- sub("^#.*$", "", line)
  line <- sub("\\s+#.*$", "", line)
  env_var_nm <- .projr_env_var_nm_get(line)
  env_var_val <- .projr_env_var_val_get(line)
  specification_correct <- nzchar(env_var_nm) && nzchar(env_var_val)
  unspecified <- !nzchar(Sys.getenv(env_var_nm))
  if (!specification_correct || !unspecified) {
    return(invisible(FALSE))
  }
  .projr_env_var_set(nm = env_var_nm, val = env_var_val)
  .projr_env_var_unset_on_exit(env_var_nm, env)
  invisible(TRUE)
}

.projr_build_env_file_required_check <- function() {
  path_required <- .dir_proj_get("_environment.required")
  if (!file.exists(path_required)) {
    return(invisible(FALSE))
  }
  fn_vec_required <- readLines(path_required)
  for (i in seq_along(fn_vec_required)) {
    .projr_build_env_var_required_check(fn_vec_required[[i]])
  }
}

.projr_build_env_var_required_check <- function(line) {
  env_var_nm <- .projr_env_var_nm_get(line)
  unspecified <- !nzchar(Sys.getenv(env_var_nm))
  if (unspecified) {
    warning(paste0(
      "Environment variable `", env_var_nm,
      "` unset but in _environment.required"
    ))
  }
  invisible(TRUE)
}
