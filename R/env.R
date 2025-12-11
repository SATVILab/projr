#' @title Set environment variables from files
#'
#' @description
#' Activate environment variables by reading default values from a set of files.
#' If `file` is `NULL`,
#' all existing files are used in order of decreasing priority:
#' first `_environment.local` (machine-specific overrides),
#' then any `_environment-<profile>` files (profile-specific),
#' and finally `_environment` (global defaults).
#'
#' The profiles activated are those set in the `QUARTO_PROFILE`
#' and the `PROJR_PROFILE` environment variables, with
#' `QUARTO_PROFILE` priorities.
#' The `QUARTO_PROFILE` variable can specify multiple profiles,
#' separated by commas.
#' The `PROJR_PROFILE` variable can also specify multiple profiles,
#' separated by either commas or semi-colons.
#' In both cases, the earlier profile takes precedence, e.g.
#' `QUARTO_PROFILE=test,basic` will activate the `test` profile first.
#'
#' @param file character vector.
#'   Paths to files with environment variables to activate.
#'   If provided, only these files will be read; otherwise the default set
#'   (`_environment.local`, `_environment-<profile>`, `_environment`) is used.
#'
#' @return Invisibly returns `TRUE` if any files were successfully activated,
#'   or `FALSE` if none existed.
#'
#' @examples
#' # Activate only the local overrides
#' \dontrun{
#' projr_env_set("_environment.local")
#' }
#' # Activate all available defaults in the standard order
#' \dontrun{
#' projr_env_set()
#' }
#'
#' @export
projr_env_set <- function(file = NULL) {
  .env_set(file, FALSE)
}

.env_set <- function(file = NULL, unset = FALSE) {
  .file_rm(.env_file_get_path_list())
  if (!is.null(file)) {
    for (x in file) {
      .env_file_activate_ind(file = x, unset)
    }
    return(invisible(TRUE))
  }
  .env_file_activate_ind("_environment.local", unset)
  env_profile_vec <- .env_profile_get()
  for (i in seq_along(env_profile_vec)) {
    .env_file_activate_ind(
      paste0("_environment-", env_profile_vec[[i]]), unset
    )
  }
  .env_file_activate_ind("_environment", unset)
  invisible(TRUE)
}

.env_profile_get <- function() {
  quarto_profile_vec <- .env_profile_get_quarto()
  profile_vec <- .env_profile_get_projr()
  c(quarto_profile_vec, profile_vec) |>
    setdiff("required") |>
    unique()
}

.env_profile_get_quarto <- function() {
  quarto_profile <- Sys.getenv("QUARTO_PROFILE")
  if (!nzchar(quarto_profile)) {
    return(character())
  }
  quarto_profile_vec <- strsplit(quarto_profile, ",")[[1]]
  vapply(quarto_profile_vec, trimws, character(1)) |> stats::setNames(NULL)
}

.env_profile_get_projr <- function() {
  .profile_get_var()
}

.build_env_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  # check implied remotes
  .build_check_auth_remote()
  .build_env_file_required_check()
  invisible(TRUE)
}

.env_file_activate_ind <- function(file, unset) {
  path_env <- .path_get(file)
  if (!file.exists(path_env)) {
    return(invisible(FALSE))
  }
  .env_file_activate_ind_ignore(file)
  fn_vec_local <- readLines(path_env, warn = FALSE)
  for (i in seq_along(fn_vec_local)) {
    .env_var_set_line(fn_vec_local[[i]], unset)
  }
}

.env_file_activate_ind_ignore <- function(file) {
  if (!fs::path_has_parent(file, .path_get())) {
    return(invisible(FALSE))
  }
  if (file == "_environment.local") {
    projr_ignore_file(file)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

.env_var_set_line <- function(line, unset) {
  # handle comments
  line <- sub("^#.*$", "", line)
  line <- sub("\\s+#.*$", "", line)
  env_var_nm <- .env_var_nm_get(line)
  env_var_val <- .env_var_val_get(line)
  specification_correct <- nzchar(env_var_nm) && nzchar(env_var_val)
  unspecified <- !nzchar(Sys.getenv(env_var_nm))
  if (!specification_correct || !unspecified) {
    return(invisible(FALSE))
  }
  .env_var_set(nm = env_var_nm, val = env_var_val)
  .env_var_add_to_unset(env_var_nm, unset)
  invisible(TRUE)
}

.build_env_file_required_check <- function() {
  path_required <- .path_get("_environment.required")
  if (!file.exists(path_required)) {
    return(invisible(FALSE))
  }
  fn_vec_required <- readLines(path_required, warn = FALSE)
  for (i in seq_along(fn_vec_required)) {
    .build_env_var_required_check(fn_vec_required[[i]])
  }
}

.build_env_var_required_check <- function(line) {
  env_var_nm <- .env_var_nm_get(line)
  unspecified <- !nzchar(Sys.getenv(env_var_nm))
  if (unspecified) {
    warning(paste0(
      "Environment variable `", env_var_nm,
      "` unset but in _environment.required"
    ))
  }
  invisible(TRUE)
}

.env_var_add_to_unset <- function(env_var_nm, unset) {
  if (!unset) {
    return(invisible(FALSE))
  }
  path_file <- .env_file_get_path_list()
  if (!file.exists(path_file)) {
    .dir_create(dirname(path_file))
    invisible(file.create(path_file))
  }
  fn_vec <- readLines(path_file, warn = FALSE)
  if (env_var_nm %in% fn_vec) {
    return(invisible(FALSE))
  }
  fn_vec <- c(fn_vec, env_var_nm)
  writeLines(fn_vec, path_file)
  .newline_append(path_file)
  invisible(TRUE)
}

.env_file_get_path_list <- function() {
  file.path(tempdir(), "projr-env_file", "env.list")
}

.env_unset <- function() {
  if (!file.exists(.env_file_get_path_list())) {
    return(invisible(FALSE))
  }
  fn_vec <- readLines(.env_file_get_path_list(), warn = FALSE)
  for (i in seq_along(fn_vec)) {
    if (.is_string(fn_vec[[i]])) {
      eval(parse(text = paste0("Sys.unsetenv('", fn_vec[[i]], "')")))
    }
  }
  .dir_rm(dirname(.env_file_get_path_list()))
}
