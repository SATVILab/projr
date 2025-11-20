.build_script_run <- function(stage) {
  yml_script <- .yml_script_get(NULL)
  if (.is_len_0(yml_script)) {
    return(invisible(FALSE))
  }
  for (x in yml_script) {
    .build_script_run_title(x, stage = stage)
  }
}

.build_post_script_run <- function() {
  .build_script_run("post")
}

.build_pre_script_run <- function() {
  .build_script_run("pre")
}

.build_script_run_title <- function(x, stage) {
  if (!.is_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .script_run(x[["path"]][i])
  }
}

.script_run <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Script '", path, "' does not exist."))
  }
  # Run script in an isolated child environment of the global environment
  # This prevents scripts from cluttering the global environment or affecting each other
  script_env <- new.env(parent = .GlobalEnv)
  source(path, local = script_env)
  invisible(NULL)
}
