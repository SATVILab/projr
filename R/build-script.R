.build_script_run <- function(stage) {
  yml_script <- .yml_script_get(NULL)
  if (.is_len_0(yml_script)) {
    return(invisible(FALSE))
  }
  # Validate all script paths exist before execution
  .build_script_validate_paths(yml_script, stage)
  
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

.build_script_validate_paths <- function(yml_script, stage) {
  missing_scripts <- character(0)
  
  for (x in yml_script) {
    # Only check scripts for the current stage
    if (.is_opt(x[["stage"]], stage)) {
      paths <- x[["path"]]
      for (path in paths) {
        if (!file.exists(path)) {
          missing_scripts <- c(missing_scripts, path)
        }
      }
    }
  }
  
  if (length(missing_scripts) > 0) {
    stop(
      "The following script(s) specified in _projr.yml do not exist:\n",
      paste0("  - ", missing_scripts, collapse = "\n"),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
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
  source(path)
}
