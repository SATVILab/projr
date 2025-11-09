.build_hooks_run <- function(stage) {
  # Try new hooks configuration first
  yml_hooks <- .yml_hooks_get(NULL)
  if (!.is_len_0(yml_hooks)) {
    for (x in yml_hooks) {
      .build_hooks_run_title(x, stage = stage)
    }
    return(invisible(TRUE))
  }
  
  # Fall back to old script configuration for backward compatibility
  yml_script <- .yml_script_get(NULL)
  if (!.is_len_0(yml_script)) {
    for (x in yml_script) {
      .build_hooks_run_title(x, stage = stage)
    }
  }
  invisible(TRUE)
}

.build_post_hooks_run <- function() {
  .build_hooks_run("post")
}

.build_pre_hooks_run <- function() {
  .build_hooks_run("pre")
}

.build_hooks_run_title <- function(x, stage) {
  if (!.is_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .hook_run(x[["path"]][i])
  }
}

.hook_run <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Hook '", path, "' does not exist."))
  }
  source(path)
}
