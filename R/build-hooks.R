.build_hooks_run <- function(stage) {
  # Try new hooks configuration first (build.hooks.pre, build.hooks.post, build.hooks)
  hooks_list <- .yml_hooks_get_stage(stage, NULL)
  if (!is.null(hooks_list) && length(hooks_list) > 0) {
    for (hook_path in hooks_list) {
      if (is.character(hook_path)) {
        .hook_run(hook_path)
      }
    }
    return(invisible(TRUE))
  }
  
  # Fall back to old structure with stage attribute for backward compatibility
  yml_hooks <- .yml_hooks_get(NULL)
  if (!.is_len_0(yml_hooks)) {
    for (x in yml_hooks) {
      if (is.list(x) && !is.null(x[["stage"]])) {
        .build_hooks_run_title(x, stage = stage)
      }
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
