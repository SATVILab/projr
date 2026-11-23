.build_hooks_run <- function(stage, is_dev_build = FALSE) {
  # For dev builds, use dev.hooks and ignore build.hooks
  # For production builds, use build.hooks and ignore dev.hooks
  if (is_dev_build) {
    hooks <- .yml_dev_get_hooks(NULL)
    paths <- c(as.character(hooks[[stage]]), as.character(hooks[["both"]]))
    for (path in paths) .hook_run(path, stage)
    return(invisible(TRUE))
  }

  # Production builds: use build.hooks
  # Try new hooks configuration first (build.hooks.pre, build.hooks.post, build.hooks.both)
  hooks_list <- .yml_hooks_get_stage(stage, NULL)
  if (!is.null(hooks_list) && length(hooks_list) > 0) {
    for (hook_path in hooks_list) {
      if (is.character(hook_path)) {
        .hook_run(hook_path, stage)
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

.build_post_hooks_run <- function(is_dev_build = FALSE) {
  .build_hooks_run("post", is_dev_build)
}

.build_pre_hooks_run <- function(is_dev_build = FALSE) {
  .build_hooks_run("pre", is_dev_build)
}

.build_hooks_run_title <- function(x, stage) {
  if (!.is_opt(x[["stage"]], stage)) {
    return(invisible(FALSE))
  }
  for (i in seq_along(x[["path"]])) {
    .hook_run(x[["path"]][i], stage)
  }
}

.hook_run <- function(path, stage = NULL) {
  if (!file.exists(path)) {
    stop(paste0("Hook '", path, "' does not exist."))
  }
  # Run hook in an isolated child environment of the global environment
  # This prevents hooks from cluttering the global environment or affecting each other
  label <- if (is.null(stage)) "hook" else paste0(stage, "-build hook")
  .cli_step("Running {label} {path}")
  hook_env <- new.env(parent = .GlobalEnv)
  source(path, local = hook_env)
  invisible(NULL)
}
