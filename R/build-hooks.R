.build_hooks_run <- function(stage, is_dev_build = FALSE) {
  # For dev builds, use dev.hooks and ignore build.hooks
  # For production builds, use build.hooks and ignore dev.hooks
  if (is_dev_build) {
    # Use dev.hooks
    dev_hooks_yml <- .yml_dev_get_hooks(NULL)
    if (!is.null(dev_hooks_yml)) {
      # Get hooks for this stage from dev.hooks
      hooks_list <- c()

      # Stage-specific hooks
      if (stage %in% names(dev_hooks_yml)) {
        stage_hooks <- dev_hooks_yml[[stage]]
        if (!is.null(stage_hooks)) {
          hooks_list <- c(hooks_list, as.character(stage_hooks))
        }
      }

      # Hooks that run in both stages
      if ("both" %in% names(dev_hooks_yml)) {
        both_hooks <- dev_hooks_yml[["both"]]
        if (!is.null(both_hooks)) {
          hooks_list <- c(hooks_list, as.character(both_hooks))
        }
      }

      # Run the hooks
      if (length(hooks_list) > 0) {
        for (hook_path in hooks_list) {
          if (is.character(hook_path)) {
            .cli_debug("Running dev hook: {hook_path}")
            .hook_run(hook_path)
          }
        }
      } else {
        .cli_debug("No dev hooks to run for stage: {stage}")
      }
    } else {
      .cli_debug("No dev.hooks configuration found")
    }
    return(invisible(TRUE))
  }

  # Production builds: use build.hooks
  # Try new hooks configuration first (build.hooks.pre, build.hooks.post, build.hooks.both)
  hooks_list <- .yml_hooks_get_stage(stage, NULL)
  if (!is.null(hooks_list) && length(hooks_list) > 0) {
    for (hook_path in hooks_list) {
      if (is.character(hook_path)) {
        .cli_debug("Running build hook: {hook_path}")
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
  } else {
    .cli_debug("No hooks to run for stage: {stage}")
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
    .cli_debug("Running hook (legacy format): {x[['path']][i]}")
    .hook_run(x[["path"]][i])
  }
}

.hook_run <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Hook '", path, "' does not exist."))
  }
  # Run hook in an isolated child environment of the global environment
  # This prevents hooks from cluttering the global environment or affecting each other
  hook_env <- new.env(parent = .GlobalEnv)
  source(path, local = hook_env)
  invisible(NULL)
}
