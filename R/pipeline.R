# Named pipelines: ordered scripts and hooks under top-level `pipelines`,
# selected by `build.pipeline`, `dev.pipeline` or the `pipeline` argument of
# the build functions (passed to the run as PROJR_PIPELINE).
#
# Inline `scripts` and `hooks` under `build`/`dev` combine with the pipeline.
# Whether they run before or after its entries follows their key position
# relative to `pipeline` in the highest-precedence file that sets `pipeline`.
# A pipeline selected only by argument runs after the inline entries.
# `pipeline: false` or `[]` (or `pipeline = FALSE`) selects no pipeline.

.pipeline_mode <- function(bump_component) {
  if (.is_dev_build(bump_component)) "dev" else "build"
}

# PROJR_PIPELINE value for `pipeline = FALSE`. The argument passes several
# entries comma-separated, so pipeline names cannot hold ":" or ",".
.pipeline_none <- ":none"

# The selected pipeline entries (names or globs), or NULL when none.
.pipeline_selection <- function(mode, yml = .yml_get(NULL)) {
  argument <- Sys.getenv("PROJR_PIPELINE")
  if (identical(argument, .pipeline_none)) {
    return(NULL)
  }
  if (nzchar(argument)) {
    return(strsplit(argument, ",", fixed = TRUE)[[1L]])
  }
  selection <- yml[[mode]][["pipeline"]]
  if (is.null(selection) && mode == "dev") {
    selection <- yml[["build"]][["pipeline"]]
  }
  if (isFALSE(selection) || !length(selection)) NULL else unlist(selection)
}

# Pipeline names for the selected entries: entries in the order given, a glob's
# matches in their order under `pipelines`, each pipeline once.
.pipeline_resolve <- function(
  selection,
  yml = .yml_get(NULL),
  key = "pipeline"
) {
  defined <- names(yml[["pipelines"]])
  names <- unlist(lapply(selection, function(entry) {
    matched <- defined[grepl(utils::glob2rx(entry), defined)]
    if (!length(matched)) {
      stop(
        key,
        ": ",
        entry,
        " matches no pipeline defined under pipelines.",
        call. = FALSE
      )
    }
    matched
  }))
  unique(names)
}

# The resolved scripts and hooks for a build mode, or NULL without a pipeline.
# `scripts`, the build functions' argument, takes the place of the inline
# scripts and combines with the pipelines in their position.
.pipeline_plan <- function(mode, scripts = NULL) {
  yml <- .yml_get(NULL)
  selection <- .pipeline_selection(mode, yml)
  if (is.null(selection)) {
    return(NULL)
  }
  names <- .pipeline_resolve(selection, yml)
  pipelines <- yml[["pipelines"]][names]
  inline_scripts <- scripts %||%
    if (mode == "dev") {
      .yml_dev_get_scripts(NULL) %||% .yml_scripts_get(NULL)
    } else {
      .yml_scripts_get(NULL)
    }
  inline_hooks <- if (mode == "dev") {
    .yml_dev_get_hooks(NULL)
  } else {
    .yml_build_get_hooks(NULL)
  }
  own_scripts <- unlist(lapply(names, function(name) {
    .yml_scripts_normalize(
      pipelines[[name]][["scripts"]],
      paste0("pipelines.", name, ".scripts")
    )
  }))
  scripts <- if (.pipeline_inline_first("scripts", mode)) {
    c(inline_scripts, own_scripts)
  } else {
    c(own_scripts, inline_scripts)
  }
  hooks_first <- .pipeline_inline_first("hooks", mode)
  hooks <- lapply(c(pre = "pre", post = "post"), function(stage) {
    own <- unlist(lapply(pipelines, function(pipeline) {
      .pipeline_hooks_stage(pipeline[["hooks"]], stage)
    }))
    inline <- .pipeline_hooks_stage(inline_hooks, stage)
    # A hook named more than once runs once, at its first position.
    unique(as.character(if (hooks_first) c(inline, own) else c(own, inline)))
  })
  list(
    name = names,
    scripts = as.character(scripts),
    pre = hooks[["pre"]],
    post = hooks[["post"]]
  )
}

.pipeline_hooks_stage <- function(hooks, stage) {
  c(as.character(unlist(hooks[[stage]])), as.character(unlist(hooks[["both"]])))
}

# Inline entries run first unless the highest-precedence file that sets
# `pipeline` places `key` after it.
.pipeline_inline_first <- function(key, mode) {
  layers <- c(
    list(.yml_get_local()),
    lapply(.profile_get_var(), .yml_get_profile_ind),
    list(.yml_get_default())
  )
  for (block in unique(c(mode, "build"))) {
    for (layer in layers) {
      keys <- names(layer[[block]])
      if ("pipeline" %in% keys) {
        return(!key %in% keys || match(key, keys) < match("pipeline", keys))
      }
    }
  }
  TRUE
}

.pipeline_check_name <- function(selection, yml = .yml_get(NULL)) {
  .assert_chr(selection, TRUE, nm = "pipeline")
  .pipeline_resolve(selection, yml)
  invisible(TRUE)
}

# Validate `pipelines` and the `build.pipeline`/`dev.pipeline` selections.
.yml_pipelines_check <- function(profile) {
  yml <- .yml_get(profile)
  pipelines <- yml[["pipelines"]]
  .yml_check_remote_mapping(pipelines, "pipelines")
  for (name in names(pipelines)) {
    key <- paste0("pipelines.", name)
    if (grepl("[:,]", name)) {
      stop(key, ": pipeline names cannot contain ':' or ','.", call. = FALSE)
    }
    pipeline <- pipelines[[name]]
    .yml_check_remote_mapping(pipeline, key, c("scripts", "hooks"))
    .yml_scripts_normalize(pipeline[["scripts"]], paste0(key, ".scripts"))
    hooks <- pipeline[["hooks"]]
    .yml_check_remote_mapping(
      hooks,
      paste0(key, ".hooks"),
      c("pre", "post", "both")
    )
    for (stage in names(hooks)) {
      .assert_chr(
        unlist(hooks[[stage]]),
        nm = paste0(key, ".hooks.", stage)
      )
    }
  }
  for (mode in c("build", "dev")) {
    selection <- yml[[mode]][["pipeline"]]
    if (isFALSE(selection) || !length(selection)) {
      next
    }
    key <- paste0(mode, ".pipeline")
    selection <- unlist(selection)
    .assert_chr(selection, TRUE, nm = key)
    .pipeline_resolve(selection, yml, key)
  }
  invisible(TRUE)
}

# Report the resolved plan in the console and detailed log.
.pipeline_report <- function(plan) {
  if (is.null(plan)) {
    return(invisible(FALSE))
  }
  scripts <- paste(plan[["scripts"]], collapse = ", ")
  names <- paste(plan[["name"]], collapse = ", ")
  .cli_info(
    "Pipeline {names}: {if (nzchar(scripts)) scripts else 'no scripts'}"
  )
  detail <- paste0(
    "Pipeline hooks: pre [",
    paste(plan[["pre"]], collapse = ", "),
    "], post [",
    paste(plan[["post"]], collapse = ", "),
    "]"
  )
  .cli_debug("{detail}")
  invisible(TRUE)
}

# The resolved plan for production build history.
.buildlog_get_pipeline <- function() {
  plan <- tryCatch(.pipeline_plan("build"), error = function(e) NULL)
  if (is.null(plan)) {
    return(character())
  }
  entry <- list(
    name = plan[["name"]],
    scripts = plan[["scripts"]],
    hooks = list(pre = plan[["pre"]], post = plan[["post"]])
  )
  c("**Pipeline**", "", "```yaml", yaml::as.yaml(entry), "```", "")
}
