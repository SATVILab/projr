# Return the concrete directories the replay can read or clear inside its clone.
.reproduce_check_dirs <- function(
  sources = .reproduce_sources(),
  native_cache = character()
) {
  root <- as.character(fs::path_real(.path_get()))
  if (root != fs::path_real(".")) {
    stop("Run the replay from its checkout root.", call. = FALSE)
  }
  labels <- c(
    .yml_dir_get_label_in(NULL),
    .yml_dir_get_label_output(NULL),
    .yml_dir_get_label_docs(NULL)
  )
  .assert_directory_labels(labels)
  dirs <- vapply(
    labels,
    projr_path_get_dir,
    character(1),
    safe = FALSE,
    create = FALSE,
    format = "absolute"
  )
  outputs <- setdiff(labels, .yml_dir_get_label_raw(NULL))
  paths <- c(dirs, setdiff(native_cache, dirs[outputs]))
  .restore_build_check_paths(paths)
  .reproduce_check_paths(paths, c(.path_get(c("R", "man", "tests")), sources))
  dirs
}

# Check real paths, including directory links above an as-yet missing file.
.reproduce_check_paths <- function(paths, protected = character()) {
  root <- fs::path_real(.path_get())
  protected <- c(
    .path_get(c(".git", ".projr", "_projr", "renv", "DESCRIPTION", "VERSION")),
    protected
  )
  protected <- vapply(protected, .path_resolve_links, character(1))
  for (path in paths) {
    path <- .path_resolve_links(path)
    if (
      identical(as.character(root), path) ||
        !fs::path_has_parent(path, root) ||
        any(
          fs::path_has_parent(protected, path) |
            fs::path_has_parent(path, protected)
        )
    ) {
      stop(
        "Replay path outside the checkout or overlapping protected files: ",
        path,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.reproduce_sources <- function(
  engines = .engines_get(.scripts_get(NULL, "patch", NULL))
) {
  plan <- .pipeline_plan("build")
  hooks <- if (is.null(plan)) {
    c(.yml_hooks_get_stage("pre", NULL), .yml_hooks_get_stage("post", NULL))
  } else {
    c(plan[["pre"]], plan[["post"]])
  }
  sources <- unique(c(
    unlist(engines, use.names = FALSE),
    .build_document_sources(engines),
    unlist(hooks, use.names = FALSE)
  ))
  sources <- as.character(fs::path_abs(
    as.character(sources),
    start = .path_get()
  ))
  .reproduce_check_paths(sources)
  sources
}

# Keep the checkout's byte-level formatting when temporarily changing its version.
.reproduce_at_version <- function(version, code) {
  read_text <- function(path) rawToChar(readBin(path, "raw", file.size(path)))
  version <- .version_v_add(version)
  path <- .path_get("VERSION")
  desc <- .path_get("DESCRIPTION")
  if (any(fs::link_exists(c(path, desc)))) {
    stop("Cannot update a linked version file during replay.", call. = FALSE)
  }
  original <- if (file.exists(path)) read_text(path)
  metadata <- if (file.exists(desc)) read_text(desc) else ""
  version_line <- "(?m)^Version:[^\r\n]*"
  previous <- regmatches(metadata, regexpr(version_line, metadata, perl = TRUE))
  if (
    length(previous) &&
      trimws(sub("^Version:", "", previous)) != .version_v_rm(version)
  ) {
    on.exit(
      {
        current <- read_text(desc)
        writeBin(
          charToRaw(sub(version_line, previous, current, perl = TRUE)),
          desc
        )
      },
      add = TRUE
    )
    writeBin(
      charToRaw(sub(
        version_line,
        paste("Version:", .version_v_rm(version)),
        metadata,
        perl = TRUE
      )),
      desc
    )
  }
  if (
    (!is.null(original) && trimws(original) != version) ||
      (is.null(original) && !length(previous))
  ) {
    on.exit(
      if (is.null(original)) {
        unlink(path)
      } else {
        writeBin(charToRaw(original), path)
      },
      add = TRUE
    )
    writeLines(version, path)
  }
  force(code)
}

.reproduce_render <- function(version, profile = NULL) {
  if (!is.null(profile)) {
    previous <- xfun::set_envvar(c(PROJR_PROFILE = profile))
    on.exit(xfun::set_envvar(previous), add = TRUE)
  }
  .env_set(unset = TRUE)
  on.exit(.env_unset(), add = TRUE, after = FALSE)
  context <- xfun::set_envvar(c(PROJR_BUILD_VERSION = .version_v_add(version)))
  on.exit(xfun::set_envvar(context), add = TRUE, after = FALSE)
  if (.is_env_var_true("PROJR_REPRODUCE_ISOLATED")) {
    libs <- strsplit(Sys.getenv("R_LIBS"), .Platform$path.sep, fixed = TRUE)[[
      1L
    ]]
    session <- callr::r_session$new(callr::r_session_options(
      libpath = unique(c(libs, .Library))
    ))
    on.exit(session$close(), add = TRUE, after = FALSE)
    old <- options(projr.reproduce.session = session)
    on.exit(options(old), add = TRUE, after = FALSE)
  }
  projr_yml_check()
  .yml_hooks_check_exist(NULL)
  engines <- .engines_get(.scripts_get(NULL, "patch", NULL))
  sources <- .reproduce_sources(engines)
  project_files <- Sys.glob(.path_get(c("_quarto*.yml", "_bookdown.yml")))
  if (
    any(names(engines) %in% c("quarto_project", "bookdown")) &&
      any(fs::link_exists(project_files))
  ) {
    stop(
      "Cannot rewrite a linked project configuration during replay.",
      call. = FALSE
    )
  }
  # Frozen project results can otherwise bypass the checked-out analysis code.
  native_cache <- if ("quarto_project" %in% names(engines)) .path_get("_freeze")
  .reproduce_check_dirs(sources, native_cache)
  restore_project <- .build_engines_backup_project(engines)
  on.exit(restore_project(), add = TRUE, after = FALSE)
  .build_pre_hooks_run()
  .reproduce_at_version(version, {
    # Hooks may change the configuration. Check every target before clearing any.
    dirs <- .reproduce_check_dirs(sources, native_cache)
    outputs <- c(
      .yml_dir_get_label_output(NULL),
      .yml_dir_get_label_docs(NULL),
      .yml_dir_get_label_cache(NULL)
    )
    clean <- unique(c(unname(dirs[outputs]), native_cache))
    .cli_step("Clearing the replay's output and cache directories")
    fs::dir_delete(clean[dir.exists(clean)])
    fs::dir_create(unname(dirs[outputs]))
    .build_include_check_config()
    # The restored raw inputs already include the licenses recorded for this build.
    for (cache in .yml_dir_get_label_cache(NULL)) {
      .license_dir_create(cache, safe = FALSE)
    }
    engines <- .build_engines_override_project(engines, restore_project)
    rendered <- .build_engines(engines, version_run_on_list = NULL)
    # Validate again before collecting files from native renderer output paths.
    dirs <- .reproduce_check_dirs(sources, native_cache)
    products <- .build_copy_docs_targets(rendered)$source
    .reproduce_check_paths(
      products,
      c(dirs[.yml_dir_get_label_raw(NULL)], sources)
    )
    if (dir.exists(projr_path_get_dir("data", create = FALSE))) {
      .reproduce_check_paths(
        .path_get("data"),
        dirs[.yml_dir_get_label_raw(NULL)]
      )
    }
    if (
      file.exists(.dir_get_cache_auto_version(
        "R",
        "sysdata.rda",
        profile = NULL
      ))
    ) {
      .reproduce_check_paths(
        .path_get("R", "sysdata.rda"),
        dirs[.yml_dir_get_label_raw(NULL)]
      )
    }
    .build_post_finalise_artefacts("patch", NULL, "pre", rendered)
    .build_post_hooks_run()
  })
  invisible(TRUE)
}
