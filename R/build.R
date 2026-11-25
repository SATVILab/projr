#' @rdname projr_build
#' @title Build project to output
#'
#' @description .build_output` Builds project to output, which
#' means recording the input and output data hashes,
#' building the actual bookdown document and
#' saving and archiving selected output.
#'
#' .build_major`, .build_minor` and .build_patch`
#' are wrappers around .build_output` with the version component
#' bumped set automatically, e.g. projr_build_major()` is equivalent
#' projr_build(bump_component = "major")`.
#'
#' @param project Path to the project root. The default discovers the project
#'   from the working directory. The first interactive build asks permission to
#'   run its scripts and hooks, change project files and publish configured
#'   outputs. projr remembers approval for this checkout in the user's R
#'   configuration directory. In a noninteractive session, pass the root
#'   explicitly (for example, `project = "."`) to authorize this build without
#'   saving approval.
#' @param bump_component "major", "minor", "patch" or missing.
#' Specifies version component to increment.
#' If missing, then is set equal to the lowest version component
#' in used version format.
#' No default (i.e. is missing by default).
#' @param msg character.
#' Message to append to Git commit messages.
#' Default is \code{NULL}, in which
#' case the user is prompted for a message or,
#' if the session is not interactive, it is
#' left empty.
#' Default is \code{NULL}.
#' Note that the Git messages in this case would not be blank -
#' they would simply consist of details as to the version
#' being bumped to and the stage in the build process
#' at which the commit was made.
#' @param profile character.
#' `projr` profile to use. Sets `PROJR_PROFILE` for the build, so it
#' overrides that variable and the `profile` key in `_projr-local.yml` and
#' `_projr.yml`.
#' @param archive_github `TRUE`,`FALSE` orcharacter vector of
#' directory labels.
#' If `TRUE`, then all directories (`raw-data`, `output`, etc)
#' are uploaded to a GitHub release named `archive`
#' as versioned files (e.g. `output-v0.1.2.zip`).
#' If `FALSE`, then no directories are uploaded.
#' If a character vector, then only the directories
#' specified are uploaded.
#' Default is `FALSE`.
#' Ignored if a release named `archive`
#' is already selected under `build.dest` in
#' the `projr` configuration file.
#' Otherwise, `remotes.github.archive` is registered in `_projr.yml`
#' if absent, without adding it to `build.dest`.
#' @param archive_local `TRUE`, `FALSE` or character vector of
#' directory labels.
#' If `TRUE`, then all directories (`raw-data`, `output`, etc)
#' are archived to a local directory.
#' If `FALSE`, then no directories are archived locally.
#' If a character vector, then only the directories
#' specified are archived.
#' Default is `FALSE`.
#' When used, `remotes.local.archive` (with `path: _archive`) is registered
#' in `_projr.yml` if absent, without adding it to `build.dest`.
#' @param always_archive logical.
#' If `TRUE`, then the directories are uploaded
#' regardless of whether the directory to be uploaded
#' has exactly the same contents as the latest version
#' of the directory on the GitHub release.
#' Default is `TRUE`.
#' Ignored if a release named `archive`
#' is already selected under `build.dest` in
#' the `projr` configuration file.
#' @param args_engine List of options for the selected build engines. For one
#'   engine, use its native arguments directly, e.g. `list(quiet = TRUE)` for
#'   one or more standalone R Markdown documents. When multiple engines run,
#'   group options by engine, e.g.
#'   `list(quarto_project = list(profile = "print"), rmd = list(quiet = TRUE))`.
#'   Use `quarto_project` or `quarto_document` for [quarto::quarto_render()],
#'   `rmd` for [rmarkdown::render()] and `bookdown` for [bookdown::render_book()].
#'   Plain R scripts (`r`) take no render options. Omit any engine to use its
#'   defaults. The default `list()` works for any selection.
#'   Quarto renders in the foreground so later scripts and hooks see its output.
#' @param clear_output character.
#' When to clear output directories: "pre" (before build, default),
#' "post" (after build), or "never".
#' Can also be set via PROJR_CLEAR_OUTPUT environment variable.
#'
#' @export
projr_build <- function(
  bump_component,
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
) {
  old_wd <- setwd(.build_prepare_project(project))
  on.exit(setwd(old_wd), add = TRUE)
  bump_component <- .build_output_get_bump_component(
    bump_component
  )
  msg <- .build_output_get_msg(msg)

  .build(
    bump_component = bump_component,
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output
  )
}

#' @rdname projr_build
#' @export
projr_build_major <- function(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
) {
  projr_build(
    bump_component = "major",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output,
    project = project
  )
}

#' @rdname projr_build
#' @export
projr_build_minor <- function(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
) {
  projr_build(
    bump_component = "minor",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output,
    project = project
  )
}

#' @rdname projr_build
#' @export
projr_build_patch <- function(
  msg = NULL,
  args_engine = list(),
  profile = NULL,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL,
  project = NULL
) {
  projr_build(
    bump_component = "patch",
    msg = msg,
    args_engine = args_engine,
    profile = profile,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    clear_output = clear_output,
    project = project
  )
}

#' @title Build dev project
#' @inheritParams projr_build
#'
#' @description Runs the selected scripts and renders documents for development.
#' Development builds preserve the project's version and can run before its
#' first versioned build. Staged outputs are available in the build cache.
#'
#' @param scripts character vector.
#' Paths or glob patterns for scripts to build, relative to the project root
#' or absolute. Prefix a pattern with `!` to exclude matches. Use an empty
#' character vector or `""` to skip running scripts. The default, `NULL`,
#' checks `dev.scripts` and then `build.scripts`. If neither is set, a
#' Quarto or Bookdown project renders before automatically detected R scripts.
#' Without a project, scripts are detected in the project root and run
#' alphabetically. When every entry is an exclusion, use these automatic rules
#' and skip the matching files. In a YAML scripts list, use an entry such
#' as `- exclude: scratch.R` or a quoted string such as `"!scratch.R"`.
#'
#' Explicit selections run in order, with each glob expanded alphabetically. Listing an R script or
#' standalone document twice runs it twice. A repeated project document is an error.
#' If a selection includes Quarto or Bookdown project documents, those
#' documents render together at the position of the last selected project
#' document. The project YAML is restored after rendering and post-build hooks.
#' @param bump logical.
#' Retained for compatibility. Supplying `TRUE` warns and runs a development
#' build with the project's existing version, if any.
#' @param old_dev_remove logical.
#' If `TRUE`, then previous development builds are deleted
#' after a successful run.
#' @param profile character.
#' `projr` profile to use. Sets `PROJR_PROFILE` for the build, so it
#' overrides that variable and the `profile` key in `_projr-local.yml` and
#' `_projr.yml`.
#' @param clear_output character.
#' When to clear output directories: "pre" (before build),
#' "post" (after build), or "never" (default for dev builds).
#' Can also be set via PROJR_CLEAR_OUTPUT environment variable.
#'
#' @return Invisibly returns `TRUE` after a successful build.
#' @examples
#' \dontrun{
#' projr_build_dev(scripts = c("reports/*.Rmd", "!reports/draft.Rmd"))
#' }
#'
#' @export
projr_build_dev <- function(
  scripts = NULL,
  bump = FALSE,
  old_dev_remove = TRUE,
  args_engine = list(),
  profile = NULL,
  clear_output = "never",
  project = NULL
) {
  .assert_flag(bump, TRUE)
  .assert_flag(old_dev_remove, TRUE)
  if (bump) {
    warning(
      "Development builds do not change the project version. Remove bump = TRUE.",
      call. = FALSE
    )
  }
  old_wd <- setwd(.build_prepare_project(project))
  on.exit(setwd(old_wd), add = TRUE)
  .build(
    scripts = scripts,
    bump_component = NULL,
    old_dev_remove = old_dev_remove,
    args_engine = args_engine,
    profile = profile,
    clear_output = clear_output
  )
}

.build_init_config <- function(root) {
  starter <- system.file(
    "project_structure",
    "_projr.yml",
    package = "projr",
    mustWork = TRUE
  )
  config <- yaml::read_yaml(starter)
  if (!file.exists(file.path(root, ".git"))) {
    config$build$git <- FALSE
  } else if (!.build_has_upstream(root)) {
    config$build$git <- list(push = FALSE)
  }
  # Never overwrite configuration created by another process during discovery.
  tmp <- tempfile(tmpdir = root)
  on.exit(unlink(tmp), add = TRUE)
  yaml::write_yaml(config, tmp)
  fs::file_copy(tmp, file.path(root, "_projr.yml"))
  .init_ignore_starter(root)
  invisible(TRUE)
}

.build_has_upstream <- function(root) {
  if (.git_system_get() == "git") {
    args <- c(
      "-C",
      shQuote(root),
      "rev-parse",
      "--verify",
      "--quiet",
      shQuote("@{upstream}")
    )
    return(system2("git", args, stdout = FALSE, stderr = FALSE) == 0L)
  }
  upstream <- gert::git_info(repo = root)$upstream
  length(upstream) == 1L && !is.na(upstream) && nzchar(upstream)
}

.build <- function(
  scripts = NULL,
  bump_component,
  old_dev_remove = TRUE,
  msg = "",
  args_engine,
  profile,
  archive_github = FALSE,
  archive_local = FALSE,
  always_archive = TRUE,
  clear_output = NULL
) {
  if (!is.null(profile)) {
    old_profile <- Sys.getenv("PROJR_PROFILE")
    Sys.setenv(PROJR_PROFILE = profile)
    on.exit(Sys.setenv(PROJR_PROFILE = old_profile), add = TRUE, after = FALSE)
  }
  # set and unset environment variables
  .env_set(unset = TRUE)
  on.exit(.env_unset(), add = TRUE, after = FALSE)
  engines <- tryCatch(
    .engines_get(.scripts_get(scripts, bump_component, profile = NULL)),
    error = identity
  )
  if (!inherits(engines, "error")) {
    args_by_engine <- .build_args_by_engine(engines, args_engine)
  }
  restore_library <- .build_renv_library_start()
  on.exit(restore_library(), add = TRUE, after = FALSE)
  output_run <- .build_get_output_run(bump_component)
  version_run_on_list <- if (output_run) {
    .version_run_onwards_get(bump_component, initial = "0.0.1")
  }
  version <- if (output_run) {
    version_run_on_list$desc[["run"]]
  } else {
    .version_get_v(required = FALSE)
  }
  restore_context <- .template_context_start(version)
  on.exit(restore_context(), add = TRUE, after = FALSE)
  clear_output <- .build_get_clear_output(clear_output)

  build_type <- if (output_run) "output" else "dev"

  # Script selection can fail before the normal build ignore refresh.
  .ignore_project_state()
  .log_build_init(
    build_type = build_type,
    bump_component = bump_component,
    msg = msg
  )
  .cli_debug(
    "projr profiles: {paste(projr_profile_get(), collapse = ', ')} (from {if (is.null(profile)) .profile_get_source() else 'the profile argument'})"
  )
  if (!inherits(engines, "error")) {
    args_by_engine <- .build_args_quarto_profile(args_by_engine, names(engines))
  }

  # Track build success for logging
  build_success <- FALSE
  time_start <- Sys.time()

  finish_log <- function(success) {
    .log_build_finalize(success, time_start)
    .log_history_add(build_type, bump_component, msg, success)
  }
  record_failure <- function(condition) {
    detail <- conditionMessage(condition)
    if (
      inherits(condition, "interrupt") && (!length(detail) || !nzchar(detail))
    ) {
      detail <- "Build interrupted"
    }
    tryCatch(
      {
        .log_build_append(detail, "error")
        finish_log(FALSE)
      },
      error = function(log_error) {
        message("Could not finish the build log: ", conditionMessage(log_error))
      }
    )
    stop(condition)
  }
  tryCatch(
    withCallingHandlers(
      {
        # Show initial build stage header
        .cli_stage_header(
          "Pre-Build Preparation",
          build_type
        )

        # Report script-selection errors through the normal build log.
        if (inherits(engines, "error")) {
          stop(engines)
        }
        restore_project <- .build_engines_backup_project(engines)
        on.exit(restore_project(), add = TRUE, after = FALSE)
        documents <- .build_document_sources(engines)
        .build_pre(
          bump_component,
          msg,
          clear_output,
          archive_github,
          archive_local,
          always_archive,
          engines,
          documents,
          version_run_on_list
        )
        render_dirs <- if (any(names(engines) != "r")) {
          unique(c(.path_get(), dirname(documents)))
        } else {
          character()
        }
        intermediates_before <- .ignore_auto_document_intermediates(render_dirs)
        on.exit(
          {
            if (!build_success) {
              .ignore_auto_failed_document_outputs(
                render_dirs,
                intermediates_before
              )
            }
          },
          add = TRUE,
          after = FALSE
        )
        engines <- .build_engines_override_project(
          engines,
          restore_project,
          args_by_engine[["quarto_project"]][["profile"]]
        )

        # Build stage
        if (length(engines)) {
          .cli_stage_header("Build", build_type)
        } else {
          .cli_info("No scripts or documents selected")
        }
        rendered <- .build_engines(engines, version_run_on_list, args_by_engine)

        # Post-build stage
        .cli_stage_header("Post-Build", build_type)
        total_time <- Sys.time() - time_start
        .build_post(
          version_run_on_list,
          bump_component,
          msg,
          old_dev_remove,
          archive_github,
          archive_local,
          always_archive,
          clear_output,
          total_time,
          rendered,
          restore_project
        )

        build_success <- TRUE
      },
      warning = function(w) {
        .log_build_append(conditionMessage(w), "warning")
      }
    ),
    error = record_failure,
    interrupt = record_failure
  )

  finish_log(TRUE)
  # The detailed log already ends with the build status.
  if (.cli_should_show("std")) {
    cli::cli_alert_success("Build completed successfully")
  }
  invisible(TRUE)
}

.build_get_output_run <- function(bump_component) {
  !(is.null(bump_component) || bump_component == "dev")
}

.build_ensure_version <- function() {
  if (!.version_exists()) {
    projr_version_set("0.0.1")
  }
}

.build_get_clear_output <- function(clear_output) {
  clear_output <- clear_output %||%
    Sys.getenv("PROJR_CLEAR_OUTPUT", unset = "pre")
  .assert_in_single(clear_output, c("pre", "post", "never"), TRUE)
  clear_output
}

# pre
# ------------------------
.build_pre <- function(
  bump_component,
  msg,
  clear_output,
  archive_github,
  archive_local,
  always_archive,
  engines,
  documents,
  version_run_on_list
) {
  .cli_step("Checking project configuration")

  projr_yml_check(NULL)
  .yml_hooks_check_exist(NULL)
  # whether it's an output run  or not
  output_run <- .build_get_output_run(bump_component)

  # Plan raw-input fetches (downloads into staging only) before any changes.
  source_workspace <- .dir_create_tmp_random()
  on.exit(unlink(source_workspace, recursive = TRUE), add = TRUE)
  source_plan <- .source_plan(source_workspace)

  if (output_run) {
    .build_check_remote_versions(
      version_run_on_list$desc[["run"]],
      bump_component,
      archive_github,
      archive_local,
      always_archive
    )
    .build_ensure_version()
  }

  # check required env vars are present, and
  # that we're not behind upstream remote,
  # and that we have Git and  Git remote setup
  .build_pre_check(output_run, engines)

  # Prepare remote destinations (GitHub releases)
  .build_pre_remotes_prepare(
    bump_component = bump_component,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive
  )
  .build_pre_register_archives(output_run, archive_github, archive_local)

  # update reng, ignore files, doc directory and version
  .build_pre_document(output_run, archive_local)
  .ignore_auto_document_outputs(documents)

  .source_install(source_plan)

  # run any pre-build hooks (backward compatible with build.script)
  # For dev builds, use dev.hooks; for production builds, use build.hooks
  .build_pre_hooks_run(.is_dev_build(bump_component))

  # clear output and docs directories, and set
  # run version to output run version if need be
  .build_pre_setup_for_output_run(
    version_run_on_list,
    output_run,
    clear_output
  )

  # commit any unstaged files pre-run
  .build_pre_commit_git(
    bump_component,
    version_run_on_list,
    msg
  )

  # Output Git information for debug (after pre-build commit)
  .build_debug_git_info()

  # create licenses for input directories (raw-data, cache)
  .license_dir_create_all_pre(output_run)

  # hash cache and raw directories
  .build_manifest_pre(output_run)

  # return version_run_on_list
  invisible(version_run_on_list)
}

# post
# ------------------------
.build_post <- function(
  version_run_on_list,
  bump_component,
  msg,
  old_dev_remove,
  archive_github,
  archive_local,
  always_archive,
  clear_output,
  total_time,
  engines,
  restore_project
) {
  output_run <- .build_get_output_run(bump_component)

  # move artefacts to unsafe, final directories
  .build_post_finalise_artefacts(
    bump_component,
    version_run_on_list,
    clear_output,
    engines
  )

  # update documentation
  .build_post_document(
    bump_component,
    version_run_on_list,
    msg,
    total_time,
    .buildlog_get_destinations(archive_github, archive_local)
  )

  # record (version, rmd, git)
  .build_post_commit_git(bump_component, version_run_on_list, msg)

  # send to remotes
  .build_post_send_dest(
    bump_component,
    old_dev_remove,
    archive_github,
    archive_local,
    always_archive
  )

  # run post-build hooks (backward compatible with build.script)
  # For dev builds, use dev.hooks; for production builds, use build.hooks
  .build_post_hooks_run(.is_dev_build(bump_component))

  restore_project()

  # initiate dev version
  .build_post_dev(bump_component, version_run_on_list, msg)

  # push
  .build_git_push(output_run)

  invisible(TRUE)
}

# update lock file, help files, citation files, README
# and CHANGELOG
.build_post_document <- function(
  bump_component,
  version_run_on_list,
  msg,
  total_time,
  destinations = .buildlog_get_destinations()
) {
  output_run <- .build_get_output_run(bump_component)
  if (!output_run) {
    .cli_debug("Development builds skip production metadata and publication")
    return(invisible(FALSE))
  }

  .cli_step("Recording build metadata")
  .build_renv_snapshot(output_run)
  .build_roxygenise(output_run)
  .build_cite(output_run)
  .cli_debug("Updating CHANGELOG.md")
  .build_changelog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list
  )

  # hash raw-data and outputs, then save manifest table
  .build_manifest_post(output_run)

  .cli_debug("Updating .projr/BUILDLOG.md")
  .build_buildlog_add(
    msg = msg,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    total_time = total_time,
    destinations = destinations
  )

  # update README
  .build_readme_rmd_render(output_run)
}


# organise files and folders (clear and copy)
.build_post_finalise_artefacts <- function(
  bump_component,
  version_run_on_list,
  clear_output,
  engines
) {
  output_run <- .build_get_output_run(bump_component)
  .build_clear_post_safe(
    output_run,
    clear_output,
    names(engines),
    as.character(unlist(engines[names(engines) == "document_output"]))
  )
  .build_copy(
    output_run,
    bump_component,
    version_run_on_list,
    engines,
    clear_output
  )
}

# record (version, rmd, git)
.build_pre_commit_git <- function(bump_component, version_run_on_list, msg) {
  output_run <- .build_get_output_run(bump_component)

  # commit any files generated by run
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "pre",
    msg = msg
  )
}

# record (version, rmd, git)
.build_post_commit_git <- function(bump_component, version_run_on_list, msg) {
  output_run <- .build_get_output_run(bump_component)

  # commit any files generated by run
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "post",
    msg = msg
  )
}

# send to remotes
.build_post_send_dest <- function(
  bump_component,
  old_dev_remove,
  archive_github,
  archive_local,
  always_archive
) {
  # Lenient: prepare all required GitHub releases for this build (non-blocking)
  .dest_prepare_github_releases(
    bump_component,
    archive_github,
    archive_local,
    always_archive,
    strict = FALSE
  )

  .dest_send(
    bump_component,
    archive_github,
    archive_local,
    always_archive
  )
  .build_clear_old(
    .build_get_output_run(bump_component),
    old_dev_remove
  )
}

# bump to dev
.build_post_dev <- function(bump_component, version_run_on_list, msg) {
  output_run <- .build_get_output_run(bump_component)

  # Only bump to dev version for production builds
  if (!output_run) {
    return(invisible(FALSE))
  }

  # Get current version and append dev component
  version_current_vec <- .version_current_vec_get(dev_force = FALSE)
  version_format_list <- .version_format_list_get(NULL)

  # Append dev component if not already there
  version_dev_vec <- .version_run_onwards_get_dev_append_dev(
    version_current_vec,
    version_format_list$sep
  )
  version_dev <- .version_concat(version_dev_vec, version_format_list$sep)

  # Set the dev version
  .cli_step("Setting development version to {version_dev}")
  projr_version_set(version_dev)

  # Commit dev version
  .build_git_commit(
    output_run = output_run,
    bump_component = bump_component,
    version_run_on_list = version_run_on_list,
    stage = "dev",
    msg = msg
  )
}
