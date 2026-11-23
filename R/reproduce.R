#' Rebuild an analysis and compare its files with a completed build
#'
#' Clone a trusted analysis, restore its recorded inputs and run its production
#' scripts from empty output folders. projr compares the resulting files with
#' the build's recorded filenames and hashes. The scripts and hooks run with
#' your usual R permissions. The replay itself creates no version or release.
#'
#' @param repo A GitHub repository in `owner/repo` form, optionally followed by `@branch`, or a local Git repository.
#' @param branch Branch to search. Defaults to the repository's default branch.
#' @param version Exact completed build version, with or without a leading v.
#' @param commit Full or abbreviated commit SHA on the selected branch. Alone,
#'   it selects the last completed build at or before this commit and runs that
#'   build's code. With `version`, it selects the code to run against that
#'   version's inputs and expected outputs.
#' @param path New or empty directory for the clone. By default it is created
#'   in the R session's temporary directory. Supply a path to keep the results.
#' @param destination GitHub archive title or release tag for recovering missing
#'   snapshots and inputs. Defaults to `"archive"`.
#' @details With neither selector, reproduce the latest completed build on the
#'   branch. A version on its own selects that build and its code.
#'   With a lockfile, projr restores its package versions with renv. With only
#'   a DESCRIPTION, pak installs its dependencies and their versions are
#'   unpinned. With neither file, the analysis uses the available R libraries.
#'   The input, output and cache paths used during replay must be inside the
#'   clone. Archives can be stored elsewhere.
#' @return A list with `exact` for the whole comparison, a `directories` data
#'   frame with one row per output or documentation directory and counts of
#'   identical, changed, missing and extra files, and a `files` data frame
#'   with each label, relative path and status. The result also identifies the
#'   checkout path, version, code commit, build record commit and dependency
#'   method. `dependencies$packages` lists package names and versions and
#'   `dependencies$pinned` indicates whether the run used a lockfile.
#'   A missing baseline cannot produce an exact match.
#' @examples
#' \dontrun{
#' result <- projr_reproduce("owner/study", branch = "analysis", version = "1.2.3")
#' result$exact
#' subset(result$files, status != "identical")
#' }
#' @export
projr_reproduce <- function(
  repo,
  branch = NULL,
  version = NULL,
  commit = NULL,
  path = tempfile("projr-reproduce-"),
  destination = "archive"
) {
  .assert_string(destination, TRUE)
  git_environment <- .reproduce_git_environment()
  on.exit(xfun::set_envvar(git_environment), add = TRUE)
  build <- .reproduce_checkout(repo, branch, commit, version, path)
  path <- build$path
  scratch <- tempfile("projr-reproduce-stage-")
  fs::dir_create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  baseline_path <- path
  if (build$code_commit != build$build_commit) {
    baseline_path <- file.path(scratch, "baseline")
    worktree <- function(...) {
      .git_run_write(
        shQuote(c("-C", path, "worktree", ...)),
        stdout = TRUE,
        stderr = TRUE
      )
    }
    worktree("add", "--quiet", "--detach", baseline_path, build$build_commit)
    on.exit(
      worktree("remove", "--force", baseline_path),
      add = TRUE,
      after = FALSE
    )
  }
  old <- setwd(baseline_path)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  restore_context <- .template_context_start(build$version)
  on.exit(restore_context(), add = TRUE)
  Sys.setenv(PROJR_BUILD_HEAD = paste0("ref: refs/heads/", build$branch))
  record <- .get_build_record(.buildlog_read(), build$version)
  old_profile <- xfun::set_envvar(c(
    PROJR_PROFILE = paste(record$profile, collapse = ",")
  ))
  on.exit(xfun::set_envvar(old_profile), add = TRUE)

  baseline <- .reproduce_baseline(build$version, destination)
  dirs <- .reproduce_check_dirs()
  raw <- names(dirs)[.yml_dir_label_class_detect_raw(names(dirs))]
  inputs <- .reproduce_inputs(raw, build$version, destination, scratch)

  setwd(path)
  dirs <- .reproduce_check_dirs()
  code_raw <- names(dirs)[.yml_dir_label_class_detect_raw(names(dirs))]
  if (!setequal(code_raw, raw)) {
    stop(
      "The selected code and build declare different raw input labels.",
      call. = FALSE
    )
  }
  for (label in raw) {
    target <- dirs[[label]]
    if (dir.exists(target)) {
      fs::dir_delete(target)
    }
    fs::dir_create(dirname(target))
    fs::dir_copy(inputs[[label]], target)
  }
  dependencies <- .reproduce_run(build$version, scratch)
  result <- .reproduce_compare(baseline)
  c(result, build, list(dependencies = dependencies))
}

.reproduce_inputs <- function(labels, version, destination, scratch) {
  if (!length(labels)) {
    return(character())
  }
  stage <- file.path(scratch, "inputs")
  # Tracked inputs and configured local archives work offline. The checkout's
  # GitHub release can supply raw files missing from both.
  tryCatch(
    projr_get_local(
      path = getwd(),
      label = labels,
      version = version,
      out = stage
    ),
    error = function(local_error) {
      .cli_debug("Local input retrieval: {conditionMessage(local_error)}")
      projr_restore(
        label = labels,
        version = version,
        destination = destination,
        clear = TRUE
      )
      projr_get_local(
        path = getwd(),
        label = labels,
        version = version,
        out = stage
      )
    }
  )
}
