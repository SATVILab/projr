# ==========================
# Pre-build
# ==========================

.projr_build_pre_check <- function(output_run) {
  # set and check authorisation is available
  .projr_build_env_check(output_run)

  # check that we have Git if needed


  # check we are not missing upstream commits
  .projr_build_exit_if_behind_upstream(output_run)
}

.projr_build_git_check <- function(output_run) {
  need_git <- output_run && .projr_yml_git_get_commit(NULL)
  if (need_git && !.projr_git_repo_check_exists()) {
    stop("Git commits requested but no Git directory found")
  }
}

.projr_build_pre_document <- function(output_run) {

  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .projr_build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .projr_build_ignore()

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .projr_build_doc_output_dir_update(FALSE)

  # ensure that pre-build, we are on dev version
  .projr_build_ensure_dev_version()
}

.projr_build_pre_setup_for_output_run <- function(version_run_on_list,
                                                  output_run) {
  # set the version pre-run
  .projr_build_version_set_pre(version_run_on_list)

  # empty output directories
  # (docs, output and data)
  .projr_build_clear_pre(output_run)
}

# misc
# -----------------
.projr_build_output_get_bump_component <- function(bump_component) {
  if (missing(bump_component)) {
    version <- .projr_yml_metadata_get_version_format(NULL)
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  bump_component
}

.projr_build_output_get_msg <- function(msg) {
  if (is.null(msg) || .is_len_0(msg)) {
    if (!.is_test()) {
      if (interactive()) {
        cat("Please enter a one-line description of change", "\n")
        msg <- readline(prompt = ">> ")
        while (.is_len_0(msg)) {
          cat("Message cannot be empty", "\n")
          cat("Please enter a one-line description of change", "\n")
          msg <- readline(prompt = ">> ")
        }
      } else {
        msg <- "Build project"
      }
    } else {
      msg <- "Build project"
    }
  } else {
    .assert_string(msg)
  }
  msg
}

.projr_build_check_auth_remote <- function() {
  remote_vec <- .projr_remote_ls()
  if (length(remote_vec) == 0L) {
    return(invisible(TRUE))
  }
  if (!any(nzchar(remote_vec))) {
    return(invisible(TRUE))
  }
  for (i in seq_along(remote_vec)) {
    switch(remote_vec[[i]],
      "github" = {
        if (!.projr_git_gh_check_auth()) stop()
      },
      "osf" = {
        if (!nzchar(.projr_auth_get_osf_pat())) stop()
      }
    )
  }
}

# check we are not missing upstream commits
.projr_build_exit_if_behind_upstream <- function(output_run) {
  if (!.projr_build_exit_if_behind_upstream_check(output_run)) {
    return(invisible(FALSE))
  }
  if (.projr_git_check_behind()) {
    stop(
      "Remote is ahead of local.\n",
      "Merge remote changes before proceeding\n",
      "(e.g by running `git fetch`, then `git merge`)."
    )
  }
  invisible(TRUE)
}

.projr_build_exit_if_behind_upstream_check <- function(output_run) {
  if (!.projr_git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  if (!output_run) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# ignore
.projr_build_ignore <- function() {
  old_profile <- .projr_profile_get_raw()
  Sys.unsetenv("PROJR_PROFILE")
  projr_path_get_dir("docs")
  if (!identical(old_profile, "default")) {
    Sys.setenv("PROJR_PROFILE" = old_profile)
  }
  projr_ignore_auto()
  invisible(TRUE)
}

.projr_build_version_set_pre <- function(version_run_on_list) {
  projr_version_set(version_run_on_list$desc[["run"]])
  invisible(TRUE)
}

.projr_build_doc_output_dir_update <- function(output_run) {
  # sets docs directory correctly whenever called
  invisible(.projr_dir_get_label("docs", safe = !output_run))
}

.projr_build_ensure_dev_version <- function() {
  .projr_version_get(dev_force = TRUE) |>
    projr_version_set()
  invisible(TRUE)
}
