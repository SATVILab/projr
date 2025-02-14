# ==========================
# Pre-build
# ==========================

.build_pre_check <- function(output_run) {
  # set and check authorisation is available
  .build_env_check(output_run)

  # check that we have Git if needed


  # check we are not missing upstream commits
  .build_exit_if_behind_upstream(output_run)
}

.build_git_check <- function(output_run) {
  need_git <- output_run && .yml_git_get_commit(NULL)
  if (need_git && !.git_repo_check_exists()) {
    stop("Git commits requested but no Git directory found")
  }
}

.build_pre_document <- function(output_run) {

  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .build_ignore()

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .build_doc_output_dir_update(FALSE)

  # ensure that pre-build, we are on dev version
  .build_ensure_dev_version()
}

.build_pre_setup_for_output_run <- function(version_run_on_list,
                                                  output_run) {
  # set the version pre-run
  .build_version_set_pre(version_run_on_list)

  # empty output directories
  # (docs, output and data)
  .build_clear_pre(output_run)
}

# misc
# -----------------
.build_output_get_bump_component <- function(bump_component) {
  if (missing(bump_component)) {
    version <- .yml_metadata_get_version_format(NULL)
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  bump_component
}

.build_output_get_msg <- function(msg) {
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

.build_check_auth_remote <- function() {
  remote_vec <- .remote_ls()
  if (length(remote_vec) == 0L) {
    return(invisible(TRUE))
  }
  if (!any(nzchar(remote_vec))) {
    return(invisible(TRUE))
  }
  for (i in seq_along(remote_vec)) {
    switch(remote_vec[[i]],
      "github" = {
        if (!.git_gh_check_auth()) stop()
      },
      "osf" = {
        if (!nzchar(.auth_get_osf_pat())) stop()
      }
    )
  }
}

# check we are not missing upstream commits
.build_exit_if_behind_upstream <- function(output_run) {
  if (!.build_exit_if_behind_upstream_check(output_run)) {
    return(invisible(FALSE))
  }
  if (.git_check_behind()) {
    stop(
      "Remote is ahead of local.\n",
      "Merge remote changes before proceeding\n",
      "(e.g by running `git fetch`, then `git merge`)."
    )
  }
  invisible(TRUE)
}

.build_exit_if_behind_upstream_check <- function(output_run) {
  if (!.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  if (!output_run) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# ignore
.build_ignore <- function() {
  old_profile <- .profile_get_raw()
  Sys.unsetenv(.PROFILE")
 .path_get_dir("docs")
  if (!identical(old_profile, "default")) {
    Sys.setenv(.PROFILE" = old_profile)
  }
 .ignore_auto()
  invisible(TRUE)
}

.build_version_set_pre <- function(version_run_on_list) {
 .version_set(version_run_on_list$desc[["run"]])
  invisible(TRUE)
}

.build_doc_output_dir_update <- function(output_run) {
  # sets docs directory correctly whenever called
  invisible(.dir_get_label("docs", safe = !output_run))
}

.build_ensure_dev_version <- function() {
  .version_get(dev_force = TRUE) |>
   .version_set()
  invisible(TRUE)
}
