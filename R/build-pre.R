# ==========================
# Pre-build
# ==========================

# misc
# -----------------
.projr_build_output_get_bump_component <- function(bump_component) {
  if (missing(bump_component)) {
    version <- projr_version_format_get()
    version_vec <- strsplit(version, split = "\\.|\\-")[[1]]
    bump_component <- version_vec[length(version_vec) - 1]
  }
  bump_component
}

.projr_build_output_get_msg <- function(msg) {
  if (is.null(msg)) {
    if (!Sys.getenv("PROJR_TEST") == "TRUE") {
      if (interactive()) {
        cat("Please enter a one-line description of change", "\n")
        msg <- readline(prompt = ">> ")
      } else {
        msg <- ""
      }
    } else {
      msg <- ""
    }
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
  projr_path_get_dir("docs")
  projr_dir_ignore()
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
