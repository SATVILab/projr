# ============================
# Pre/post # nolint: commented_code_linter.
# ============================

# renv
.projr_build_renv_snapshot <- function(output_run) {
  if (!.projr_build_renv_snapshot_check(output_run)) {
    return(invisible(FALSE))
  }
  if (.projr_yml_renv_get()) {
    renv::snapshot(prompt = FALSE)
  }
  invisible(TRUE)
}

.projr_build_renv_snapshot_check <- function(output_run) {
  output_run && !.assert_test()
}

# commit
.projr_build_git_commit <- function(output_run,
                                    bump_component,
                                    version_run_on_list,
                                    stage,
                                    msg) {
  # exit early if required
  if (!.projr_build_git_check(output_run)) {
    return(invisible(FALSE))
  }
  if (!.projr_git_repo_check_exists()) {
    stop("Git commits requested but no Git directory found")
  }

  # commit
  .projr_git_commit_all(add_untracked = .projr_yml_git_get_add_untracked())
}

.projr_build_git_check <- function(output_run) {
  output_run && .projr_yml_git_get_commit()
}

# commit messages
.projr_build_git_msg_get <- function(stage,
                                     version_run_on_list,
                                     bump_component,
                                     msg) {
  switch(stage,
    "pre" = .projr_build_git_msg_get_pre(bump_component),
    "post" = .projr_build_git_msg_get_post(
      bump_component = bump_component,
      version_run_on_list = version_run_on_list,
      msg = msg
    ),
    "dev" = .projr_build_git_msg_get_dev(version_run_on_list)
  )
}

.projr_build_git_msg_get_pre <- function(bump_component) {
  paste0("Snapshot pre-", bump_component, " build")
}

.projr_build_git_msg_get_post <- function(bump_component,
                                          version_run_on_list,
                                          msg) {
  msg_append <- ifelse(
    nzchar(msg),
    paste0(": ", msg),
    ""
  )
  paste0(
    "Record ", bump_component,
    " v", version_run_on_list[["desc"]][["success"]], " build",
    msg_append
  )
}

.projr_build_git_msg_get_dev <- function(version_run_on_list) {
  paste0("Begin v", version_run_on_list$bd[["success"]])
}

# commit
.projr_build_git_push <- function(output_run) {
  if (!output_run || !.projr_yml_git_get_push()) {
    return(invisible(FALSE))
  }
  if (!.projr_git_repo_check_exists()) {
    stop("No git repository detected but needed based on settings")
  }
  if (!.projr_git_remote_check_upstream()) {
    warning("No upstream remote detected")
  }
  .projr_git_push()
  invisible(TRUE)
}
