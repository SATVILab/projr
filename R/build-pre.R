# ==========================
# Pre-build
# ==========================

.build_pre_check <- function(output_run, output_level = "std", log_file = NULL) {
  # set and check authorisation is available
  .cli_debug("Checking environment variables", output_level = output_level, log_file = log_file)
  .build_env_check(output_run)

  # check that we have Git if needed
  .cli_debug("Checking Git repository", output_level = output_level, log_file = log_file)
  .build_git_check(output_run)

  # check that we have GitHub remote if needed
  .cli_debug("Checking GitHub remote", output_level = output_level, log_file = log_file)
  .build_github_check(output_run)

  # check we are not missing upstream commits
  .cli_debug("Checking upstream commits", output_level = output_level, log_file = log_file)
  .build_exit_if_behind_upstream(output_run)
}

.build_git_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  # force setup if explicitly required
  if (.build_git_check_required()) {
    return(.build_git_required_setup())
  }
  if (.build_git_check_depends()) {
    return(.build_git_depends_setup())
  } else {
    return(.build_git_depends_disable())
  }
}

.build_git_check_required <- function() {
  # only return TRUE if explicitly required
  yml_git <- .yml_git_get(NULL)
  if (is.null(yml_git) || isFALSE(yml_git)) {
    return(FALSE)
  }
  if (isTRUE(yml_git)) {
    return(TRUE)
  }
  if (is.null(yml_git[["commit"]])) {
    return(FALSE)
  }
  yml_git[["commit"]]
}

.build_git_required_setup <- function() {
  cli::cli_alert_info("Git is explicitly required but no Git repository was found.")
  cli::cli_alert_info("Creating a new Git repository...")
  projr_init_git(TRUE)
}

.build_git_check_depends <- function() {
  # check if we need to set up Git
  yml_git <- .yml_git_get(NULL)
  # do nothing if explicitly not requested
  if (isFALSE(yml_git)) {
    return(FALSE)
  }
  if (!is.null(yml_git[["commit"]]) && !yml_git[["commit"]]) {
    return(FALSE)
  }
  # check if it's an option
  if (interactive()) {
    cli::cli_alert_warning("Git repository not found.")
    cli::cli_inform("It is not required, but recommended, and projr will handle setup and (by default) commits for you.")
    cli::cli_inform("If you choose not to create one now, then Git handling by projr will be disabled.")
    choice <- menu(
      c("Yes", "No"),
      title = "Do you want to create a new Git repository?"
    )
    choice == 1
  } else {
    cli::cli_alert_warning("Git repository not found.")
    cli::cli_inform("Git repository will be created automatically, as in non-interactive mode.")
    TRUE
  }
}

.build_git_depends_setup <- function() {
  cli::cli_alert_info("Git repository setup requested.")
  cli::cli_alert_info("Creating a new Git repository...")
  projr_init_git(TRUE)
}

.build_git_depends_disable <- function() {
  cli::cli_alert_info("Git repository setup not requested.")
  cli::cli_inform("Disabling Git handling by projr.")
  cli::cli_inform("You can re-enable it by running `projr_yml_git_set(TRUE)`.")
  projr_yml_git_set(FALSE)
  invisible(TRUE)
}

# github
# -----------------
.build_github_check <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    return(invisible(FALSE))
  }
  if (.git_remote_check_exists()) {
    return(invisible(FALSE))
  }
  # check if we need to set up GitHub
  yml_git <- .yml_git_get(NULL)
  # do nothing if explicitly not requested
  if (is.null(yml_git) || isFALSE(yml_git)) {
    return(invisible(FALSE))
  }
  if (!isTRUE(yml_git)) {
    yml_commit_false <- isFALSE(yml_git[["commit"]])
    yml_push_false <- isFALSE(yml_git[["push"]])
    if (yml_commit_false || yml_push_false) {
      return(invisible(FALSE))
    }
  }
  if (isTRUE(yml_git) || isTRUE(yml_git[["push"]])) {
    cli::cli_alert_warning("GitHub remote required, but not found.")
    cli::cli_inform("Will attempt to create a new GitHub remote.")
    return(invisible(TRUE))
  }
  # check if it's an option
  if (interactive()) {
    cli::cli_alert_warning("GitHub remote not found.")
    cli::cli_inform("It is not required, but recommended, and projr will help handle setup for you.") # nolint
    choice <- menu(
      c("Yes", "No"),
      title = "Do you want to create a new GitHub remote?"
    )
    if (choice == 2) {
      cli::cli_alert_info("Disabling automated pushes to GitHub by projr.")
      cli::cli_inform("You can re-enable it by running `projr_yml_git_set(push = TRUE)`.") # nolint
      projr_yml_git_set(push = FALSE)
      return(FALSE)
    }
    TRUE
  } else {
    cli::cli_alert_info("GitHub remote not found.")
    cli::cli_alert_info("GitHub remote will be created automatically, as in non-interactive mode.") # nolint
    TRUE
  }
}

.build_github_setup <- function() {
  cli::cli_alert_info("GitHub remote setup requested.")
  cli::cli_alert_info("Creating a new GitHub remote...")
  .build_github_setup_check_pat()
  user <- .build_github_setup_user()
  .build_github_setup_repo(user)
  cli::cli_alert_info("GitHub remote created successfully.")
}

.build_github_setup_check_pat <- function() {
  if (!.git_gh_check_auth()) {
    stop("GitHub PAT not found.")
  }
  invisible(TRUE)
}

.build_github_setup_user <- function() {
  user <- gh::gh_whoami()$login
  if (!.is_string(user)) {
    stop("GitHub user not found.")
  }
  if (!interactive()) {
    return(c("user" = user))
  }
  choice <- menu(
    c(user, "Organisation"),
    title = paste0("Do you want to set up the GitHub remote under user ", user, ", or under an organisation?") # nolint
  )
  if (choice == 1) {
    return(c("user" = user))
  }
  cli::cli_alert_info("Please enter the name of the organisation:")
  org <- readline(prompt = ">> ")
  while (.is_len_0(org)) {
    cli::cli_alert_warning("Organisation name cannot be empty.")
    cli::cli_alert_info("Please enter the name of the organisation:")
    org <- readline(prompt = ">> ")
  }
  c("org" = org)
}

.build_github_setup_repo <- function(user) {
  cli::cli_alert_info("Creating new GitHub repository...")
  if ("user" %in% names(user)) {
    .init_github_actual_user(FALSE)
  }
  if ("org" %in% names(user)) {
    .init_github_actual_org(FALSE, user[["org"]])
  }
}

.build_pre_document <- function(output_run, archive_local, output_level = "std", log_file = NULL) {
  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .cli_debug("Snapshotting renv", output_level = output_level, log_file = log_file)
  .build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .cli_debug("Updating ignore files", output_level = output_level, log_file = log_file)
  .build_ignore(output_run, archive_local)

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .cli_debug("Updating documentation output directory", output_level = output_level, log_file = log_file)
  .build_doc_output_dir_update(FALSE)

  # ensure that pre-build, we are on dev version
  .cli_debug("Ensuring development version", output_level = output_level, log_file = log_file)
  .build_ensure_dev_version()
}

.build_pre_setup_for_output_run <- function(version_run_on_list,
                                            output_run,
                                            clear_output,
                                            output_level = "std",
                                            log_file = NULL) {
  # set the version pre-run
  .cli_debug("Setting build version", output_level = output_level, log_file = log_file)
  .build_version_set_pre(version_run_on_list)

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .cli_debug("Configuring documentation directory", output_level = output_level, log_file = log_file)
  .build_doc_output_dir_update(FALSE)


  # empty output directories
  # (docs, output and data)
  .cli_debug("Clearing output directories (pre-build)", output_level = output_level, log_file = log_file)
  .build_clear_pre(output_run, clear_output)
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
        cli::cli_inform("Please enter a one-line description of the change:")
        msg <- readline(prompt = ">> ")
        while (.is_len_0(msg)) {
          cli::cli_alert_warning("Message cannot be empty.")
          cli::cli_inform("Please enter a one-line description of the change:")
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
.build_ignore <- function(output_run, archive_local) {
  old_profile <- .profile_get_raw()
  Sys.unsetenv("PROJR_PROFILE")
  projr_path_get_dir("docs")
  if (!identical(old_profile, "default")) {
    Sys.setenv("PROJR_PROFILE" = old_profile)
  }
  .ignore_auto(output_run && archive_local)
  invisible(TRUE)
}

.build_version_set_pre <- function(version_run_on_list) {
  projr_version_set(version_run_on_list$desc[["run"]])
  invisible(TRUE)
}

.build_doc_output_dir_update <- function(output_run) {
  # sets docs directory correctly whenever called
  invisible(.dir_get_label("docs", safe = !output_run))
}

.build_ensure_dev_version <- function() {
  .version_get(dev_force = TRUE) |>
    projr_version_set()
  invisible(TRUE)
}
