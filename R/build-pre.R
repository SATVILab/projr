# ==========================
# Pre-build
# ==========================

.build_pre_check <- function(output_run) {
  # Check required packages are available BEFORE starting build
  .cli_debug("Checking required packages")
  .build_check_packages_available(output_run)

  # check build restrictions (branch)
  .cli_debug("Checking build restrictions")
  .build_check_restrictions(output_run)

  # set and check authorisation is available
  .cli_debug("Checking environment variables")
  .build_env_check(output_run)

  # check that we have Git if needed
  .cli_debug("Checking Git repository")
  .build_git_check(output_run)

  # check that we have GitHub remote if needed
  .cli_debug("Checking GitHub remote")
  .build_github_check(output_run)

  # check we are not missing upstream commits
  .cli_debug("Checking upstream commits")
  .build_exit_if_behind_upstream(output_run)
}

.build_pre_remotes_prepare <- function(bump_component,
                                       archive_github,
                                       archive_local,
                                       always_archive) {
  output_run <- .build_get_output_run(bump_component)
  if (!output_run) {
    return(invisible(FALSE))
  }

  # For now, only GitHub releases
  .dest_prepare_github_releases(
    bump_component = bump_component,
    archive_github = archive_github,
    archive_local = archive_local,
    always_archive = always_archive,
    strict = TRUE
  )
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
  if (.is_interactive_and_not_test()) {
    cli::cli_alert_warning("Git repository not found.")
    cli::cli_inform("It is not required, but recommended, and projr will handle setup and (by default) commits for you.")
    cli::cli_inform("If you choose not to create one now, then Git handling by projr will be disabled.")
    choice <- utils::menu(
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
  if (.is_interactive_and_not_test()) {
    cli::cli_alert_warning("GitHub remote not found.")
    cli::cli_inform("It is not required, but recommended, and projr will help handle setup for you.") # nolint
    choice <- utils::menu(
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
  user <- tryCatch(
    {
      gh::gh_whoami()$login
    },
    error = function(e) {
      NULL
    }
  )
  if (!.is_string(user)) {
    stop("GitHub user not found.")
  }
  if (!.is_interactive_and_not_test()) {
    return(c("user" = user))
  }
  choice <- utils::menu(
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
    username <- user[["user"]]
    if (!.is_string(username)) {
      stop("GitHub user not found")
    }
    .init_github_actual_user(FALSE, username)
  }
  if ("org" %in% names(user)) {
    .init_github_actual_org(FALSE, user[["org"]])
  }
}

.build_pre_document <- function(output_run, archive_local) {
  # get version for DESCRIPTION and bookdown from run onwards
  # snapshot if need be
  .cli_debug("Snapshotting renv")
  .build_renv_snapshot(output_run)

  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .cli_debug("Updating ignore files")
  .build_ignore(output_run, archive_local)

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .cli_debug("Updating documentation output directory")
  .build_doc_output_dir_update(FALSE)

  # ensure that pre-build, we are on dev version
  .cli_debug("Ensuring development version")
  .build_ensure_dev_version()
}

.build_pre_setup_for_output_run <- function(version_run_on_list,
                                            output_run,
                                            clear_output) {
  # set the version pre-run
  .cli_debug("Setting build version")
  .build_version_set_pre(version_run_on_list)

  # ensure that docs directory is the unsafe directory.
  # will copy docs across upon success.
  .cli_debug("Configuring documentation directory")
  .build_doc_output_dir_update(FALSE)


  # empty output directories
  # (docs, output and data)
  .cli_debug("Clearing output directories (pre-build)")
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

.build_is_current_version_dev <- function() {
  # Return FALSE if no version file exists yet (not a dev version)
  if (!file.exists(.path_get("VERSION")) && !file.exists(.path_get("DESCRIPTION"))) {
    return(FALSE)
  }

  version_vec_current <- .version_current_vec_get(dev_force = FALSE)
  version_format <- .version_format_list_get(NULL)[["component"]]
  # If current version vector length matches format length, it has dev component
  length(version_vec_current) == length(version_format)
}

.build_output_get_msg <- function(msg) {
  if (is.null(msg) || .is_len_0(msg)) {
    if (.is_interactive_and_not_test()) {
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
      }
    )
  }
}

# check we are not missing upstream commits
.build_exit_if_behind_upstream <- function(output_run) {
  if (!.build_exit_if_behind_upstream_check(output_run)) {
    return(invisible(FALSE))
  }

  # Check if not_behind restriction is enabled
  not_behind <- .yml_restrictions_get_not_behind(NULL)
  if (!isTRUE(not_behind)) {
    # Check is disabled
    return(invisible(FALSE))
  }

  if (.git_check_behind()) {
    stop(
      "Remote is ahead of local.\n",
      "Merge remote changes before proceeding\n",
      "(e.g by running `git fetch`, then `git merge`).\n",
      "To disable this check, set build.restrictions.not_behind to FALSE in _projr.yml",
      call. = FALSE
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

#' Output Git information for debug
#'
#' @keywords internal
.build_debug_git_info <- function() {
  if (!.git_repo_check_exists()) {
    .cli_debug(
      "Git repository: Not a Git repository"
    )
    return(invisible(NULL))
  }

  # Get branch name
  branch <- .git_branch_get()
  if (!is.null(branch)) {
    .cli_debug(
      "Git branch: {branch}"
    )
  }

  # Get last commit info
  commit_info <- .git_last_commit_get()
  if (!is.null(commit_info)) {
    .cli_debug(
      "Last commit: {commit_info$sha} - {commit_info$message}"
    )
  }

  # Get modified tracked files (after pre-build commit)
  modified_files <- .git_modified_get()
  if (length(modified_files) > 0) {
    .cli_debug(
      "Modified tracked files ({length(modified_files)}): {paste(head(modified_files, 10), collapse = ', ')}{if (length(modified_files) > 10) '...' else ''}"
    )
  } else {
    .cli_debug("Modified tracked files: None")
  }

  # Get untracked files that are not ignored
  untracked_files <- .git_untracked_not_ignored_get()
  if (length(untracked_files) > 0) {
    .cli_debug(
      "Untracked files (not ignored) ({length(untracked_files)}): {paste(head(untracked_files, 10), collapse = ', ')}{if (length(untracked_files) > 10) '...' else ''}"
    )
  } else {
    .cli_debug(
      "Untracked files (not ignored): None"
    )
  }

  invisible(NULL)
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

# Package availability checking
# ==============================

.build_check_packages_available <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }

  # Use the new exported function to get package status
  pkg_status <- projr_build_check_packages()

  if (pkg_status$available) {
    return(invisible(TRUE))
  }

  # Build error message with copy-paste command
  msg <- paste0(
    pkg_status$message,
    "\n\nFor programmatic access to installation commands, use:\n",
    "  pkg_status <- projr::projr_build_check_packages()\n",
    "  pkg_status$install_cmds"
  )

  stop(msg, call. = FALSE)
}

# Build restrictions checking
# ============================

.build_check_restrictions <- function(output_run) {
  if (!output_run) {
    return(invisible(FALSE))
  }

  # Check branch restrictions
  .build_check_branch_restriction()

  invisible(TRUE)
}

.build_check_branch_restriction <- function() {
  # Get branch restriction from YAML
  branch_restriction <- .yml_restrictions_get_branch(NULL)

  # If TRUE, no restrictions
  if (isTRUE(branch_restriction)) {
    return(invisible(TRUE))
  }

  # Get current branch
  current_branch <- .git_branch_get()

  # If not in a Git repo, allow build
  if (is.null(current_branch)) {
    return(invisible(TRUE))
  }

  # If FALSE (logical), restrict on all branches
  if (isFALSE(branch_restriction)) {
    stop(
      "Builds are restricted on all branches.\n",
      "Current branch: ", current_branch, "\n",
      "To allow builds, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  # If empty character vector or empty list, restrict on all branches
  if ((is.character(branch_restriction) && length(branch_restriction) == 0) ||
    (is.list(branch_restriction) && length(branch_restriction) == 0)) {
    stop(
      "Builds are restricted on all branches.\n",
      "Current branch: ", current_branch, "\n",
      "To allow builds, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  # Check if current branch matches any allowed branches
  if ((is.character(branch_restriction) || is.list(branch_restriction)) &&
    length(branch_restriction) > 0 &&
    !current_branch %in% branch_restriction) {
    stop(
      "Builds are restricted to specific branches.\n",
      "Current branch: ", current_branch, "\n",
      "Allowed branches: ", paste(branch_restriction, collapse = ", "), "\n",
      "To allow builds on this branch, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
