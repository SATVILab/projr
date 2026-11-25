# ==========================
# Pre-build
# ==========================

.build_pre_check <- function(output_run, engines = NULL) {
  if (
    any(
      names(engines) %in%
        c("quarto_project", "bookdown", "quarto_document", "rmd")
    )
  ) {
    .yml_dir_get_path("docs", NULL)
  }
  missing <- .build_renv_missing_renderers(engines, output_run)
  if (length(missing)) {
    stop(.build_renv_render_status(missing)$message, call. = FALSE)
  }
  if (!output_run) {
    return(invisible(FALSE))
  }

  .cli_debug("Checking production build prerequisites")
  .build_check_packages_available(output_run, engines)
  .build_check_restrictions(output_run)
  .build_renv_snapshot(output_run)
  .build_env_check(output_run)
  .build_git_check(output_run)
  .build_github_check(output_run)
  .build_exit_if_behind_upstream(output_run)
}

.build_pre_remotes_prepare <- function(
  bump_component,
  archive_github,
  archive_local,
  always_archive
) {
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
  cli::cli_alert_info(
    "Git is explicitly required but no Git repository was found."
  )
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
    cli::cli_inform(
      "It is not required, but recommended, and projr will handle setup and (by default) commits for you."
    )
    cli::cli_inform(
      "If you choose not to create one now, then Git handling by projr will be disabled."
    )
    choice <- utils::menu(
      c("Yes", "No"),
      title = "Do you want to create a new Git repository?"
    )
    choice == 1
  } else {
    cli::cli_alert_warning("Git repository not found.")
    cli::cli_inform(
      "Git repository will be created automatically, as in non-interactive mode."
    )
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
    .cli_info(
      "Git pushes are enabled. Configure a Git remote to push this build."
    )
    return(invisible(TRUE))
  }
  # check if it's an option
  if (.is_interactive_and_not_test()) {
    .cli_info("No Git remote is configured for this project.")
    choice <- utils::menu(
      c("Yes", "No"),
      title = "Do you want to keep automatic Git pushes enabled?"
    )
    if (choice == 2) {
      cli::cli_alert_info("Disabling automated pushes to GitHub by projr.")
      cli::cli_inform(
        "You can re-enable it by running `projr_yml_git_set(push = TRUE)`."
      ) # nolint
      projr_yml_git_set(push = FALSE)
      return(FALSE)
    }
    TRUE
  } else {
    .cli_info("No Git remote is configured for this project.")
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
    title = paste0(
      "Do you want to set up the GitHub remote under user ",
      user,
      ", or under an organisation?"
    ) # nolint
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
    .init_github_impl_user(username, public = FALSE)
  }
  if ("org" %in% names(user)) {
    .init_github_impl_org(public = FALSE, org = user[["org"]])
  }
}

.build_pre_document <- function(output_run, archive_local) {
  # get version for DESCRIPTION and bookdown from run onwards
  # make sure everything is ignored that should be ignored
  # (including docs directory)
  .cli_debug("Updating ignore files")
  .build_ignore(output_run, archive_local)

  if (output_run) {
    .cli_debug("Ensuring development version")
    .build_ensure_dev_version()
  }
}

.build_pre_setup_for_output_run <- function(
  version_run_on_list,
  output_run,
  clear_output
) {
  if (output_run) {
    .cli_step("Setting build version to {version_run_on_list$desc[['run']]}")
    .build_version_set_pre(version_run_on_list)
  }

  # empty output directories
  # (docs, output and data)
  .build_include_check_config()
  .build_clear_pre(output_run, clear_output)
  # Existing analysis scripts can write directly to their declared output paths.
  if (output_run) {
    .dir_create_by_label(.yml_dir_get_label_output(NULL), safe = FALSE)
  }
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
    switch(remote_vec[[i]], "github" = {
      if (!.git_gh_check_auth()) stop()
    })
  }
}

# check we are not missing upstream commits
.build_exit_if_behind_upstream <- function(output_run) {
  if (!.build_exit_if_behind_upstream_check(output_run)) {
    return(invisible(FALSE))
  }

  not_behind <- .yml_restrictions_get_not_behind_git(NULL)
  if (!isTRUE(not_behind)) {
    # Check is disabled
    return(invisible(FALSE))
  }

  .cli_debug("Checking for upstream commits")
  if (.git_check_behind()) {
    stop(
      "Remote is ahead of local.\n",
      "Merge remote changes before proceeding\n",
      "(e.g by running `git fetch`, then `git merge`).\n",
      "To disable this check, set build.restrictions.not_behind_git to FALSE in _projr.yml",
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
  .ignore_auto(output_run && archive_local)
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

.build_ensure_dev_version <- function() {
  .version_get(dev_force = TRUE) |>
    projr_version_set()
  invisible(TRUE)
}

# Package availability checking
# ==============================

.build_check_packages_available <- function(output_run, engines = NULL) {
  if (!output_run) {
    return(invisible(FALSE))
  }

  pkg_status <- if (is.null(engines)) {
    projr_build_check_packages()
  } else {
    .build_check_packages(NULL, names(engines))
  }

  if (pkg_status$available) {
    return(invisible(TRUE))
  }

  # Auto-install if PROJR_AUTO_INSTALL=TRUE, or prompt user in interactive mode
  if (
    .is_env_var_true("PROJR_AUTO_INSTALL") || .is_interactive_and_not_test()
  ) {
    .dep_install_only(pkg_status$missing)
    return(invisible(TRUE))
  }

  # Non-interactive without PROJR_AUTO_INSTALL: error with build-specific message
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
      "Current branch: ",
      current_branch,
      "\n",
      "To allow builds, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  # If empty character vector or empty list, restrict on all branches
  if (
    (is.character(branch_restriction) && length(branch_restriction) == 0) ||
      (is.list(branch_restriction) && length(branch_restriction) == 0)
  ) {
    stop(
      "Builds are restricted on all branches.\n",
      "Current branch: ",
      current_branch,
      "\n",
      "To allow builds, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  # Check if current branch matches any allowed branches
  if (
    (is.character(branch_restriction) || is.list(branch_restriction)) &&
      length(branch_restriction) > 0 &&
      !current_branch %in% branch_restriction
  ) {
    stop(
      "Builds are restricted to specific branches.\n",
      "Current branch: ",
      current_branch,
      "\n",
      "Allowed branches: ",
      paste(branch_restriction, collapse = ", "),
      "\n",
      "To allow builds on this branch, update build.restrictions.branch in _projr.yml",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Register a one-off `archive_github`/`archive_local` archive under `remotes`,
# so later source lookups know where this build's files were sent.
.build_pre_register_archives <- function(
  output_run,
  archive_github,
  archive_local
) {
  if (!output_run) {
    return(invisible(FALSE))
  }
  config <- .yml_get("default")
  added <- character()
  for (type in .opt_remote_get_type()) {
    archive_type <- .dest_send_get_archive_type(
      type,
      archive_github,
      archive_local
    )
    if (
      isFALSE(archive_type) ||
        "archive" %in% names(config[["remotes"]][[type]])
    ) {
      next
    }
    config[["remotes"]][[type]][["archive"]] <- if (type == "local") {
      list(path = "_archive")
    } else {
      structure(list(), names = character())
    }
    added <- c(added, paste0(type, ":archive"))
  }
  if (!length(added)) {
    return(invisible(FALSE))
  }
  .yml_set(config, "default")
  .cli_info("Registered one-off archive{?s} under remotes: {added}")
  invisible(TRUE)
}
