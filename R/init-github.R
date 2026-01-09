# github
# --------------------------
#' @rdname projr_init

#' @export
projr_init_github <- function(username = NULL,
                              public = FALSE,
                              create_repo = TRUE,
                              git_commit = TRUE) {
   if (!.git_repo_check_exists()) {
    # offer create repository if it does not exist
    if (create_repo) {
      projr_init_git(git_commit)
    }
   }
  .init_std_github(TRUE, public, username)
}

.init_github <- function(username,
                         public) {
  # Check if git repo exists
  if (!.git_repo_check_exists()) {
    .yml_unset_github_dest()
    return(invisible(FALSE))
  }

  # Check if username is NULL or a placeholder value
  if (is.null(username) || identical(username, "GITHUB_USER_NAME")) {
    .yml_unset_github_dest()
    return(invisible(FALSE))
  }

  # Check if remote already exists
  if (.git_remote_check_exists()) {
    return(invisible(FALSE))
  }

  .init_github_impl(username, public)
}

.init_github_impl <- function(username, public) {
  .dep_install_only("usethis")
  .dep_install_only("gh")
  .auth_check_github("creating GitHub repository")
  current_user <- tryCatch(
    {
      gh::gh_whoami()$login
    },
    error = function(e) {
      NULL
    }
  )
  if (!.is_string(current_user)) {
    stop("Failed to get GitHub user information. Please check your GitHub authentication.")
  }
  if (is.null(username) || identical(username, current_user)) {
    .init_github_actual_user(public, current_user)
  } else {
    .init_github_actual_org(public, username)
  }
  invisible(TRUE)
}

.init_github_actual_user <- function(public, username) {
  .cli_info("Creating GitHub remote for user {username}")
  result <- tryCatch(
    usethis::use_github(private = !public),
    error = function(e) {
      .init_github_actual_user_error(public)
      NULL
    }
  )
  if (!is.null(result)) {
    # Do something if the call was successful.
    .cli_info("GitHub remote created successfully!")
  }
  invisible(result)
}

.init_github_actual_user_error <- function(public) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(private = {!public})")
}

.init_github_actual_org <- function(public, username) {
  .cli_info("Creating GitHub remote for organisation {username}")
  if ("username" %in% names(formals(usethis::use_github))) {
    .init_github_actual_org_old(public, username)
  } else {
    .init_github_actual_org_new(public, username)
  }
}

.init_github_actual_org_new <- function(public, username) {
  result <- tryCatch(
    usethis::use_github(
      organisation = username,
      private = !public
    ),
    error = function(e) {
      .init_github_actual_org_new_error(public, username)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for organisation created successfully!")
  }
  invisible(result)
}

.init_github_actual_org_old <- function(public, username) {
  result <- tryCatch(
    usethis::use_github(
      username = username,
      private = !public
    ),
    error = function(e) {
      .init_github_actual_org_old_error(public, username)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for user created successfully!")
  }
  invisible(result)
}

.init_github_actual_org_old_error <- function(public, username) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(username = '{username}', private = {!public})")
}
.init_github_actual_org_new_error <- function(public, username) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(organisation = '{username}', private = {!public})")
}

.git_gh_check_auth <- function(use_gh_if_available = TRUE,
                               use_gitcreds_if_needed = TRUE) {
  if (nzchar(.auth_get_github_pat(
    use_gh_if_available = use_gh_if_available,
    use_gitcreds_if_needed = use_gitcreds_if_needed
  ))) {
    return(invisible(TRUE))
  }
  warning(
    "GITHUB_PAT environment variable not found.\n",
    "\n",
    "To allow creating a GitHub repository, please set it.\n",
    "\n",
    "To easily set it in less than two minutes, do the following:\n",
    "1. If you do not have a GitHub account, create one here: https://github.com\n", # nolint: line_length_linter.
    "2. In R, run usethis::create_github_token()\n",
    "3. In R, run gitcreds::gitcreds_set()\n",
    "4. Paste the token from step 1 into the R command line (terminal), and press enter\n", # nolint: line_length_linter.
    "For more details, see https://happygitwithr.com/https-pat#tldr\n",
    "\n",
    "After doing the above:\n",
    "1. In R, rerun projr::projr_init()\n",
    "It will skip what's been done already and try set up GitHub again.\n",
    call. = FALSE
  )
  invisible(FALSE)
}

# ========================================
# GitHub
# ========================================

.init_std_github <- function(github, public, org) {
  if (!github) {
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    .cli_info("Local Git repository does not exist, so skipping creation of GitHub repo.") # nolint
    return(invisible(FALSE))
  }
  if (.git_remote_check_exists()) {
    .cli_info("GitHub remote already set, so skipping creation of GitHub repo.") # nolint
    return(invisible(FALSE))
  }
  .init_std_github_impl(public, org)
}

.init_std_github_impl <- function(public, org) {
  .dep_install_only(c("gh", "usethis"))
  if (is.null(org)) {
    .init_github_impl(NULL, public)
  } else {
    .init_github_actual_org(public, org)
  }
  invisible(TRUE)
}
