# github
# --------------------------
#' @rdname projr_init

#' @export
projr_init_github <- function(
  user = NULL,
  org = NULL,
  public = FALSE,
  create_repo = TRUE,
  git_commit = TRUE
) {
  if (!.git_repo_check_exists()) {
    # offer create repository if it does not exist
    if (create_repo) {
      projr_init_git(git_commit)
    }
  }
  .init_github(TRUE, user, org, public)
}

.git_gh_check_auth <- function(
  use_gh_if_available = TRUE,
  use_gitcreds_if_needed = TRUE
) {
  if (
    nzchar(.auth_get_github_pat(
      use_gh_if_available = use_gh_if_available,
      use_gitcreds_if_needed = use_gitcreds_if_needed
    ))
  ) {
    return(invisible(TRUE))
  }
  warning(
    "GitHub token not found.\n",
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

.init_github <- function(github, user, org, public) {
  if (!github) {
    return(invisible(FALSE))
  }
  if (!.git_repo_check_exists()) {
    .cli_info(
      "Local Git repository does not exist, so skipping creation of GitHub repo."
    ) # nolint
    return(invisible(FALSE))
  }
  if (.git_remote_check_exists()) {
    .cli_info("GitHub remote already set, so skipping creation of GitHub repo.") # nolint
    return(invisible(FALSE))
  }
  .dep_install_only(c("gh", "usethis"))
  .init_github_impl(user, org, public)
}

.init_github_impl <- function(user, org, public) {
  if (is.null(org)) {
    .init_github_impl_user(user, public)
  } else {
    .init_github_impl_org(public, org)
  }
  invisible(TRUE)
}

.init_github_impl_user <- function(user, public) {
  .auth_check_github("creating GitHub repository")
  user <- user %||%
    tryCatch(
      {
        gh::gh_whoami()$login
      },
      error = function(e) {
        NULL
      }
    )
  if (!.is_string(user)) {
    stop(
      "Failed to get GitHub user information. Please check your GitHub authentication."
    ) # nolint line_length_linter.
  }
  # Check if repository already exists
  repo_name <- basename(getwd())
  if (
    .init_github_handle_existing_repo(
      owner = user,
      repo = repo_name
    )
  ) {
    return(invisible(FALSE))
  }

  .cli_info("Creating GitHub remote for user {user}")
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
  invisible(TRUE)
}

#' Check if GitHub repository exists using httr
#'
#' @description
#' Uses the GitHub API to check if a repository exists for a given owner.
#'
#' @param owner Character string. Repository owner (username or organization).
#' @param repo Character string. Repository name.
#' @param api_url Character string. Optional GitHub API URL for enterprise instances.
#' @param token Character string. Optional GitHub token.
#'
#' @return Logical TRUE/FALSE if repository exists/doesn't exist.
#' @keywords internal
.gh_repo_check_exists_httr <- function(
  owner,
  repo,
  api_url = NULL,
  token = NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop(
      "httr is required for .gh_repo_check_exists_httr(); please install it."
    )
  }

  .assert_string(owner, required = TRUE)
  .assert_string(repo, required = TRUE)

  base <- .github_api_base(api_url)
  url <- sprintf("%s/repos/%s/%s", base, owner, repo)

  tok <- token %||% .auth_get_github_pat_find(api_url = api_url)
  headers <- c("Accept" = "application/vnd.github+json")
  if (.is_string(tok)) {
    headers["Authorization"] <- paste("token", tok)
  }

  resp <- httr::GET(url, httr::add_headers(.headers = headers))
  status <- httr::status_code(resp)

  if (status == 200L) {
    TRUE
  } else if (status == 404L) {
    FALSE
  } else if (status == 401L || status == 403L) {
    stop(
      "GitHub API authentication error (status ",
      status,
      ") ",
      "when checking repository existence for '",
      owner,
      "/",
      repo,
      "'. ",
      "Please check your GITHUB_PAT is set correctly.",
      call. = FALSE
    )
  } else {
    stop(
      "Unexpected HTTP status from GitHub API: ",
      status,
      " (url: ",
      url,
      ")",
      call. = FALSE
    )
  }
}

#' Handle existing GitHub repository
#'
#' @description
#' Checks if a GitHub repository exists and prompts user to set it as remote
#' if it does. In non-interactive contexts, throws an error.
#'
#' @param owner Character string. Repository owner (username or organization).
#' @param repo Character string. Repository name.
#'
#' @return Invisible TRUE if repository doesn't exist or user chose to set remote.
#'   Otherwise stops with error.
#' @keywords internal
.init_github_handle_existing_repo <- function(owner, repo) {
  .assert_string(owner, required = TRUE)
  .assert_string(repo, required = TRUE)

  if (!.gh_repo_check_exists_httr(owner = owner, repo = repo)) {
    return(invisible(FALSE))
  }

  repo_url <- sprintf("https://github.com/%s/%s", owner, repo)

  if (.is_interactive_and_not_in_test()) {
    .cli_info("GitHub repository already exists: {repo_url}")
    response <- .menu_get(
      paste0(
        "The repository '",
        owner,
        "/",
        repo,
        "' already exists on GitHub.\n",
        "Do you want to set it as the Git remote for this project?"
      ),
      c("Yes", "No")
    )

    if (response == 1L) {
      # Set the remote
      .dep_install_only("usethis")
      tryCatch(
        usethis::use_git_remote("origin", repo_url),
        error = function(e) {
          stop(
            "Failed to set GitHub remote: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      .cli_success("Set {repo_url} as Git remote 'origin'")
      return(invisible(TRUE))
    } else {
      stop(
        "GitHub repository '",
        owner,
        "/",
        repo,
        "' already exists. ",
        "Please choose a different repository name or set it as remote manually.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "GitHub repository '",
      owner,
      "/",
      repo,
      "' already exists at ",
      repo_url,
      ". ",
      "Cannot proceed in non-interactive mode. ",
      "Either use a different repository name or set it as remote manually with:\n",
      "  usethis::use_git_remote('origin', '",
      repo_url,
      "')",
      call. = FALSE
    )
  }
}

.init_github_actual_user_error <- function(public) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(private = {!public})")
}

.init_github_impl_org <- function(public, org) {
  .cli_info("Creating GitHub remote for organisation {org}")
  if ("username" %in% names(formals(usethis::use_github))) {
    .init_github_impl_org_old(public, org)
  } else {
    .init_github_impl_org_new(public, org)
  }
}

.init_github_impl_org_new <- function(public, org) {
  # Check if repository already exists
  repo_name <- basename(getwd())
  if (
    .init_github_handle_existing_repo(
      owner = org,
      repo = repo_name
    )
  ) {
    return(invisible(FALSE))
  }

  result <- tryCatch(
    usethis::use_github(
      organisation = org,
      private = !public
    ),
    error = function(e) {
      .init_github_impl_org_new_error(public, org)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for organisation created successfully!")
  }
  invisible(TRUE)
}

.init_github_impl_org_old <- function(public, org) {
  # Check if repository already exists
  repo_name <- basename(getwd())
  if (
    .init_github_handle_existing_repo(
      owner = org,
      repo = repo_name
    )
  ) {
    return(invisible(FALSE))
  }
  result <- tryCatch(
    usethis::use_github(
      username = org,
      private = !public
    ),
    error = function(e) {
      .init_github_impl_org_old_error(public, org)
      NULL
    }
  )
  if (!is.null(result)) {
    .cli_info("GitHub remote for organisation created successfully!")
  }
  invisible(TRUE)
}

.init_github_impl_org_old_error <- function(public, org) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(username = '{org}', private = {!public})")
}
.init_github_impl_org_new_error <- function(public, org) {
  .cli_info("Failed to create GitHub remote")
  .cli_info("Can try again later with:")
  .cli_info("usethis::use_github(organisation = '{org}', private = {!public})")
}
