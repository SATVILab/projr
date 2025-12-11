# Skip wrapper for tests that modify GitHub repositories
# Ensures tests only run when:
# 1. A token is detectable via .auth_get_github_pat_find()
# 2. The token is NOT the same as GITHUB_TOKEN (prevents using CI tokens)
# 3. gh::gh_whoami() can successfully retrieve the username
.test_skip_if_cannot_modify_github <- function() {
  # Check if token is detectable
  token <- .auth_get_github_pat_find()
  if (!nzchar(token)) {
    testthat::skip("No GitHub token found")
  }

  # Check if token is same as GITHUB_TOKEN
  github_token <- Sys.getenv("GITHUB_TOKEN", "")
  if (nzchar(github_token) && identical(token, github_token)) {
    testthat::skip("Cannot modify GitHub repos with GITHUB_TOKEN (use GITHUB_PAT instead)")
  }

  # Verify that gh::gh_whoami() works with the available credentials
  # This prevents tests from running when auth exists but gh_whoami() fails,
  # which would cause malformed GitHub URLs
  if (requireNamespace("gh", quietly = TRUE)) {
    user <- tryCatch(
      {
        gh::gh_whoami()[["login"]]
      },
      error = function(e) {
        NULL
      }
    )
    if (!.is_string(user)) {
      testthat::skip("gh::gh_whoami() failed to retrieve GitHub username")
    }
  } else {
    testthat::skip("gh package not available")
  }

  invisible(TRUE)
}

.test_github_repo_create <- function(user = NULL,
                                     # token = NULL,
                                     repo = NULL,
                                     env = NULL,
                                     debug = FALSE) {
  if (debug) {
    print("Running repo creation function")
    print("getwd()")
    print(getwd())
    print(".git repo exists")
    print(".git" %in% .dir_ls(getwd()))
  }
  .assert_string(user)
  .assert_string(repo)
  .assert_class(env, "environment")
  if (debug) {
    print("beginning config again")
  }
  .test_setup_project_git_config(TRUE)
  if (debug) {
    print("ending config again")
  }
  # set up
  # ----------
  if (debug) {
    print("beginning install")
  }
  # Packages should be installed via Suggests dependencies
  # If not available, .dep_install_only will throw informative error
  if (is.null(env)) {
    env <- rlang::caller_env()
  }
  if (debug) {
    print("ending install")

    print("getting upload stuff")
  }
  .dep_install_only("gh")
  .dep_install_only("httr")
  if (is.null(user)) {
    user <- tryCatch(
      {
        gh::gh_whoami()[["login"]]
      },
      error = function(e) {
        NULL
      }
    )
  }
  if (!.is_string(user)) stop("No GitHub user found")

  # credentials::set_github_pat()
  token <- .auth_get_github_pat_find()
  if (!nzchar(token)) stop("No GitHub token found")
  if (debug) {
    print("ending upload stuff")
    print("user")
    print(user)
    print("nchar(token)")
    print(nchar(token))

    # check that it exists or not

    print("Checking if repo exists")
  }

  exists_ind <- .test_github_repo_check_exists(
    user = user,
    token = token,
    repo = repo
  )

  if (debug) {
    print("exists_ind")
    print(exists_ind)
  }

  if (exists_ind) {
    return(paste0(user, "/", repo))
  }

  if (debug) {
    print("setting up fn body")
  }


  # Define the URL of the GitHub API
  url <- "https://api.github.com/user/repos"

  # Define the body of the POST request
  body <- list(
    name = repo,
    description = "projr test repository",
    private = FALSE,
    auto_init = TRUE
  )

  if (debug) {
    print("got function body")
    print("body")
    print(body)
  }

  # create
  # ----------

  # Make the POST request to the GitHub API

  if (debug) {
    print("running POST request")
  }

  response <- httr::POST(
    url,
    config = httr::add_headers(Authorization = paste("token", token)),
    body = body,
    encode = "json"
  )

  if (debug) {
    print("Done running post request")
  }

  # check
  # ----------

  # Check the status of the response
  if (debug) {
    print("checking response status")
  }
  if (httr::http_status(response)$category == "Success") {
    if (debug) {
      print("response successfull")
    }
    file.create(file.path(.test_git_remote_dir_get_tmp(), repo))
    # defer deletion
    withr::defer(
      {
        try(
          {
            .test_remote_host_rm(
              type = "github",
              host = c("repo" = basename(repo))
            )
          },
          silent = TRUE
        )
      },
      envir = env
    )

    # Package should be installed via Suggests
    # If not available, will get clear error
    .dep_install_only("gert")

    if (debug) {
      print("cloning repo")
    }
    gert::git_clone(paste0("https://www.github.com/", user, "/", repo))
    if (debug) {
      print("done cloning repo")
      print("setting up config")
    }
    with_dir(repo, .test_setup_project_git_config(FALSE))
    if (debug) {
      print("done setting up config")
      print(".dir_ls(getwd, recursive = FALSE)")
      print(.dir_ls(getwd(), recursive = FALSE))
      print(".dir_ls(file.path(getwd(), repo), recursive = FALSE)")
      print(.dir_ls(file.path(getwd(), repo), recursive = FALSE))
    }
    return(paste0(user, "/", repo))
  } else {
    stop("response failure")
  }
}

.test_github_repo_check_exists <- function(user = NULL,
                                           token = NULL,
                                           repo = NULL) {
  # set up
  # ----------
  # Packages should be installed via Suggests
  .dep_install_only("gh")
  .dep_install_only("httr")

  # gh prefers github_pat over github_token.
  if (!gh::gh_token_exists()) stop("No GitHub token found")
  if (is.null(user)) {
    user <- tryCatch(
      {
        gh::gh_whoami()[["login"]]
      },
      error = function(e) {
        NULL
      }
    )
  }
  if (!.is_string(user)) stop("No GitHub user found")

  repo <- repo %||% "test.alt"

  # Define the URL of the GitHub API
  url <- paste0("https://api.github.com/repos/", user, "/", repo)

  # Make the GET request to the GitHub API
  response <- httr::GET(
    url,
    config =
      httr::add_headers(
        Authorization = paste("token", token)
      )
  )

  # Check the status of the response
  httr::http_status(response)$category == "Success"
}

.test_github_repo_remote_add <- function(user = NULL,
                                         token = NULL,
                                         repo = NULL) {
  # Packages should be installed via Suggests
  .dep_install_only("gh")
  .dep_install_only("httr")

  # defaults
  if (is.null(user)) {
    user <- tryCatch(
      {
        gh::gh_whoami()[["login"]]
      },
      error = function(e) {
        NULL
      }
    )
  }
  if (!.is_string(user)) stop("No GitHub user found")
  token <- .auth_get_github_pat_find()
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- repo %||% "test.alt"

  # Define the URL of the remote repository
  url <- paste0("https://github.com/", user, "/", repo, ".git")

  # Call the 'git' command to add the remote repository
  system2(
    "git",
    args = c("remote", "add", "origin", url), timeout = 20
  )
}

.test_git_remote_get <- function() {
  system2("git", args = "remote", stdout = TRUE, timeout = 20)
}

.test_git_branch_get <- function() {
  system2(
    "git",
    args = c("branch", "--show-current"), stdout = TRUE,
    timeout = 20
  )
}

.test_git_set_upstream_and_force_push <- function(remote = NULL,
                                                  branch = NULL) {
  remote <- remote %||% .test_git_remote_get()
  remote <- remote[1]
  branch <- branch %||% .test_git_branch_get()
  invisible(
    system2(
      "git",
      args = c("push", "--force", "-u", remote, branch),
      stdout = NULL, stderr = NULL, timeout = 20
    )
  )
}

# MANUAL CLEANUP FUNCTION - USE WITH CAUTION
# This function is for manual cleanup of ALL ProjrGitHubTest repos
# for the authenticated user. Unlike the automatic cleanup in setup.R
# which only deletes repos from the current test run, this function
# finds and deletes ALL repos starting with "ProjrGitHubTest".
# Use this when you need to clean up orphaned test repos from failed
# test runs or other manual cleanup scenarios.
.remote_host_rm_all_github <- function(user = NULL) {
  # set up
  # ----------
  # Packages should be installed via Suggests
  .dep_install_only("gh")
  .dep_install_only("httr")

  # Check authentication before any GitHub API calls
  .auth_check_github("listing and deleting GitHub repositories")

  # defaults
  if (is.null(user)) {
    user <- tryCatch(
      {
        gh::gh_whoami()[["login"]]
      },
      error = function(e) {
        NULL
      }
    )
  }
  if (!.is_string(user)) stop("No GitHub user found")

  # Initialize an empty list to store the repositories
  repo_list <- list()

  # Initialize the page number
  page <- 1

  # Get the repositories page by page
  while (TRUE) {
    # Get the current page of repositories
    .dep_install_only("gh")
    repos <- gh::gh(
      "GET /users/{username}/repos?page={page}",
      username = user, page = page
    )

    # If the page is empty, break the loop
    if (length(repos) == 0) {
      break
    }

    # Add the repositories to the list
    repo_list <- c(repo_list, repos)

    # Go to the next page
    page <- page + 1
  }

  # Get the names of repositories
  repo_vec <- vapply(repo_list, function(x) x$name, character(1))

  # choose which to delete - only those starting with ProjrGitHubTest
  repo_vec_ind_del <- grepl("^ProjrGitHubTest", repo_vec)
  name_vec <- repo_vec[repo_vec_ind_del]
  # make sure we cannot actually delete everything
  rm(repo_vec)

  if (length(name_vec) == 0L) {
    message("No GitHub repositories to delete.")
    return(invisible(FALSE))
  }
  cat(name_vec, sep = "\n")
  opt_vec <- c("Yes", "No", "Definitely not")[sample(1:3, size = 3)]
  yes_ind <- which(opt_vec == "Yes")
  delete_opt <- utils::menu(
    choices = opt_vec,
    title = "Do you want to delete all the above Github repos?"
  )
  if (delete_opt != yes_ind) {
    return(invisible(FALSE))
  }
  if (length(name_vec) > 5) {
    cat(name_vec, sep = "\n")
    opt_vec <- c("Yes", "No", "Actually - no")[sample(1:3, size = 3)]
    yes_ind <- which(opt_vec == "Yes")
    delete_opt <- utils::menu(
      choices = opt_vec,
      title = "Are you SURE you want to delete all the above Github repos?"
    )
    if (delete_opt != yes_ind) {
      return(invisible(FALSE))
    }
  }
  # Delete all confirmed repos
  for (repo in name_vec) {
    .test_remote_host_rm_github(
      host = list(
        user = user,
        repo = repo
      )
    )
  }
}

.test_git_remote_dir_get_tmp <- function() {
  path_dir <- file.path(tempdir() |> dirname(), "github_repo_to_remove")
  .dir_create(path_dir)
  path_dir
}

.test_remote_host_exists_github <- function(host, token = NULL) {
  .assert_given_full(host)
  if (!requireNamespace("gh", quietly = TRUE)) {
    .dep_install_only("gh")
  }

  if (is.null(token)) {
    token <- .auth_get_github_pat_find()
  }
  if (!.is_string(token)) {
    stop("No GitHub token found")
  }

  user <- try(host[["user"]], silent = TRUE)
  if (inherits(user, "try-error") || is.null(user)) {
    user <- tryCatch(gh::gh_whoami(.token = token)[["login"]], error = function(e) NULL)
  }
  if (!.is_string(user)) stop("No GitHub user found")

  repo <- host[["repo"]]
  if (!.is_string(repo)) stop("No GitHub repo specified")
  repo <- basename(repo)
  if (repo == "projr") stop("Cannot delete the projr repo")

  res <- tryCatch(
    gh::gh("GET /repos/{owner}/{repo}", owner = user, repo = repo, .token = token),
    error = function(e) e
  )
  !inherits(res, "error")
}
