.projr_test_github_repo_create <- function(user = NULL,
                                           # token = NULL,
                                           repo = NULL,
                                           env = NULL) {
  .assert_string(user)
  .assert_string(repo)
  .assert_class(env, "environment")

  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }
  if (is.null(env)) {
    env <- rlang::caller_env()
  }

  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")

  # credentials::set_github_pat()
  token <- Sys.getenv("GITHUB_PAT")
  if (!nzchar(token)) stop("No GitHub token found")

  # check that it exists or not

  exists_ind <- .projr_test_github_repo_check_exists(
    user = user,
    token = token,
    repo = repo
  )

  if (exists_ind) {
    return(paste0(user, "/", repo))
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

  # create
  # ----------

  # Make the POST request to the GitHub API

  response <- httr::POST(
    url,
    config = httr::add_headers(Authorization = paste("token", token)),
    body = body,
    encode = "json"
  )

  # check
  # ----------

  # Check the status of the response
  if (httr::http_status(response)$category == "Success") {
    file.create(file.path(.projr_test_git_remote_dir_get_tmp(), repo))
    # defer deletion
    withr::defer(
      {
        try(
          {
            .projr_remote_host_rm(
              type = "github",
              host = c("repo" = basename(repo))
            )
          },
          silent = TRUE
        )
      },
      envir = env
    )

    if (!requireNamespace("gert", quietly = TRUE)) {
      utils::install.packages("gert")
    }
    gert::git_clone(paste0("https://www.github.com/", user, "/", repo))

    return(paste0(user, "/", repo))
  } else {
    character()
  }
}

.projr_test_github_repo_check_exists <- function(user = NULL,
                                                 token = NULL,
                                                 repo = NULL) {
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- repo %||% "test_projr_alt"

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

.projr_test_github_repo_remote_add <- function(user = NULL,
                                               token = NULL,
                                               repo = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- repo %||% "test_projr_alt"

  # Define the URL of the remote repository
  url <- paste0("https://github.com/", user, "/", repo, ".git")

  # Call the 'git' command to add the remote repository
  system2("git", args = c("remote", "add", "origin", url))
}

.projr_test_git_remote_get <- function() {
  system2("git", args = "remote", stdout = TRUE)
}

.projr_test_git_branch_get <- function() {
  system2(
    "git",
    args = c("branch", "--show-current"), stdout = TRUE
  )
}

.projr_test_git_set_upstream_and_force_push <- function(remote = NULL,
                                                        branch = NULL) {
  remote <- remote %||% .projr_test_git_remote_get()
  remote <- remote[1]
  branch <- branch %||% .projr_test_git_branch_get()
  invisible(
    system2(
      "git",
      args = c("push", "--force", "-u", remote, branch),
      stdout = NULL, stderr = NULL, timeout = 20
    )
  )
}

.projr_remote_host_rm_all_github <- function(user = NULL) {
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")

  # Initialize an empty list to store the repositories
  repo_list <- list()

  # Initialize the page number
  page <- 1

  # Get the repositories page by page
  while (TRUE) {
    # Get the current page of repositories
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

  # choose which to delete
  repo_vec_ind_del <- grepl("^ProjrGitHubTest", repo_vec)
  name_vec <- repo_vec[repo_vec_ind_del]
  # make sure we cannot actually delete everything
  rm(repo_vec)
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
    for (repo in name_vec) {
      .projr_remote_host_rm_github(host = c("repo" = repo))
    }
  }
}

.projr_test_git_remote_dir_get_tmp <- function() {
  path_dir <- file.path(tempdir() |> dirname(), "github_repo_to_remove")
  .dir_create(path_dir)
  path_dir
}
