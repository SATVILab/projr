.projr_test_github_repo_create <- function(user = NULL,
                                           token = NULL,
                                           repo = NULL,
                                           env = NULL) {
  message("Beginning .projr_test_github_repo_create")
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

  # defaults
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  message("p1")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- repo %||% "test_projr_alt"
  repo <- paste0("ProjrGitHubTest", repo, signif(stats::rnorm(1), 4))

  # check that it exists or not
  message("p3")
  exists_ind <- .projr_test_github_repo_check_exists(
    user = user,
    token = token,
    repo = repo
  )
  message("p4")
  if (exists_ind) {
    return(paste0(user, "/", repo))
  }
  message("p5")

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
  message("p6")
  response <- httr::POST(
    url,
    config = httr::add_headers(Authorization = paste("token", token)),
    body = body,
    encode = "json"
  )
  message("p7")

  # check
  # ----------

  # Check the status of the response
  message("p8")
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
    message("p9")
    return(paste0(user, "/", repo))
  } else {
    message("p10")
    message("Ending .projr_test_github_repo_create")
    character()
  }
}

.projr_test_github_repo_check_exists <- function(user = NULL,
                                                 token = NULL,
                                                 repo = NULL) {
  message("Beginning .projr_test_github_repo_check_exists")
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  message("p1")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!nzchar(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
  repo <- repo %||% "test_projr_alt"
  message("p2")

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
  message("p3")

  # Check the status of the response
  message("Ending .projr_test_github_repo_check_exists")
  httr::http_status(response)$category == "Success"
}

.projr_test_github_repo_remote_add <- function(user = NULL,
                                               token = NULL,
                                               repo = NULL) {
  message("Beginning .projr_test_github_repo_remote_add")
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  message("p1")
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }
  message("p2 ")

  # defaults
  user <- user %||% gh::gh_whoami()[["login"]]
  message("p3")
  if (!nzchar(user)) stop("No GitHub user found")
  message("p4")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  message("p5")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  message("p6")
  if (!nzchar(token)) stop("No GitHub token found")
  message("p7")
  repo <- repo %||% "test_projr_alt"
  message("p8")

  # Define the URL of the remote repository
  message("p9")
  url <- paste0("https://github.com/", user, "/", repo, ".git")

  # Call the 'git' command to add the remote repository
  message("p10")
  message("p11")
  message("Ending .projr_test_github_repo_remote_add")
  system2("git", args = c("remote", "add", "origin", url))
}

.projr_test_git_remote_get <- function() {
  message("Beginning .projr_test_git_remote_get")
  system2("git", args = "remote", stdout = TRUE)
}

.projr_test_git_branch_get <- function() {
  message("Beginning .projr_test_git_branch_get")
  system2(
    "git",
    args = c("branch", "--show-current"), stdout = TRUE
  )
}

.projr_test_git_set_upstream_and_force_push <- function(remote = NULL,
                                                        branch = NULL) {
  message("Beginning .projr_test_git_set_upstream_and_force_push")
  remote <- remote %||% .projr_test_git_remote_get()
  remote <- remote[1]
  print(.string_create(remote))
  branch <- branch %||% .projr_test_git_branch_get()
  print(.string_create(branch))
  message("p1")
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
