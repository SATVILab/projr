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
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }
  if (is.null(env)) {
    env <- rlang::caller_env()
  }
  if (debug) {
    print("ending install")

    print("getting upload stuff")
  }
  .dep_install_only("gh")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!.is_string(user)) stop("No GitHub user found")

  # credentials::set_github_pat()
  token <- Sys.getenv("GITHUB_PAT")
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
            .remote_host_rm(
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
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  .dep_install_only("gh")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!.is_string(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!nzchar(token)) stop("No GitHub token found")
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
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  .dep_install_only("gh")
  user <- user %||% gh::gh_whoami()[["login"]]
  if (!.is_string(user)) stop("No GitHub user found")
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
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

.remote_host_rm_all_github <- function(user = NULL) {
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    utils::install.packages("gh")
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    utils::install.packages("httr")
  }

  # defaults
  .dep_install_only("gh")
  user <- user %||% gh::gh_whoami()[["login"]]
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
      .remote_host_rm_github(host = c("repo" = repo))
    }
  }
}

.test_git_remote_dir_get_tmp <- function() {
  path_dir <- file.path(tempdir() |> dirname(), "github_repo_to_remove")
  .dir_create(path_dir)
  path_dir
}
