.remote_ls <- function() {
  remote_vec <- c(
    .remote_ls_source(),
    .remote_ls_dest(),
    "github"[.git_push_check()]
  ) |>
    unique()
  remote_vec[nzchar(remote_vec)]
}

.remote_ls_source <- function() {
  yml_projr_dir <- .yml_dir_get(NULL)
  lapply(yml_projr_dir, function(x) {
    remote_vec <- c("github", "osf")
    remote_vec[remote_vec %in% names(x)]
  }) |>
    unlist() |>
    unique()
}

.remote_ls_dest <- function() {
  yml_projr_build <- .yml_build_get(NULL)
  remote_vec <- c("github", "osf")
  remote_vec[remote_vec %in% names(yml_projr_build)]
}

.git_push_check <- function() {
  setting_git <-
    .yml_build_get(NULL)[["git"]]
  switch(class(setting_git),
    "NULL" = TRUE,
    "logical" = setting_git,
    list = {
      setting_push <- setting_git[["push"]]
      if (is.null(setting_push)) TRUE else setting_push
    },
    stop(paste0("git setting '", class(setting_git), "' not recognized"))
  )
}

.gh_repo_get <- function() {
  if (.git_repo_is_worktree()) {
    git_file <- readLines(.path_get(".git"), warn = FALSE)
    git_file <- gsub("^gitdir: ", "", git_file)
    if (!file.exists(git_file)) {
      stop("Cannot find gitdir: ", git_file)
    }
    .gh_guess_repo(git_file)
  } else {
    .gh_guess_repo()
  }
}

.gh_guess_repo <- function(path = ".") {
  # determine git invocation mode: top-level repo path (-C) or bare git dir (--git-dir)
  git_args <- if (file.exists(file.path(path, "config"))) {
    c("--git-dir", path)
  } else {
    c("-C", path)
  }

  # prefer 'origin', fallback to first remote returned by `git remote`
  remote_url <- tryCatch({
    system2("git", args = c(git_args, "remote", "get-url", "origin"), stdout = TRUE, stderr = TRUE)
  }, error = function(e) {
    character(0)
  })

  if (length(remote_url) == 0 || !nzchar(remote_url)) {
    # fallback: pick the first remote name, then get-url
    remotes <- tryCatch(system2("git", args = c(git_args, "remote"), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
    if (length(remotes) == 0) {
      stop("Could not detect any git remotes. Please ensure repository has a git remote.")
    }
    remote_url <- tryCatch(system2("git", args = c(git_args, "remote", "get-url", remotes[1]), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
    if (length(remote_url) == 0 || !nzchar(remote_url)) {
      stop("Could not get URL for git remote 'origin' or first remote in this repository.")
    }
  }

  remote_url <- remote_url[1]

  # Parse the owner/repo from a variety of URL formats:
  # - git@github.com:owner/repo.git
  # - https://github.com/owner/repo.git
  # - ssh://git@github.com/owner/repo.git
  # - https://github.com/owner/repo
  owner_repo <- NULL

  # SSH scp-like: git@github.com:owner/repo.git
  if (grepl("^[^@]+@[^:]+:[^/]+/.+$", remote_url)) {
    owner_repo <- sub("^.+?:", "", remote_url)
  } else if (grepl("^ssh://", remote_url)) {
    owner_repo <- sub("^ssh://[^/]+/", "", remote_url)
  } else if (grepl("^(https?)://", remote_url)) {
    owner_repo <- sub("^https?://[^/]+/", "", remote_url)
  } else if (grepl("^[^/]+/[^/]+$", remote_url)) {
    # already owner/repo style
    owner_repo <- remote_url
  }

  if (is.null(owner_repo) || !nzchar(owner_repo)) {
    stop("Failed to parse owner/repo from remote URL: ", remote_url)
  }

  # remove possible trailing .git and trailing slashes
  owner_repo <- gsub("\\.git$", "", owner_repo)
  owner_repo <- gsub("/+$", "", owner_repo)

  # ensure owner/repo structure
  parts <- strsplit(owner_repo, "/")[[1]]
  if (length(parts) < 2 || !nzchar(parts[1]) || !nzchar(parts[2])) {
    stop("Failed to get GitHub repository owner and name from git remote URL: ", remote_url)
  }

  paste0(parts[1], "/", parts[2])
}