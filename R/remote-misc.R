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
    remote_vec <- c("github")
    remote_vec[remote_vec %in% names(x)]
  }) |>
    unlist() |>
    unique()
}

.remote_ls_dest <- function() {
  yml_projr_build <- .yml_build_get(NULL)
  remote_vec <- c("github")
  remote_vec[remote_vec %in% names(yml_projr_build)]
}

.git_push_check <- function() {
  setting_git <- .yml_build_get(NULL)[["git"]]
  switch(class(setting_git),
    "NULL" = TRUE,
    "logical" = setting_git,
    "list" = {
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
  switch(.gh_guess_repo_tool(path),
    "gh"   = .gh_guess_repo_gh(path),
    "git"  = .gh_guess_repo_git(path),
    "gert" = .gh_guess_repo_gert(path),
    stop(paste0(.git_system_get(), " not recognised"))
  )
}

.gh_guess_repo_tool <- function(path = ".") {
  if (.gh_guess_repo_use_gh(path)) {
    "gh"
  } else {
    .git_system_get()
  }
}

.gh_guess_repo_use_gh <- function(path = ".") {
  requireNamespace("gh", quietly = TRUE) && !.git_repo_is_worktree()
}

.gh_guess_repo_gh <- function(path = ".") {
  remote_list <- tryCatch(
    gh::gh_tree_remote(path),
    error = function(e) {
      stop("Failed to get GitHub repository information using 'gh' package: ", e$message) # nolint
    }
  )
  paste0(remote_list$username, "/", remote_list$repo)
}

# Shared flow: origin -> first remote; robustly handle git/gert errors
.gh_guess_repo_git <- function(path = ".") {
  remote_url <- .gh_remote_url_from_git(path)
  .gh_owner_repo_from_url(remote_url)
}

.gh_guess_repo_gert <- function(path = ".") {
  remote_url <- .gh_remote_url_from_gert(path)
  .gh_owner_repo_from_url(remote_url)
}

.gh_remote_url_from_git <- function(path = ".") {
  git_args <- if (file.exists(file.path(path, "config"))) c("--git-dir", path) else c("-C", path)

  remote_url <- .gh_git_remote_url(git_args, "origin")
  if (nzchar(remote_url)) return(remote_url)

  remotes <- .gh_git_remote_list(git_args)
  if (length(remotes) == 0) {
    stop("Could not detect any git remotes. Please ensure repository has a git remote.")
  }

  remote_url <- .gh_git_remote_url(git_args, remotes[1])
  if (!nzchar(remote_url)) {
    stop("Could not get URL for git remote '", remotes[1], "'.")
  }
  remote_url
}

.gh_remote_url_from_gert <- function(path = ".") {
  remotes <- .gh_gert_remote_list(path)

  remote_url <- .gh_gert_remote_url(remotes, "origin")
  if (nzchar(remote_url)) return(remote_url)

  if (nrow(remotes) == 0) {
    stop("Could not detect any git remotes. Please ensure repository has a git remote.")
  }

  remote_url <- .gh_gert_remote_url(remotes)
  if (!nzchar(remote_url)) {
    stop("Could not get URL for git remote '", remotes$name[[1]], "'.")
  }
  remote_url
}

.gh_owner_repo_from_url <- function(remote_url) {
  owner_repo <- .gh_parse_owner_repo(remote_url)
  paste0(owner_repo[1], "/", owner_repo[2])
}

.gh_git_remote_url <- function(git_args, remote) {
  output <- .gh_git_exec(c(git_args, "remote", "get-url", remote))
  if (length(output) == 0 || !nzchar(output[1])) "" else output[1]
}

.gh_git_remote_list <- function(git_args) {
  remotes <- .gh_git_exec(c(git_args, "remote"))
  remotes[!.gh_git_output_has_error(remotes)]
}

.gh_git_exec <- function(args) {
  output <- suppressWarnings(tryCatch(
    system2("git", args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  ))
  if (.gh_git_output_has_error(output)) character(0) else output
}

.gh_git_output_has_error <- function(output) {
  any(grepl("(?i)^(fatal:|error:)|not a git repository", output, perl = TRUE))
}

.gh_gert_remote_list <- function(path) {
  try_gert <- function(repo_path) {
    suppressWarnings(
      tryCatch(
        gert::git_remote_list(repo = repo_path),
        error = function(e) NULL
      )
    )
  }

  remotes <- try_gert(path)

  # If path looks like a gitdir and gert couldn't read it, try parent-of-parent
  if ((is.null(remotes) || nrow(remotes) == 0) && file.exists(file.path(path, "config"))) {
    alt <- dirname(dirname(path))
    remotes <- try_gert(alt)
  }

  if (is.null(remotes)) remotes <- data.frame(name = character(), url = character())

  remotes[stats::complete.cases(remotes$url) & nzchar(remotes$url), , drop = FALSE]
}

.gh_gert_remote_url <- function(remotes, remote = NULL) {
  if (nrow(remotes) == 0) return("")
  if (!is.null(remote)) {
    match_idx <- which(remotes$name == remote)
    if (length(match_idx) > 0 && nzchar(remotes$url[[match_idx[1]]])) {
      return(remotes$url[[match_idx[1]]])
    }
  }
  remotes$url[[1]]
}

.gh_parse_owner_repo <- function(remote_url) {
  owner_repo <- NULL
  if (grepl("^[^@]+@[^:]+:[^/]+/.+$", remote_url)) {
    owner_repo <- sub("^.+?:", "", remote_url)
  } else if (grepl("^ssh://", remote_url)) {
    owner_repo <- sub("^ssh://[^/]+/", "", remote_url)
  } else if (grepl("^(https?)://", remote_url)) {
    owner_repo <- sub("^https?://[^/]+/", "", remote_url)
  } else if (grepl("^[^/]+/[^/]+$", remote_url)) {
    owner_repo <- remote_url
  }

  if (is.null(owner_repo) || !nzchar(owner_repo)) {
    stop("Failed to parse owner/repo from remote URL: ", remote_url)
  }

  owner_repo <- gsub("\\.git$", "", owner_repo)
  owner_repo <- gsub("/+$", "", owner_repo)
  parts <- strsplit(owner_repo, "/")[[1]]
  if (length(parts) < 2 || !nzchar(parts[1]) || !nzchar(parts[2])) {
    stop("Failed to get GitHub repository owner and name from git remote URL: ", remote_url)
  }
  parts
}