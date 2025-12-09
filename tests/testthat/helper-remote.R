# ========================
# delete a remote host
# ========================

# this is different to deleting a remote,
# at least for GitHub as we don't in that case
# delete the release itself - we actually delete the repo.

.test_remote_host_rm <- function(type,
                            host) {
  .assert_in(type, .opt_remote_get_type(), TRUE)
  switch(type,
    "local" = .test_remote_host_rm_local(host),
    "osf" = .test_remote_host_rm_osf(host),
    "github" = .test_remote_host_rm_github(host)
  )
}

# local
.test_remote_host_rm_local <- function(host) {
  .assert_string(host, TRUE)
  if (!dir.exists(host)) {
    return(invisible(FALSE))
  }
  unlink(host, recursive = TRUE)
  invisible(TRUE)
}

# osf
.test_remote_host_rm_osf <- function(host) {
  .assert_given_full(host)
  .auth_check_osf("deleting OSF node")
  .osf_rm(
    x = .osf_retrieve_node(host), check = FALSE, recurse = TRUE
  )
  invisible(TRUE)
}

.test_remote_host_rm_github <- function(host, token = NULL) {
  .assert_given_full(host)

  if (!requireNamespace("gh", quietly = TRUE)) {
    .dep_install_only("gh")
  }

  # Ensure we have a token with delete scope
  if (is.null(token)) {
    token <- .auth_get_github_pat_find()
  }
  if (!.is_string(token)) {
    stop("No GitHub token found")
  }

  # Identify user
  user <- try(host[["user"]])
  if (inherits(user, "try-error") || is.null(user)) {
    user <- tryCatch(
      gh::gh_whoami(.token = token)[["login"]],
      error = function(e) NULL
    )
  }
  if (!.is_string(user)) stop("No GitHub user found")

  # Repo name
  repo <- host[["repo"]]
  if (!.is_string(repo)) stop("No GitHub repo specified")
  repo <- basename(repo)
  if (repo == "projr") stop("Cannot delete the projr repo")

  message("Attempting to delete ", user, "/", repo, " ...")

  res <- tryCatch(
    gh::gh(
      "DELETE /repos/{owner}/{repo}",
      owner  = user,
      repo   = repo,
      .token = token
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    message("  FAILED: ", conditionMessage(res))
    return(FALSE)
  } else {
    message("  OK")
    return(TRUE)
  }
}
