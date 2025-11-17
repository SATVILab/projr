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

# github
.test_remote_host_rm_github <- function(host) {
  .assert_given_full(host)
  # set up
  # ----------
  if (!requireNamespace("gh", quietly = TRUE)) {
    .dep_install_only("gh")
  }

  # Check authentication before any GitHub API calls
  .auth_check_github("deleting GitHub repository")

  # defaults
  user <- if ("user" %in% names(host)) host[["user"]] else NULL
  .dep_install("gh")
  if (is.null(user)) {
    user <- tryCatch({
      gh::gh_whoami()[["login"]]
    }, error = function(e) {
      NULL
    })
  }
  if (!.is_string(user)) stop("No GitHub user found")
  token <- if ("token" %in% names(host)) host[["token"]] else NULL # nolint
  token <- token %||% Sys.getenv("GITHUB_PAT")
  token <- if (!nzchar(token)) Sys.getenv("GH_TOKEN") else token
  if (!.is_string(token)) stop("No GitHub token found")
  repo <- if ("repo" %in% names(host)) host[["repo"]] else NULL
  if (!.is_string(repo)) stop("No GitHub repo specified")
  # Define the URL of the GitHub API
  # take basename in case we've accidentally specified
  # the user as well in the repo specification
  repo <- basename(repo)
  if (repo == "projr") stop("Cannot delete the projr repo")

  try(
    gh::gh(
      "DELETE /repos/{username}/{pkg}",
      username = user,
      pkg = repo
    ),
    silent = TRUE
  )
}