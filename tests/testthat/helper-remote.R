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

.test_remote_host_rm_github <- function(host, token = NULL) {
  stop("Automatic GitHub repository deletion is disabled", call. = FALSE)
}
