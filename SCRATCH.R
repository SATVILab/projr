library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_set_select()
devtools::test_active_file("tests/testthat/test-git.R")

library(testthat)
# devtools::load_all(path = file.path(Sys.getenv("pkg"), "projr"))
devtools::load_all()
.test_unset_select()
.test_set_fast()
devtools::test()

# seems to be the third test stanza in test-git.R
x <- 1

remote_vec <- .projr_test_git_remote_get()
if (.is_len_0(remote_vec)) {
  # creating a repo works fine
  repo <- .projr_test_github_repo_create(
    repo = basename(path_dir), env = env
  ) |>
    basename()
  # clone it down now
  # adding remotes works
  .projr_test_github_repo_remote_add(repo = repo)
  if (length(remote_vec) == 0L) {
    stop("No remotes found")
  }
  # we must not then have upstream set:
  invisible(.projr_test_git_set_upstream_and_force_push())
} else {
  # should really check that the remote exists
}
