#' @rdname projr_content_update
#' @export
projr_restore_repo <- function(
  repo,
  path = NULL,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  destination = NULL
) {
  .assert_string(repo, TRUE)
  .assert_string(path)
  .assert_chr(label)
  remote <- .restore_remote(type, title, destination)

  result <- tryCatch(
    {
      path <- path %||% basename(repo)
      .git_clone(repo, path)
      .restore_repo_labels(path, label, remote)
    },
    error = function(e) {
      .cli_info("Error in projr_restore_repo: {e$message}")
      return(FALSE)
    }
  )
  invisible(result)
}

#' @rdname projr_content_update
#' @export
projr_restore_repo_wd <- function(
  repo,
  label = NULL,
  pos = NULL,
  type = NULL,
  title = NULL,
  destination = NULL
) {
  projr_restore_repo(
    repo,
    path = ".",
    label = label,
    pos = pos,
    type = type,
    title = title,
    destination = destination
  )
}

.restore_repo_labels <- function(path, label, remote) {
  orig_wd <- getwd()
  on.exit(setwd(orig_wd), add = TRUE)
  setwd(path)
  projr_source(label, remote, cue = "always", conflict = "error")
}
